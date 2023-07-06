[@@@warning "-partial-match"]

open Base
module StdString = Stdlib.String

module DStrLit = struct
  type Expr.t += String of string
  type Type.t += TString

  let eval = function String s -> String s

  let typecheck (_, e) = match e with String _ -> TString

  (* Boring code *)
  let subst (_, _, e2) = match e2 with 
    | String s -> String s

  let type_subst (_, _, t2) = match t2 with 
    | TString -> TString

  let desugar = function String s -> String s

  let show = function String s -> Printf.sprintf "\"%s\"" s

  let type_show = function TString -> "string"

  let is_article = Open_func.noop
end
module DStrLitFrag = MakeExprFragment(DStrLit)
module DStrLitTypeFrag = MakeTypeFragment(DStrLit)


module DStrProg = struct
  open DStrLit

  type dir = Left | Right

  type Expr.t += 
    | Concat of Expr.t * Expr.t 
    | Let of var * Expr.t * Expr.t
    | Fix of var * Type.t * Expr.t
    | Var of var
    | Inject of dir * Type.t * Expr.t
    | Case of {
        expr: Expr.t;
        left: var * Expr.t;
        right: var * Expr.t
      }
    | Lambda of var * Type.t * Expr.t
    | App of Expr.t * Expr.t
    | Unit
    | Pair of Expr.t * Expr.t
    | Project of Expr.t * dir
    | Fold of Type.t * Expr.t
    | Unfold of Type.t * Expr.t
    | TyLambda of var * Expr.t
    | TyApp of Expr.t * Type.t

  type Type.ctx_elem += 
    | BoundVar of var * Type.t 
    | BoundTypeVar of var

  type Type.t +=
    | TFun of Type.t * Type.t
    | TProd of Type.t * Type.t
    | TSum of Type.t * Type.t
    | TUnit
    | TForall of var * Type.t
    | TRec of var * Type.t
    | TVar of var

  let app1 f x = App (f, x)
  let app2 f x y = App (App (f, x), y)
  let app3 f x y z = App (App (App (f, x), y), z)

  let tyapp1 f x = TyApp (f, x)
  let tyapp2 f x y = TyApp (TyApp (f, x), y)  

  let lam1 x t e = Lambda (x,t, e)
  let lam2 x t1 y t2 e = Lambda (x, t1, Lambda (y, t2, e))
  let lam3 x t1 y t2 z t3 e = Lambda (x, t1, Lambda (y, t2, Lambda (z, t3, e)))

  let tylam1 x e = TyLambda (x, e)
  let tylam2 x y e = TyLambda (x, TyLambda (y, e))

  let tya = TVar "a"
  let tyb = TVar "b"

  let eunit = Unit

  let tybool = TSum (TUnit, TUnit)
  let false_ = Inject (Right, tybool, eunit)
  let true_ = Inject (Left, tybool, eunit) 
  let if_ expr then_ else_ = Case {
      expr; left = ("_", then_); right = ("_", else_)
    }


  let tylist t = TRec ("list", TSum (TUnit, TProd (t, TVar "list")))
  let enil = TyLambda ("a", Inject (Left, tylist tya, eunit))
  let nil t = tyapp1 enil t

  let econs = tylam1 "a" (
      lam2 "x" tya "y" (tylist tya) 
        (Inject (Right, tylist tya, Pair (Var "x", Var "y"))))
  let cons a = app2 (tyapp1 econs a)

  (* fold : forall a, b. (a -> b -> a) -> b -> a list -> b*)
  let efold = 
    let fty = TFun(tya, TFun(tyb, tya)) in
    let ty = TFun(fty, TFun(tyb, TFun(tylist tya, tyb))) in
    tylam2 "a" "b" (Fix ("fold", ty, lam3 "f" fty "z" tyb "l" (tylist tya) (Case {
        expr = Var "l";
        left = ("_", Var "z");
        right = ("tup", 
                 app2 (Var "f") 
                   (Project (Var "tup", Left)) 
                   (app3 (Var "fold") 
                      (Var "f") 
                      (Var "z") 
                      (Project (Var "tup", Right))));
      })))
  let fold a b = app3 (tyapp2 (Var "fold") a b)

  let eappend = tylam1 "a" (lam2 "x" (tylist tya) "y" (tylist tya) (
      fold tya (tylist tya) (tyapp1 (Var "cons") tya)
        (Var "y") 
        (Var "x")    
    ))
  let eflatten = tylam1 "a" (lam1 "x" (tylist (tylist tya)) (
      fold tya (tylist tya) 
        (tyapp1 (Var "append") tya) 
        (nil tya) 
        (Var "x")
    ))
  let eforeach = tylam2 "a" "b" (lam2 "f" (TFun (tya, tyb)) "l" (tylist tya) (
      fold tya (tylist tyb)
        (lam2 "x" tya "xs" (tylist tyb) 
           (cons tyb (app1 (Var "f") (Var "x")) (Var "xs")))
        (nil tyb) 
        (Var "l")
    ))

  let ejoin = lam1 "l" (tylist TString) (
      fold TString TString
        (lam2 "x" TString "y" TString (Concat (Var "x", Var "y")))
        (String "") 
        (Var "l")
    )

  let prelude = [
    ("nil", enil);
    ("cons", econs);
    ("fold", efold);
    ("append", eappend);
    ("flatten", eflatten);
    ("foreach", eforeach);
    ("join", ejoin)
  ]

  let join = app1 (Var "join")
  let append a = app2 (tyapp1 (Var "append") a)
  let flatten a = app1 (tyapp1 (Var "flatten") a)
  let foreach a b = app2 (tyapp2 (Var "foreach") a b)
  let list a l = List.fold_right (cons a) l (nil a)

  let with_prelude e = List.fold_left (fun e (v, f) -> Let(v, f, e)) e prelude

  let eval e =     
    (* Printf.printf "??? %s\n" (Expr.show e); *)
    match e with
    | Concat (e1, e2) -> 
      let (String s1, String s2) = (Expr.eval e1, Expr.eval e2) in
      String (s1 ^ s2)
    | Let (x, e1, e2) -> Expr.eval(Expr.subst (x, (Expr.eval e1), e2))
    | Fix (x, t, e) -> Expr.eval (Expr.subst (x, Fix (x, t, e), e))
    | Var _ -> raise Undefined_behavior
    | Inject (dir, t, e) -> Inject (dir, t, Expr.eval e)
    | Case {expr; left = (x, e1); right = (y, e2)} -> 
      let (Inject (dir, _, e)) = Expr.eval expr in
      Expr.eval (Expr.subst ((match dir with Left -> x | Right -> y), e, (match dir with Left -> e1 | Right -> e2)))
    | Lambda (x, t, e) -> Lambda (x, t, e)
    | App (e1, e2) -> 
      let Lambda (x, _, e) = Expr.eval e1 in
      let e2' = Expr.eval e2 in
      Expr.eval (Expr.subst (x, e2', e))
    | Pair (e1, e2) -> Pair (Expr.eval e1, Expr.eval e2)
    | Project (e, d) -> 
      let Pair (v1, v2) = Expr.eval e in
      (match d with Left -> v1 | Right -> v2)
    | Fold (t, e) -> Fold (t, Expr.eval e)
    | Unfold (_, e) -> 
      let Fold (_, e) = Expr.eval e in
      e
    | TyLambda (x, e) -> TyLambda (x, e)
    | TyApp (e, _) -> 
      let TyLambda (_, e) = Expr.eval e in
      Expr.eval e    
    | Unit -> Unit

  let typecheck (ctx, e) =
    match e with 
    | Concat (e1, e2) -> 
      let (t1, t2) = (Expr.typecheck (ctx, e1), Expr.typecheck (ctx, e2)) in
      (match (t1, t2) with
       | (TString, TString) -> TString
       | _ -> raise (Type_error "concat"))
    | Let (x, e1, e2) -> 
      let t1 = Expr.typecheck (ctx, e1) in
      Expr.typecheck ((BoundVar (x, t1)) :: ctx, e2)
    | Fix (x, t, e) ->
      Expr.typecheck ((BoundVar (x, t)) :: ctx, e)
    | Var x -> 
      let ty_opt = List.find_map (fun elem -> match elem with 
          | BoundVar (y, t) -> if x = y then Some t else None
          | _ -> None) ctx in 
      (match ty_opt with
       | Some ty -> ty
       | None -> raise (Type_error "var"))
    | Inject (dir, t, e) -> 
      let t' = Expr.typecheck (ctx, e) in
      let t'' = (match (dir, t) with 
          | (Left, TSum (t'', _)) ->  t''
          | (Right, TSum (_, t'')) -> t''
          | _ -> raise (Type_error "inject")) in
      if t' = t'' then t else raise (Type_error "inject")
    | Case {expr; left = (x, e1); right = (y, e2)} ->
      let t = Expr.typecheck (ctx, expr) in
      (match t with
       | TSum (t1, t2) -> 
         let t1' = Expr.typecheck ((BoundVar (x, t1)) :: ctx, e1) in
         let t2' =  Expr.typecheck ((BoundVar (y, t2)) :: ctx, e2) in
         if t1' = t2' then t1' else raise (Type_error "case")
       | _ -> raise (Type_error "case"))
    | Lambda (x, t, e) -> 
      let t' = Expr.typecheck ((BoundVar (x, t)) :: ctx, e) in
      TFun (t, t')
    | App (e1, e2) -> 
      let t1 = Expr.typecheck (ctx, e1) in 
      let t2=  Expr.typecheck (ctx, e2) in
      (match t1 with
       | TFun (t1', t1'') -> if t1' = t2 then t1'' else raise (Type_error "app")
       | _ -> raise (Type_error "app"))
    | Pair (e1, e2) -> 
      let t1 = Expr.typecheck (ctx, e1) in
      let t2 = Expr.typecheck (ctx, e2) in 
      TProd (t1, t2)
    | Project (e, d) ->
      let t = Expr.typecheck (ctx, e) in
      (match t with
       | TProd (t1, t2) -> (match d with Left -> t1 | Right -> t2)
       | _ -> raise (Type_error "project"))
    | Fold (t, e) ->
      (* TODO *)
      let _ = Expr.typecheck (ctx, e) in t
    | Unfold (t, e) ->
      let _ = Expr.typecheck (ctx, e) in t
    | TyLambda (x, e) -> 
      let t = Expr.typecheck ((BoundTypeVar x) :: ctx, e) in
      TForall (x, t)
    | TyApp (e, t) ->
      let t' = Expr.typecheck (ctx, e) in
      (match t' with
       | TForall (x, t'') -> Type.subst (x, t, t'')
       | _ -> raise (Type_error "tyapp"))
    | Unit -> TUnit    


  (* Boring code *)

  (* ignore alpha-renaming for now *)
  let subst (x, e1, e2) = match e2 with  
    | Concat (e1', e2') -> Concat (Expr.subst (x, e1, e1'), Expr.subst (x, e1, e2'))
    | Let (y, e1', e2') ->
      Let (y, Expr.subst (x, e1, e1'), if x = y then e2' else Expr.subst (x, e1, e2'))
    | Fix (y, t, e') -> Fix (y, t, if x = y then e' else Expr.subst (x, e1, e'))
    | Var y -> if x = y then e1 else Var y
    | Inject (dir, t, e') -> Inject (dir, t, Expr.subst (x, e1, e'))
    | Case {expr; left = (y, e1'); right = (z, e2')} ->
      Case {
        expr = Expr.subst (x, e1, expr);
        left = (y, if x = y then e1' else Expr.subst (x, e1, e1'));
        right = (z, if x = z then e2' else Expr.subst (x, e1, e2'))
      }
    | Lambda (y, t, e') -> Lambda (y, t, if x = y then e' else Expr.subst (x, e1, e'))
    | App (e1', e2') -> App (Expr.subst (x, e1, e1'), Expr.subst (x, e1, e2'))
    | Pair (e1', e2') -> Pair (Expr.subst (x, e1, e1'), Expr.subst (x, e1, e2'))
    | Project (e', d) -> Project (Expr.subst (x, e1, e'), d)
    | Fold (t, e') -> Fold (t, Expr.subst (x, e1, e'))
    | Unfold (t, e') -> Unfold (t, Expr.subst (x, e1, e'))
    | TyLambda (y, e') -> TyLambda (y, if x = y then e' else Expr.subst (x, e1, e'))
    | TyApp (e', t) -> TyApp (Expr.subst (x, e1, e'), t)
    | Unit -> Unit

  let type_subst (x, t1, t2) = match t2 with
    | TFun (t1', t1'') -> TFun (Type.subst (x, t1, t1'), Type.subst (x, t1, t1''))
    | TProd (t1', t1'') -> TProd (Type.subst (x, t1, t1'), Type.subst (x, t1, t1''))
    | TSum (t1', t1'') -> TSum (Type.subst (x, t1, t1'), Type.subst (x, t1, t1''))
    | TUnit -> TUnit
    | TForall (y, t) -> TForall (y, if x = y then t else Type.subst (x, t1, t))
    | TRec (y, t) -> TRec (y, if x = y then t else Type.subst (x, t1, t))
    | TVar y -> if x = y then t1 else TVar y

  let desugar = function
    | Concat (e1, e2) -> Concat(Expr.desugar e1, Expr.desugar e2)
    | Let (x, e1, e2) -> Let(x, Expr.desugar e1, Expr.desugar e2)
    | Fix (x, t, e) -> Fix(x, t, Expr.desugar e)
    | Var x -> Var x
    | Inject (dir, t, e) -> Inject (dir, t, Expr.desugar e)
    | Case {expr; left = (x, e1); right = (y, e2)} -> 
      Case {
        expr = Expr.desugar expr;
        left = (x, Expr.desugar e1);
        right = (y, Expr.desugar e2)
      }
    | Lambda (x, t, e) -> Lambda (x, t, Expr.desugar e)
    | App (e1, e2) -> App (Expr.desugar e1, Expr.desugar e2)
    | Pair (e1, e2) -> Pair (Expr.desugar e1, Expr.desugar e2)
    | Project (e, n) -> Project (Expr.desugar e, n)
    | Fold (t, e) -> Fold (t, Expr.desugar e)
    | Unfold (t, e) -> Unfold (t, Expr.desugar e)
    | TyLambda (x, e) -> TyLambda (x, Expr.desugar e)
    | TyApp (e, t) -> TyApp (Expr.desugar e, t)
    | Unit -> Unit

  let show_dir = function Left -> "l" | Right -> "r"

  let show = function
    | Concat (e1, e2) -> Printf.sprintf "%s + %s" (Expr.show e1) (Expr.show e2)
    | Let (x, e1, e2) -> Printf.sprintf "let %s = %s in\n%s" x (Expr.show e1) (Expr.show e2)
    | Fix (x, t, e) -> Printf.sprintf "fix (%s : %s). %s" x (Type.show t) (Expr.show e)
    | Var x -> x
    | Inject (d, t, e) -> 
      (match t with
       | TRec ("list", _) -> 
         (match d with
          | Left -> "[]"
          | Right -> 
            let Pair (e1, e2) = e in
            Printf.sprintf "%s :: %s" (Expr.show e1) (Expr.show e2))
       | TRec ("node", _) ->
         (match d with
          | Left -> Printf.sprintf "<text>%s</text>" (Expr.show e)
          | Right -> 
            let Pair(nt, children) = e in
            Printf.sprintf "<%s>%s</%s>" (Expr.show nt) (Expr.show children) (Expr.show nt))
       | _ -> Printf.sprintf "in%s(%s as %s)" (show_dir d) (Expr.show e) (Type.show t))
    | Case {expr; left = (x, e1); right = (y, e2)} -> 
      Printf.sprintf "case %s of inl(%s) -> %s | inr(%s) -> %s" (Expr.show expr) x (Expr.show e1) y (Expr.show e2)
    | Lambda (x, t, e) -> Printf.sprintf " λ(%s:%s). %s" x (Type.show t) (Expr.show e)
    | App (e1, e2) -> Printf.sprintf "%s %s" (Expr.show e1) (Expr.show e2)
    | Pair (e1, e2) -> Printf.sprintf "(%s, %s)" (Expr.show e1) (Expr.show e2)
    | Project (e, d) -> Printf.sprintf "%s.%s" (Expr.show e) (show_dir d)
    | Fold (t, e) -> Printf.sprintf "fold %s %s" (Type.show t) (Expr.show e)
    | Unfold (t, e) -> Printf.sprintf "unfold %s %s" (Type.show t) (Expr.show e)
    | TyLambda (x, e) -> Printf.sprintf "Λ%s. %s" x (Expr.show e)
    | TyApp (e, t) -> Printf.sprintf "%s[%s]" (Expr.show e) (Type.show t)
    | Unit -> "()"

  let type_show = function
    | TFun (t1, t2) -> Printf.sprintf "%s -> %s" (Type.show t1) (Type.show t2)
    | TProd (t1, t2) -> Printf.sprintf "%s * %s" (Type.show t1) (Type.show t2)
    | TSum (t1, t2) -> Printf.sprintf "%s + %s" (Type.show t1) (Type.show t2)
    | TUnit -> "unit"
    | TForall (x, t) -> Printf.sprintf "∀%s. %s" x (Type.show t)
    | TRec (x, t) -> Printf.sprintf "μ%s. %s" x (Type.show t)
    | TVar x -> x

  let is_article = Open_func.noop
end
module DStrProgFrag = MakeExprFragment(DStrProg)
module DStrProgTypeFrag = MakeTypeFragment(DStrProg)

module DStrTLit = struct
  open DStrProg
  open DStrLit
  open Template

  type Template.t += TStr of string | TExpr of Expr.t
  type Expr.t += StrTmpl of ttext

  let eval = function StrTmpl _ -> raise Not_desugared    

  let typecheck (_, e) = match e with StrTmpl _ -> raise Not_desugared

  let ty_tpl = TVar "tpl"

  let desugar_template_elems = function
    | [] -> nil ty_tpl
    | lt :: lts -> Template.desugar_in_context (lt, lts)

  let desugar_template_in_context (lt, lts) = 
    cons ty_tpl (Template.desugar lt) (desugar_template_elems lts)

  let desugar = function StrTmpl kt -> TyApp (TyLambda ("tpl", join (desugar_template_elems kt)), TString)

  let desugar_template = function
    | TStr s -> String s
    | TExpr e -> Expr.desugar e    

  (* Boring code *)

  let subst (_, _, e2) = match e2 with StrTmpl _ -> raise Not_desugared    

  let show_ttext kt = (StdString.concat "" (List.map Template.show kt))
  let show = function StrTmpl kt -> Printf.sprintf "`%s`" (show_ttext kt)

  let show_template = function
    | TStr s -> s
    | TExpr e -> Printf.sprintf "{%s}" (Expr.show e)    

  let is_article = Open_func.noop
end
module DStrTLitFrag = MakeExprFragment(DStrTLit)
module DStrTLitInlineFrag = MakeTemplateFragment(DStrTLit)

module DStrTProg = struct
  open DStrProg
  open DStrTLit
  open Template

  type Template.t +=
    | TSet of var * Expr.t
    | TForeach of Expr.t * var * Type.t * ttext   
    | TIf of Expr.t * ttext * ttext
    | TSplice of Expr.t 

  let desugar_template_in_context (lt, lts) = match lt with 
    | TSet (x, e) -> Let (x, e, desugar_template_elems lts)
    | TSplice e -> append ty_tpl (Expr.desugar e) (desugar_template_elems lts)
    | TForeach (e, x, t, kt) -> 
      let lt' = TSplice (flatten ty_tpl (foreach t ty_tpl (lam1 x t (desugar_template_elems kt)) (Expr.desugar e))) in
      Template.desugar_in_context (lt', lts)
    | TIf (e, kt1, kt2) ->
      let lt' = TSplice (if_ (Expr.desugar e) (desugar_template_elems kt1) (desugar_template_elems kt2)) in
      Template.desugar_in_context (lt', lts)

  (* Boring code *)

  let show_template = function
    | TForeach (e1, x, t, kt) -> Printf.sprintf "{{ foreach (%s : %s) in %s }} %s {{ endforeach }}" x (Type.show t) (Expr.show e1) (show_ttext kt)
    | TSet (x, e) -> Printf.sprintf "{{ %s = %s }}" x (Expr.show e)
    | TSplice e -> Printf.sprintf ",@%s" (Expr.show e)

  let desugar_template = Open_func.noop
  let is_article = Open_func.noop
end
module DStrTProgInlineFrag = MakeTemplateFragment(DStrTProg)