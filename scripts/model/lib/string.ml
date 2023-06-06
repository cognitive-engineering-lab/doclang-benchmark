[@@@warning "-partial-match"]

open Base
module StdString = Stdlib.String

module DStrLit = struct
  type Expr.t += String of string

  let eval = function String s -> String s

  (* Boring code *)
  let subst (_, _, e2) = match e2 with 
    | String s -> String s

  let desugar = function String s -> String s

  let show = function String s -> Printf.sprintf "\"%s\"" s

  let eval_list = Open_func.noop
  let is_article = Open_func.noop
end
module DStrLitFrag = MakeExprFragment(DStrLit)

module DStrProg = struct
  open DStrLit

  type Expr.t += 
    | Concat of Expr.t * Expr.t 
    | Let of var * Expr.t * Expr.t 
    | Var of var  

  let eval = function
    | Concat (e1, e2) -> 
      let (String s1, String s2) = (Expr.eval e1, Expr.eval e2) in
      String (s1 ^ s2)
    | Let (x, e1, e2) -> Expr.eval(Expr.subst (x, (Expr.eval e1), e2))
    | Var _ -> raise Undefined_behavior

  (* Boring code *)

  (* ignore alpha-renaming for now *)
  let subst (x, e1, e2) = match e2 with  
    | Concat (e1', e2') -> Concat (Expr.subst (x, e1, e1'), Expr.subst (x, e1, e2'))
    | Let (y, e1', e2') ->
      Let (y, Expr.subst (x, e1, e1'), if x = y then e2' else Expr.subst (x, e1, e2'))
    | Var y -> if x = y then e1 else Var y

  let desugar = function
    | Concat (e1, e2) -> Concat(Expr.desugar e1, Expr.desugar e2)
    | Let (x, e1, e2) -> Let(x, Expr.desugar e1, Expr.desugar e2)
    | Var x -> Var x

  let show = function
    | Concat (e1, e2) -> Printf.sprintf "%s + %s" (Expr.show e1) (Expr.show e2)
    | Let (x, e1, e2) -> Printf.sprintf "let %s = %s in %s" x (Expr.show e1) (Expr.show e2)
    | Var x -> x

  let eval_list = Open_func.noop
  let is_article = Open_func.noop
end
module DStrProgFrag = MakeExprFragment(DStrProg)

module DStrTLit = struct
  open DStrLit
  open Template

  type Template.t += TStr of string | TExpr of Expr.t

  type Expr.t += 
    | List of Expr.t list
    | Cons of Expr.t * Expr.t
    | Join of Expr.t
    | StrTmpl of ttext

  let rec eval = function
    | List l -> eval_list_rec l
    | Cons (e1, e2) ->
      let List xs = Expr.eval e2 in
      Expr.eval (List (e1 :: xs))
    | Join e -> 
      let List l = Expr.eval e  in      
      let s = StdString.concat "" (List.map (fun e -> match e with 
          | String s -> s 
          | _ -> raise Undefined_behavior) l) in
      String s
    | StrTmpl _ -> raise Not_desugared    
  and eval_list_rec = function
    | [] -> List []
    | e :: es -> Expr.eval_list (e, es)

  let eval_list (e, es) = 
    let List l = eval_list_rec es in
    List ((Expr.eval e) :: l)

  let rec desugar = function
    | List es -> List (List.map Expr.desugar es)
    | Cons (e1, e2) -> Cons (Expr.desugar e1, Expr.desugar e2)
    | Join e -> Join (Expr.desugar e)
    | StrTmpl kt -> Join(desugar_template_elems kt)    
  and desugar_template_elems = function
    | [] -> List []
    | lt :: lts -> Template.desugar_in_context (lt, lts)

  let desugar_template_in_context (lt, lts) = 
    Cons(Template.desugar lt, desugar_template_elems lts)

  let desugar_template = function
    | TStr s -> String s
    | TExpr e -> Expr.desugar e    

  (* Boring code *)

  let subst (x, e1, e2) = match e2 with 
    | List es -> List (List.map (fun e2' -> Expr.subst (x, e1, e2')) es)
    | Cons (e1', e2') -> Cons (Expr.subst (x, e1, e1'), Expr.subst (x, e1, e2'))
    | Join e' -> Join (Expr.subst (x, e1, e'))
    | StrTmpl _ -> raise Not_desugared    

  let rec show = function
    | List es -> Printf.sprintf "[%s]" (StdString.concat ", " (List.map Expr.show es))
    | Cons (e1, e2) -> Printf.sprintf "%s :: %s" (Expr.show e1) (Expr.show e2)
    | Join e -> Printf.sprintf "join(%s)" (Expr.show e)
    | StrTmpl kt -> Printf.sprintf "`%s`" (show_ttext kt)
  and show_ttext kt = (StdString.concat "" (List.map Template.show kt))

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

  type Expr.t +=
    | Foreach of Expr.t * var * Expr.t
    | Splice of Expr.t
  type Template.t +=
    | TSet of var * Expr.t
    | TForeach of Expr.t * var * ttext    

  let eval = function
    | Foreach (e1, x, e2) ->
      let List l = Expr.eval e1 in
      Expr.eval (List (List.map (fun e -> (Expr.subst(x, e, e2))) l))
    | Splice _ -> raise Undefined_behavior

  let eval_list (e, es) = match e with
    | Splice e' -> 
      let List l1 = Expr.eval e' in
      let List l2 = eval_list_rec es in
      List (l1 @ l2)

  let desugar_template_in_context (lt, lts) = match lt with 
    | TSet (x, e) -> Let (x, e, desugar_template_elems lts)

  let desugar_template = function
    | TForeach (e1, x, kt) -> Splice (Foreach (Expr.desugar e1, x, Splice (desugar_template_elems kt)))

  (* Boring code *)

  let subst (x, e1, e2) = match e2 with
    | Foreach (e1', y, e2') ->
      Foreach (Expr.subst (x, e1, e1'), y, if x = y then e2' else Expr.subst (x, e1, e2'))    
    | Splice e -> Splice (Expr.subst (x, e1, e))

  let desugar = function
    | Foreach (e1, x, e2) -> Foreach (Expr.desugar e1, x, Expr.desugar e2)
    | Splice e -> Splice (Expr.desugar e)

  let show = function
    | Foreach (e1, x, e2) -> Printf.sprintf "foreach %s in %s %s" x (Expr.show e1) (Expr.show e2)
    | Splice e -> Printf.sprintf ",@(%s)" (Expr.show e)

  let show_template = function
    | TForeach (e1, x, kt) -> Printf.sprintf "{{ foreach %s in %s }} %s {{ endforeach }}" x (Expr.show e1) (show_ttext kt)
    | TSet (x, e) -> Printf.sprintf "{{ %s = %s }}" x (Expr.show e)

  let is_article = Open_func.noop
end
module DStrTProgFrag = MakeExprFragment(DStrTProg)
module DStrTProgInlineFrag = MakeTemplateFragment(DStrTProg)
