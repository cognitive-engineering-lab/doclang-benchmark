open Base

module DStrLit = struct
  type Expr.t += String of string

  let eval e = match e with 
    | String s -> Some (String s)
    | _ -> None

  (* Boring code *)
  let subst (_, _, e2) = match e2 with 
    | String s -> Some (String s)
    | _ -> None

  let desugar e = match e with 
    | String s -> Some (String s)
    | _ -> None

  let show e = match e with
    | String s -> Some (Printf.sprintf "\"%s\"" s)
    | _ -> None
end
module DStrLitFrag = MakeExprFragment(DStrLit)

module DStrProg = struct
  open DStrLit

  type Expr.t += 
    | Concat of Expr.t * Expr.t 
    | Let of var * Expr.t * Expr.t 
    | Var of var  

  let[@warning "-partial-match"] eval e = match e with
    | Concat (e1, e2) -> 
      let (String s1, String s2) = (Expr.eval e1, Expr.eval e2) in
      Some (String (s1 ^ s2))
    | Let (x, e1, e2) -> Some(Expr.eval(Expr.subst (x, (Expr.eval e1), e2)))
    | Var _ -> raise Undefined_behavior
    | _ -> None

  (* Boring code *)

  (* ignore alpha-renaming for now *)
  let subst (x, e1, e2) = match e2 with  
    | Concat (e1', e2') -> Some(Concat (Expr.subst (x, e1, e1'), Expr.subst (x, e1, e2')))
    | Let (y, e1', e2') ->
      Some(Let (y, Expr.subst (x, e1, e1'), if x = y then e2' else Expr.subst (x, e1, e2')))
    | Var y -> Some(if x = y then e1 else Var y)
    | _ -> None

  let desugar e = match e with
    | Concat (e1, e2) -> Some(Concat(Expr.desugar e1, Expr.desugar e2))
    | Let (x, e1, e2) -> Some(Let(x, Expr.desugar e1, Expr.desugar e2))
    | Var x -> Some(Var x)
    | _ -> None

  let show e = match e with 
    | Concat (e1, e2) -> Some(Printf.sprintf "%s + %s" (Expr.show e1) (Expr.show e2))
    | Let (x, e1, e2) -> Some(Printf.sprintf "let %s = %s in %s" x (Expr.show e1) (Expr.show e2))
    | Var x -> Some(x)
    | _ -> None
end
module DStrProgFrag = MakeExprFragment(DStrProg)

module DStrTLit = struct
  open DStrLit
  open Tinline

  type Tinline.t += TStr of string | TInlineExpr of Expr.t

  type Expr.t += 
    | List of Expr.t list
    | Join of Expr.t
    | Flatten of Expr.t
    | StrTmpl of ttext

  (* TODO: are we happy with this "recursive flatten" semantics? *)
  let rec flatten e = match e with
    | List es -> List.fold_left (@) [] (List.map flatten es)
    | _ -> [e]

  let[@warning "-partial-match"] eval e = match e with 
    | List es -> Some(List (List.map Expr.eval es))
    | Flatten e -> Some(List (flatten (Expr.eval e)))
    | Join e -> 
      let List l = Expr.eval e  in
      let s = String.concat "" (List.map (fun e -> match e with 
          | String s -> s 
          | _ -> raise Undefined_behavior) l) in
      Some (String s)      
    | StrTmpl _ -> raise Not_desugared
    | _ -> None

  let rec desugar e = match e with 
    | List es -> Some(List (List.map Expr.desugar es))
    | Join e -> Some(Join (Expr.desugar e))
    | Flatten e -> Some(Flatten (Expr.desugar e))
    | StrTmpl kt -> Some(Join(desugar_ttext kt))
    | _ -> None
  and desugar_ttext kt = Flatten(desugar_ttext_rec kt)
  and desugar_ttext_rec kt = match kt with 
    | [] -> List []
    | lt :: lts -> Tinline.desugar_ttext_elem (lt, lts)

  let[@warning "-partial-match"] desugar_ttext_elem (lt, lts) = 
    let List l = desugar_ttext_rec lts in 
    Some(List ((Tinline.desugar lt) :: l))

  let desugar_tinline lt = match lt with
    | TStr s -> Some(String s)
    | TInlineExpr e -> Some(Expr.desugar e)
    | _ -> None


  (* Boring code *)

  let subst (x, e1, e2) = match e2 with 
    | List es -> Some(List (List.map (fun e2' -> Expr.subst (x, e1, e2')) es))
    | Join e' -> Some(Join (Expr.subst (x, e1, e')))
    | Flatten e' -> Some(Flatten (Expr.subst (x, e1, e')))
    | StrTmpl _ -> raise Not_desugared
    | _ -> None

  let rec show e = match e with
    | List es -> Some(Printf.sprintf "[%s]" (String.concat ", " (List.map Expr.show es)))
    | Join e -> Some(Printf.sprintf "join(%s)" (Expr.show e))
    | Flatten e -> Some(Printf.sprintf "flatten(%s)" (Expr.show e))
    | StrTmpl kt -> Some(Printf.sprintf "`%s`" (show_ttext kt))
    | _ -> None
  and show_ttext kt = (String.concat "" (List.map Tinline.show kt))

  let show_tinline lt = match lt with
    | TStr s -> Some(s)
    | TInlineExpr e -> Some(Printf.sprintf "{%s}" (Expr.show e))
    | _ -> None
end
module DStrTLitFrag = MakeExprFragment(DStrTLit)
module DStrTLitInlineFrag = MakeTinlineFragment(DStrTLit)

module DStrTProg = struct
  open DStrProg
  open DStrTLit
  open Tinline

  type Expr.t +=
    | Foreach of Expr.t * var * Expr.t
  type Tinline.t +=
    | TInlineSet of var * Expr.t
    | TInlineForeach of Expr.t * var * ttext

  let[@warning "-partial-match"] eval e = match e with
    | Foreach (e1, x, e2) ->
      let List l = Expr.eval e1 in
      Some(List (List.map (fun e -> Expr.eval (Expr.subst(x, e, e2))) l))
    | _ -> None

  let desugar_ttext_elem (lt, lts) = match lt with 
    | TInlineSet (x, e) -> Some(Let (x, e, desugar_ttext_rec lts))
    | _ -> None

  let desugar_tinline lt = match lt with
    | TInlineForeach (e1, x, kt) -> Some(Foreach (Expr.desugar e1, x, desugar_ttext kt))
    | _ -> None

  (* Boring code *)

  let subst (x, e1, e2) = match e2 with
    | Foreach (e1', y, e2') ->
      Some(Foreach (Expr.subst (x, e1, e1'), y, if x = y then e2' else Expr.subst (x, e1, e2')))
    | _ -> None

  let desugar e = match e with
    | Foreach (e1, x, e2) -> Some(Foreach (Expr.desugar e1, x, Expr.desugar e2))
    | _ -> None

  let show e = match e with
    | Foreach (e1, x, e2) -> Some(Printf.sprintf "foreach %s in %s %s" x (Expr.show e1) (Expr.show e2))
    | _ -> None

  let show_tinline lt = match lt with
    | TInlineForeach (e1, x, kt) -> Some(Printf.sprintf "{{ foreach %s in %s }} %s {{ endforeach }}" x (Expr.show e1) (show_ttext kt))
    | TInlineSet (x, e) -> Some(Printf.sprintf "{{ %s = %s }}" x (Expr.show e))
    | _ -> None  
end
module DStrTProgFrag = MakeExprFragment(DStrTProg)
module DStrTProgInlineFrag = MakeTinlineFragment(DStrTProg)
