type var = string

exception Undefined_behavior
exception Unreachable

module DStrId = struct
  type doc = string
  type expr = string
  let eval s = s
end

module DStrCat = struct
  type doc = string  
  type expr =
    | String of string
    | Concat of expr * expr

  let rec eval e = match e with
    | String s -> s
    | Concat (e1, e2) -> (eval e1) ^ (eval e2)
end

module DStrVar = struct
  type doc = string

  type var = string
  type expr = 
    | String of string
    | Var of var
    | Concat of expr * expr
    | Let of var * expr * expr

  let rec subst x e1 e2 = match e2 with
    | String _ -> e2
    | Var y -> if x = y then e1 else e2
    | Concat (e1', e2') -> Concat (subst x e1 e1', subst x e1 e2')
    | Let (y, e1', e2') -> if x = y then e2 else Let (y, subst x e1 e1', subst x e1 e2')

  let rec eval e = match e with
    | String s -> s
    | Var _ -> raise Undefined_behavior
    | Concat (e1, e2) -> (eval e1) ^ (eval e2)
    | Let (x, e1, e2) -> eval (subst x (String (eval e1)) e2)
end

module DStrTmpl1 = struct 
  type doc = string

  type var = string

  type expr = 
    | String of string
    | Var of var
    | Concat of expr * expr
    | Let of var * expr * expr
    | Template of tmpl
  and tpart = 
    | TString of string 
    | TExpr of expr
  and tmpl = tpart list

  let rec desugar_expr e = match e with 
    | String _ | Var _ -> e
    | Concat (e1, e2) -> Concat(desugar_expr e1, desugar_expr e2)
    | Let(x, e1, e2) -> Let(x, desugar_expr e1, desugar_expr e2)
    | Template t -> desugar_tmpl t
  and desugar_tmpl t = List.fold_left (fun e1 e2 -> Concat(e1, e2)) (String "") (List.map desugar_part t)
  and desugar_part p = match p with 
    | TString s -> desugar_expr (String s)
    | TExpr e -> desugar_expr e


  let rec subst x e1 e2 = match e2 with
    | String _ -> e2
    | Var y -> if x = y then e1 else e2
    | Concat (e1', e2') -> Concat (subst x e1 e1', subst x e1 e2')
    | Let (y, e1', e2') -> if x = y then e2 else Let (y, subst x e1 e1', subst x e1 e2')
    | Template _ -> raise Unreachable

  let rec eval e = match e with
    | String s -> s
    | Var _ -> raise Undefined_behavior
    | Concat (e1, e2) -> (eval e1) ^ (eval e2)
    | Let (x, e1, e2) -> eval (subst x (String (eval e1)) e2)
    | Template _ -> raise Unreachable
end

module DStrTmpl2 = struct 
  type doc = string

  type var = string

  type expr = 
    | String of string
    | Var of var
    | Concat of expr * expr
    | Let of var * expr * expr
    | Template of tmpl
  and tpart = 
    | TString of string 
    | TExpr of expr
    | TLet of var * expr
  and tmpl = tpart list

  let rec desugar_expr e = match e with 
    | String _ | Var _ -> e
    | Concat (e1, e2) -> Concat(desugar_expr e1, desugar_expr e2)
    | Let(x, e1, e2) -> Let(x, desugar_expr e1, desugar_expr e2)
    | Template t -> desugar_tmpl t
  and desugar_tmpl t = match t with 
    | [] -> String ""
    | p :: ps -> (match p with
        | TLet (x, e) -> Let(x, e, desugar_tmpl ps)
        | _ -> Concat(desugar_part p, desugar_tmpl ps)
      )
  and desugar_part p = match p with 
    | TString s -> desugar_expr (String s)
    | TExpr e -> desugar_expr e
    | TLet _ -> raise Unreachable

  let rec subst x e1 e2 = match e2 with
    | String _ -> e2
    | Var y -> if x = y then e1 else e2
    | Concat (e1', e2') -> Concat (subst x e1 e1', subst x e1 e2')
    | Let (y, e1', e2') -> if x = y then e2 else Let (y, subst x e1 e1', subst x e1 e2')
    | Template _ -> raise Unreachable

  let rec eval e = match e with
    | String s -> s
    | Var _ -> raise Undefined_behavior
    | Concat (e1, e2) -> (eval e1) ^ (eval e2)
    | Let (x, e1, e2) -> eval (subst x (String (eval e1)) e2)
    | Template _ -> raise Unreachable
end

module DStrTmpl3 = struct 
  type doc = string

  type var = string

  type expr = 
    | String of string
    | Var of var
    | Concat of expr * expr
    | Let of var * expr * expr
    | Template of tmpl
    | List_ of expr list
    | Foreach of expr * var * expr
  and tpart = 
    | TString of string 
    | TExpr of expr
    | TLet of var * expr
    | TForeach of expr * var * tmpl
  and tmpl = tpart list

  let rec desugar_expr e = match e with 
    | String _ | Var _ -> e
    | Concat (e1, e2) -> Concat(desugar_expr e1, desugar_expr e2)
    | Let(x, e1, e2) -> Let(x, desugar_expr e1, desugar_expr e2)
    | Template t -> desugar_tmpl t
    | List_ es -> List_ (List.map desugar_expr es)
    | Foreach (e1, x, e2) -> Foreach (desugar_expr e1, x, desugar_expr e2)
  and desugar_tmpl t = match t with 
    | [] -> String ""
    | p :: ps -> (match p with
        | TLet (x, e) -> Let(x, e, desugar_tmpl ps)
        | _ -> Concat(desugar_part p, desugar_tmpl ps)
      )
  and desugar_part p = match p with 
    | TString s -> desugar_expr (String s)
    | TExpr e -> desugar_expr e
    | TForeach (e1, x, t2)  -> Foreach (desugar_expr e1, x, desugar_tmpl t2)
    | TLet _ -> raise Unreachable

  let rec subst x e1 e2 = match e2 with
    | String _ -> e2
    | Var y -> if x = y then e1 else e2
    | Concat (e1', e2') -> Concat (subst x e1 e1', subst x e1 e2')
    | Let (y, e1', e2') -> if x = y then e2 else Let (y, subst x e1 e1', subst x e1 e2')
    | List_ es -> List_ (List.map (subst x e1) es)
    | Foreach (e1', y, e2') -> Foreach (subst x e1 e1', x, if x = y then e2' else subst x e1 e2')
    | Template _ -> raise Unreachable

  let[@warning "-partial-match"] rec eval e = match e with
    | String s -> String s
    | Var _ -> raise Undefined_behavior
    | Concat (e1, e2) -> 
      let (String s1, String s2) = (eval e1, eval e2) in
      String (s1 ^ s2)
    | Let (x, e1, e2) -> eval (subst x (eval e1) e2)
    | List_ es -> List_ (List.map eval es)
    | Foreach (e1, x, e2) -> 
      let List_ es = eval e1 in 
      String (List.map (fun e -> eval (subst x e e2)) es
      |> List.map (fun (String s) -> s)
      |> List.fold_left (^) "")
    | Template _ -> raise Unreachable  
end 

