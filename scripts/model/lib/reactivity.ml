exception Undefined_behavior
exception Invalid_structure

module DReactVar = struct
  open Trees.TreeDoc

  type var = string  
  [@@deriving show]

  type expr = 
    | EString of string
    | EBold of expr
    | EPara of expr
    | ESection of expr * expr    
    | List_ of expr list
    | Concat of expr * expr
    | Let of var * expr * expr
    | Var of var
  [@@deriving show]

  let rec subst x e1 e2 = match e2 with
    | EString s -> EString s
    | EBold e -> EBold (subst x e1 e)
    | EPara e -> EPara (subst x e1 e)
    | ESection (e1', e2') -> ESection (subst x e1 e1', subst x e1 e2')
    | List_ es -> List_ (List.map (subst x e1) es)
    | Concat (e1', e2') -> Concat (subst x e1 e1', subst x e1 e2')
    | Var y -> if x = y then e1 else e2
    | Let (y, e1', e2') -> 
      Let (y, subst x e1 e1', if x = y then e2' else subst x e1 e2')

  let[@warning "-partial-match"] rec eval (e : expr) : expr = match e with 
    | EString s -> EString s
    | EBold e -> EBold (eval e)
    | EPara e -> EPara (eval e)
    | ESection (e1, e2) -> ESection(eval e1, eval e2)
    | List_ es -> List_ (List.map eval es)
    | Concat (e1, e2) ->
      let (EString s1, EString s2) = (eval e1, eval e2) in 
      EString (s1 ^ s2)
    | Let (x, e1, e2) -> eval (subst x e1 e2)
    | Var _ -> raise Undefined_behavior    

  let rec to_doc (e : expr) : doc = match e with
    | List_ es -> List.map to_block es
    | _ -> raise Invalid_structure
  and to_block (e: expr) : block = match e with
    | EPara e -> Para (to_text e)
    | ESection (e1, e2) -> Section (to_text e1, to_doc e2)
    | _ -> raise Invalid_structure
  and to_text (e : expr) : text = match e with
    | List_ es -> List.map to_inline es
    | _ -> raise Invalid_structure
  and to_inline (e : expr) : inline = match e with
    | EString s -> String s
    | EBold e -> Bold (to_text e)
    | _ -> raise Invalid_structure

  type event = expr
  type state = expr

  let[@warning "-partial-match"] step (e : expr) (s : state) (ev : event) : doc * state = 
    let e' = Let ("state", s, Let ("event", ev, e)) in
    let List_ [d; s] = eval e' in   
    (to_doc d, s)
end