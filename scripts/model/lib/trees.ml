exception Undefined_behavior
exception Invalid_structure
exception Unreachable

module TreeDoc = struct
  type doc = block list  
  and block = Para of text | Section of text * doc
  and text = inline list 
  and inline = String of string | Bold of text
  [@@deriving show]
end

module DTreeId = struct
  open TreeDoc

  type expr = doc

  let eval d = d
end

module AnchoredTreeDoc = struct
  type anchor = string
  type doc = block list  
  and block = Para of text | Section of anchor option * text * doc
  and text = inline list 
  and inline = String of string | Bold of text | Link of anchor * text  
end

module DAnchoredTreeId = struct
  open AnchoredTreeDoc

  type expr = doc

  let eval d = d

  let rec find_anchors (d : doc) : anchor list = List.map find_anchors_block d |> List.fold_left (@) []
  and find_anchors_block (b : block) : anchor list = match b with
    | Para _ -> []
    | Section (a, _, d) ->
      let anchors = match a with Some a -> [a] | None -> [] in
      anchors @ (find_anchors d)

  let wf (d : doc) : bool = 
    let anchors = find_anchors d in
    let rec wf_doc d = List.for_all wf_block d
    and wf_block b = match b with
      | Para t -> wf_text t 
      | Section (_, t, d) -> (wf_text t) && (wf_doc d)
    and wf_text t = List.for_all wf_inline t
    and wf_inline l = match l with
      | String _ -> true
      | Bold t -> wf_text t
      | Link (a, t) -> (List.exists (fun a2 -> a = a2) anchors) && (wf_text t)
    in wf_doc d
end

module DTreeVar = struct
  open TreeDoc

  type var = string  

  type expr = 
    | EString of string
    | EBold of expr
    | EPara of expr
    | ESection of expr * expr    
    | List_ of expr list
    | Let of var * expr * expr
    | Var of var

  let rec subst x e1 e2 = match e2 with
    | EString s -> EString s
    | EBold e -> EBold (subst x e1 e)
    | EPara e -> EPara (subst x e1 e)
    | ESection (e1', e2') -> ESection (subst x e1 e1', subst x e1 e2')
    | List_ es -> List_ (List.map (subst x e1) es)
    | Var y -> if x = y then e1 else e2
    | Let (y, e1', e2') -> 
      Let (y, subst x e1 e1', if x = y then e2' else subst x e1 e2')

  let rec eval (e : expr) : expr = match e with 
    | EString s -> EString s
    | EBold e -> EBold (eval e)
    | EPara e -> EPara (eval e)
    | ESection (e1, e2) -> ESection(eval e1, eval e2)
    | List_ es -> List_ (List.map eval es)
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
end

module DTreeTmpl = struct
  open TreeDoc

  type var = string
  [@@deriving show]

  type expr = 
    | EString of string
    | EBold of expr
    | EPara of expr
    | ESection of expr * expr    
    | List_ of expr list
    | Cons of expr * expr
    | Let of var * expr * expr
    | Var of var
    | Template of tdoc
  and tdoc = tblock list
  and tblock = 
    | TPara of ttext 
    | TSection of ttext * tdoc 
    | TBlockLet of var * expr
    | TBlockExpr of expr
  and ttext = tinline list
  and tinline = 
    | TString of string 
    | TBold of ttext 
    | TInlineLet of var * expr
    | TInlineExpr of expr
  [@@deriving show]

  let rec desugar (e : expr) : expr = match e with
    | EString s -> EString s
    | EBold e -> EBold (desugar e)  
    | EPara e  -> EPara (desugar e)
    | ESection (e1, e2) -> ESection (desugar e1, desugar e2)
    | List_ es -> List_ (List.map desugar es)
    | Cons (e1, e2) -> Cons (desugar e1, desugar e2)
    | Let (x, e1, e2) -> Let (x, desugar e1, desugar e2)
    | Var x -> Var x
    | Template t -> desugar_tmpl t
  and desugar_tmpl (d : tdoc) : expr  = match d with
    | [] -> List_ []
    | b :: bs -> (match b with
        | TBlockLet (x, e) -> Let (x, e, desugar_tmpl bs)
        | _ -> Cons (desugar_block b,  desugar_tmpl bs))
  and desugar_block (b : tblock) : expr = match b with
    | TPara t -> EPara (desugar_text t)
    | TSection (t, d) -> ESection (desugar_text t, desugar_tmpl d)
    | TBlockExpr e -> desugar e
    | TBlockLet _ -> raise Unreachable
  and desugar_text (t : ttext) : expr = match t with 
    | [] -> List_ []
    | l :: ls -> (match l with 
        | TInlineLet (x, e) -> Let (x, e, desugar_text ls)
        | _ -> Cons (desugar_inline l, desugar_text ls))
  and desugar_inline (l : tinline) : expr = match l with
    | TString s -> EString s
    | TBold t -> EBold (desugar_text t)
    | TInlineExpr e -> desugar e
    | TInlineLet _ -> raise Unreachable

  let rec subst x e1 e2 = match e2 with
    | EString s -> EString s
    | EBold e' -> EBold (subst x e1 e')
    | EPara e' -> EPara (subst x e1 e')
    | ESection (e1', e2') -> ESection (subst x e1 e1', subst x e1 e2')
    | List_ es -> List_ (List.map (subst x e1) es)
    | Cons (e1', e2') -> Cons (subst x e1 e1', subst x e1 e2')
    | Var y -> if x = y then e1 else e2
    | Let (y, e1', e2') -> 
      Let (y, subst x e1 e1', if x = y then e2' else subst x e1 e2')
    | Template _ -> raise Unreachable

  let[@warning "-partial-match"] rec eval (e : expr) : expr = match e with 
    | EString s -> EString s
    | EBold e -> EBold (eval e)
    | EPara e -> EPara (eval e)
    | ESection (e1, e2) -> ESection(eval e1, eval e2)
    | List_ es -> List_ (List.map eval es)
    | Cons (e1, e2) -> 
      let (x, List_ l) = (eval e1, eval e2) in
      List_ (x :: l)
    | Let (x, e1, e2) -> eval (subst x e1 e2)
    | Var _ -> raise Undefined_behavior    
    | Template _ -> raise Unreachable

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
end
