[@@@warning "-partial-match"]

open Base
open String

module DTreeProg = struct
  open DStrLit
  open DStrTLit

  type Expr.t += Node of Expr.t * Expr.t

  let eval = function
    | Node (nt, e) -> Node (nt, Expr.eval e)

  let subst (x, e1, e2) = match e2 with
    | Node (nt, e) -> Node (nt, Expr.subst (x, e1, e))

  let desugar = function
    | Node (nt, e) -> Node (nt, Expr.desugar e)

  let show = function
    | Node (nt, e) -> Printf.sprintf "<%s>%s</%s>" (Expr.show nt) (Expr.show e) (Expr.show nt)

  let eval_list = Open_func.noop  

  let is_article = function
    | String _ -> true
    | Node (String _, children) -> Expr.is_article children
    | List es -> List.for_all Expr.is_article es
    | _ -> false
end
module DTreeProgFrag = MakeExprFragment(DTreeProg)

module DTreeTProg = struct
  open Template
  open DStrLit
  open DTreeProg
  open DStrTLit

  type Template.t += TNode of string * ttext
  type Expr.t += TreeTmpl of ttext

  let eval = function TreeTmpl _ -> raise Not_desugared

  let subst (_, _, e2) = match e2 with TreeTmpl _ -> raise Not_desugared

  let desugar = function TreeTmpl kt -> desugar_template_elems kt

  let desugar_template = function
    | TNode (nt, kt) -> Node (String nt, desugar_template_elems kt)

  let show_template = function
    | TNode (nt, kt) -> Printf.sprintf "<%s>%s</%s>" nt (show_ttext kt) nt

  let show = function TreeTmpl kt -> show_ttext kt

  let eval_list = Open_func.noop
  let desugar_template_in_context = Open_func.noop
  let is_article = Open_func.noop
end
module DTreeTProgFrag = MakeExprFragment(DTreeTProg)
module DTreeTProgInlineFrag = MakeTemplateFragment(DTreeTProg)

module DExtIdent = struct
  open DStrLit
  open DStrTLit
  open DTreeProg

  type Expr.t += AttrNode of Expr.t * Expr.t * Expr.t
  type Template.t += TAttrNode of string * Expr.t * Template.ttext

  let eval = function
    | AttrNode (nt, a, e) -> AttrNode (nt, a, Expr.eval e)

  let subst (x, e1, e2) = match e2 with
    | AttrNode (nt, a, e) -> AttrNode (Expr.subst (x, e1, nt), Expr.subst (x, e1, a), Expr.subst (x, e1, e))

  let desugar = function
    | AttrNode (nt, a, e) -> AttrNode (Expr.desugar nt, Expr.desugar a, Expr.desugar e)

  let desugar_template = function
    | TAttrNode (nt, a, kt) -> AttrNode (String nt, a, desugar_template_elems kt)


  let show_attrs (List attrs) = StdString.concat " " (List.map (fun (List [k; v]) -> Printf.sprintf "%s=%s" (Expr.show k) (Expr.show v)) attrs)


  let show = function
    | AttrNode (nt, a, e) -> Printf.sprintf "<%s %s>%s</%s>" (Expr.show nt) (show_attrs a) (Expr.show e) (Expr.show nt)

  let show_template = function
    | TAttrNode (nt, a, kt) -> Printf.sprintf "<%s %s>%s</%s>" nt (show_attrs a) (show_ttext kt) nt

  let rec section_ids_at_depth (a : Expr.t) ((n :: ns) as d : int list) : ((string * int list) list) * (int list) =
    match a with 
    | String _ -> ([], d)
    | List es -> List.fold_left (fun (ids, d) e -> 
        let (ids', d') = section_ids_at_depth e d in
        (List.append ids ids', d')) ([], d) es
    | AttrNode (String "section", List [List [String "id"; String id]], children) ->
      let (ids, _) = section_ids_at_depth children (1 :: d) in     
      ((id, d) :: ids, (n + 1) :: ns)
    | Node (String "section", children) ->
      let (ids, _) = section_ids_at_depth children (1 :: d) in     
      (ids, (n + 1) :: ns)
    | Node (_, children) | AttrNode (_, _, children) -> section_ids_at_depth children d
    | _ -> raise Undefined_behavior  

  let section_ids e = 
    let (ctx, _) = (section_ids_at_depth e [1]) in ctx

  let fmt_sec_num d = StdString.concat "." (List.map Int.to_string (List.rev d))

  let rec replace_refs ctx = function
    | AttrNode (String "ref", List [List[String "id"; String id]], _) -> 
      let d = List.assoc id ctx in
      String (fmt_sec_num d)
    | AttrNode (nt, a, e) -> AttrNode (nt, a, replace_refs ctx e)
    | Node (nt, e) -> Node (nt, replace_refs ctx e)
    | List es -> List (List.map (replace_refs ctx) es)
    | String s -> String s
    | _ -> raise Undefined_behavior

  let render_refs a = 
    let ctx = section_ids a in
    replace_refs ctx a

  let is_article = function
    | AttrNode (String _, List _, List children) -> List.for_all Expr.is_article children

  let desugar_template_in_context = Open_func.noop
  let eval_list = Open_func.noop
end

module DExtIdentFrag = MakeExprFragment(DExtIdent)
module DExtIdentTemplateFrag = MakeTemplateFragment(DExtIdent)