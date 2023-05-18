[@@@warning "-partial-match"]

open Base
open Strings

module DTreeProg = struct
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
end
module DTreeTProgFrag = MakeExprFragment(DTreeTProg)
module DTreeTProgInlineFrag = MakeTemplateFragment(DTreeTProg)

module DTreeTProgReflow = struct
  open DStrLit
  open DStrTLit
  open DTreeProg

  type reflow_ctxt = {
    para : Expr.t list;    
  }  

  let block_elems = ["h1"]

  let rec reflow (ctxt : reflow_ctxt) = function
    | [] -> [Node (String "p", List ctxt.para)]
    | e :: es ->
      (match e with            
       | String _ -> 
         let para = e :: ctxt.para in
         reflow {para} es
       | Node (String tag, _) ->
         if List.mem tag block_elems then 
           let p = Node (String "p", List ctxt.para) in
           p :: e :: reflow {para = []} es
         else
           let para = e :: ctxt.para in
           reflow {para} es
       | _ -> raise Undefined_behavior
      )
end