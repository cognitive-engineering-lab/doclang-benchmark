[@@@warning "-partial-match"]
(* 
open Base
open String
open Article



module DExtReflow = struct
  open DStrLit  
  open DStrTLit
  open DTreeProg
  open DExtIdent

  type reflow_ctxt = {
    para : Expr.t list;    
  }  

  let block_elems = ["h1"; "section"]

  let reflow a =     
    let rec aux ctxt = function
      | List [] -> if List.length ctxt.para > 0 then
          List [Node (String "p", List ctxt.para)]
        else List []
      | List (e :: es) ->
        (match e with            
         | String _ -> 
           let para = e :: ctxt.para in
           aux {para} (List es)
         | Node (String tag, _) | AttrNode (String tag, _, _) ->
           if List.mem tag block_elems then 
             let p =  if List.length ctxt.para > 0 then 
                 [Node (String "p", List ctxt.para)] else []
             in
             let List es' = aux {para = []} (List es) in
             List (p @ (e :: es'))
           else
             let para = e :: ctxt.para in
             aux {para} (List es)
         | _ -> raise Undefined_behavior
        )
    in aux {para = []} a
end

module DExtReact = struct
  open DStrLit
  open DStrTLit
  open DTreeProg 
  open DExtIdent

  type component = {
    init: Expr.t;
    update: Expr.t;
    view: Expr.t;
  }

  type tcomponent = {
    init: Expr.t;
    update: Expr.t;
    view: Template.ttext;
  }

  type instance_id = int
  type instance = {
    component: component;    
    args: Expr.t;
    state: Expr.t;
    children: Expr.t;
    id: instance_id;
  }

  type Template.t += TComponent of tcomponent * Expr.t

  type Expr.t += 
    | Lambda of var * Expr.t
    | App of Expr.t * Expr.t 
    | Component of component * Expr.t
    | Instance of instance

  let eval = function
    | Lambda (x, e) -> Lambda (x, e)
    | App (e1, e2) -> 
      let Lambda (x, e) = Expr.eval e1 in
      let e' = Expr.subst (x, Expr.eval e2, e) in
      Expr.eval e'
    | Component ({init; update; view}, args) ->
      Component ({
          init = Expr.eval init; 
          update = Expr.eval update; 
          view = Expr.eval view
        }, Expr.eval args)    

  let desugar_template = function
    | TComponent (ct, e) -> 
      Component ({
          init = Expr.desugar ct.init; 
          update = Expr.desugar ct.update; 
          view = (Lambda ("a", Lambda ("s", desugar_template_elems ct.view)))}, 
          Expr.desugar e)

  let eval_update (c : component) (signal : Expr.t) (state : Expr.t) =
    Expr.eval (App (App (c.update, signal), state))

  let id_counter : instance_id ref = ref 0
  let rec docinit = function
    | String s -> String s
    | Node (tag, children) -> 
      Node (tag, docinit children)
    | AttrNode (tag, attrs, children) ->
      AttrNode (tag, attrs, docinit children)
    | List es -> List (List.map docinit es)
    | Component (component, args) ->
      let state = Expr.eval (App (component.init, args)) in
      let children = eval_view component args state in
      let id = !id_counter in 
      id_counter := !id_counter + 1;
      Instance {component; args; state; children; id}
    | _ -> raise Undefined_behavior
  and eval_view (c : component) (args : Expr.t) (state : Expr.t) = 
    let absview = Expr.eval (App (App (c.view, args), state)) in
    docinit absview

  let rec docview = function
    | String s -> String s
    | List es -> Expr.eval (List (List.map docview es))
    | Node (tag, children) -> Node(tag, docview children)
    | AttrNode (tag, attrs, children) -> AttrNode(tag, attrs, docview children)
    (* | Instance inst -> Splice (docview inst.children) *)
    | _ -> raise Undefined_behavior


  let rec has_section = function
    | String _ -> false
    | AttrNode (String "section", _, _) | Node (String "section",  _) -> true
    | AttrNode(_, _, children) | Node(_, children) -> has_section children
    | List es -> List.exists has_section es
    | Instance _ -> false
    | _ -> raise Undefined_behavior


  let dirty = ref false

  let docstep (signals: (instance_id * Expr.t) list) e = 
    dirty := false;
    let rec aux = function
      | String s -> String s
      | Node (tag, children) ->
        Node (tag, aux children)
      | AttrNode (tag, attrs, children) ->
        AttrNode (tag, attrs, aux children)
      | List es -> List (List.map aux es)
      | Instance inst ->
        let signal_opt = List.assoc_opt inst.id signals in
        (match signal_opt with
         | Some signal -> 
           let state' = eval_update inst.component signal inst.state in
           let children' = eval_view inst.component inst.args state' in
           dirty := !dirty || has_section inst.children || has_section children';
           Instance {inst with state = state'; children = reconcile inst.children children'}
         | None -> Instance {inst with children = aux inst.children})
      | _ -> raise Undefined_behavior
    and reconcile e e' = match (e, e') with
      | (_, String s') -> String s'
      | (Node (_, children), Node (String tag, children')) ->
        Node (String tag, reconcile children children')
      | (AttrNode (_, _, children), AttrNode (String tag, attrs', children')) ->
        AttrNode (String tag, attrs', reconcile children children')
      | (Instance i, Instance i') ->
        if i.args = i'.args then aux (Instance i)
        else Instance i' 
      | (List es, List es') -> List (
          if List.length es = List.length es' then List.map2 reconcile es es' else es')       
      | (_, e) -> e
    in aux e

  (* Boring code *)

  let subst (x, e1, e2) = match e2 with
    | Lambda (y, e) -> 
      if x = y then Lambda (y, e) else Lambda (y, Expr.subst (x, e1, e))
    | App (e1', e2') -> App (Expr.subst (x, e1, e1'), Expr.subst (x, e1, e2'))
    | Component (c, e) -> Component ({
        init = Expr.subst (x, e1, c.init);
        update = Expr.subst (x, e1, c.update);
        view = Expr.subst (x, e1, c.view)
      }, Expr.subst (x, e1, e))    


  let desugar = function
    | Lambda (x, e) -> Lambda (x, Expr.desugar e)
    | App (e1, e2) -> App (Expr.desugar e1, Expr.desugar e2)
    | Component (c, e) -> Component ({
        init = Expr.desugar c.init;
        update = Expr.desugar c.update;
        view = Expr.desugar c.view
      }, Expr.desugar e)

  let show_component ({init; update; view} : component) = 
    Printf.sprintf "{init=%s} {update=%s} {view=%s}" (Expr.show init) (Expr.show update) (Expr.show view)

  let show = function
    | Lambda (x, e) -> Printf.sprintf "(λ%s.%s)" x (Expr.show e)
    | App (e1, e2) -> Printf.sprintf "%s %s" (Expr.show e1) (Expr.show e2)
    | Component (c, e) -> Printf.sprintf "<C {args=%s} %s />" (Expr.show e) (show_component c)
    | Instance i -> Printf.sprintf "<I#%d {args=%s} {state=%s}>%s</I>" i.id (Expr.show i.args) (Expr.show i.state) (Expr.show i.children)

  let show_template = function
    | TComponent (c, args) -> Printf.sprintf "<C {init=%s} {update=%s} {args=%s}>%s</>" (Expr.show c.init) (Expr.show c.update) (Expr.show args) (show_ttext c.view)

  let eval_list = Open_func.noop
  let desugar_template_in_context = Open_func.noop
  let is_article = Open_func.noop
end
module DExtReactFrag = MakeExprFragment(DExtReact)
module DExtReactTemplateFrag = MakeTemplateFragment(DExtReact)
 *)
