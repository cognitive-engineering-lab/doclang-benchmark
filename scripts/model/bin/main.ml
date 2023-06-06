[@@@warning "-unused-open"]

open Model.Base
open Model.String
open Model.Article
open Model.Article_ext

let main () = 
  Printf.printf "\n";

  let open DStrLit in
  DStrLitFrag.register();

  let open DStrProg in
  DStrProgFrag.register();

  let open DStrTLit in
  DStrTLitFrag.register();
  DStrTLitInlineFrag.register();

  let open DStrTProg in
  DStrTProgFrag.register();
  DStrTProgInlineFrag.register();

  let open DTreeProg in
  let open DTreeTProg in
  DTreeProgFrag.register();
  DTreeTProgFrag.register();
  DTreeTProgInlineFrag.register();

  let open DExtIdent in 
  DExtIdentFrag.register();
  DExtIdentTemplateFrag.register();
  let mksec id children =
    AttrNode (String "section", List [List [String "id"; String id]], List children)
  in
  let e = List [
      mksec "intro" [
        mksec "subhead" [
          mksec "innermost" []
        ];
        mksec "subhead2" []
      ];
      mksec "background" [
        mksec "bg2" []
      ]
    ] in    
  let ctx = section_ids e in
  Printf.printf "{%s}\n" (
    String.concat ", " (
      List.map (fun (k, v) ->
          Printf.sprintf "%s: %s" k (fmt_sec_num v)) ctx));
  let e = List [
      AttrNode (String "ref", List [List [String "id"; String "intro"]], List []);
      mksec "intro" []
    ] in
  Printf.printf "%s\n" (Expr.show (Expr.desugar_eval e));
  Printf.printf "%s\n" (Expr.show (render_refs (Expr.desugar_eval e)));

  let open DExtReflow in 
  let open DExtReact in
  DExtReactFrag.register();
  DExtReactTemplateFrag.register();

  let a_counter: tcomponent = {
    init = Lambda ("_a", String "a");
    update = Lambda("signal", Lambda ("s", Concat (Var "s", String "a")));
    view = [TExpr (Var "s")]
  } in

  let i_counter = {
    init = Lambda ("_a", String "|");
    update = Lambda("signal", Lambda ("s", Concat (Var "s", String "|")));
    view = [TExpr (Var "s"); TComponent (a_counter, String "")]
  } in

  let e = TreeTmpl [TComponent (i_counter, String "")] in
  Printf.printf "e desugared = %s\n" (Expr.show (Expr.desugar e));
  let e0 = docinit (Expr.desugar_eval e) in
  Printf.printf "e0 = %s\n" (Expr.show e0);
  Printf.printf "e0-view = %s\n" (Expr.show (render_refs (reflow (docview e0))));
  let signals = [(0, String ""); (1, String "")] in
  let e1 = docstep signals e0 in
  Printf.printf "e1 = %s\n" (Expr.show e1);
  Printf.printf "e1-view = %s\n" (Expr.show (render_refs (reflow (docview e1))));

  let add_section: tcomponent = {
    init = Lambda ("_a", List []);
    update = Lambda("_signal", Lambda("s", Cons(String ".", Var "s")));
    view = [      
      TForeach(Var "s", "_", [TNode("section", [])]); 
      TAttrNode("section", List [List [String "id"; String "intro"]], [])
    ]
  } in
  let e = TreeTmpl [TAttrNode("ref", List [List [String "id"; String "intro"]], []); TComponent (add_section, String "")] in
  let e0 = docinit (Expr.desugar_eval e) in
  Printf.printf "e0 = %s\n" (Expr.show e0);
  Printf.printf "e0-view = %s\n" (Expr.show (render_refs (reflow (docview e0))));  
  let e1 = docstep [(3, String "")] e0 in
  Printf.printf "dirty? %s\n" (string_of_bool !dirty);
  Printf.printf "e1 = %s\n" (Expr.show e1);
  Printf.printf "e1-view = %s\n" (Expr.show (render_refs (reflow (docview e1))));

  ()



let () = main ()