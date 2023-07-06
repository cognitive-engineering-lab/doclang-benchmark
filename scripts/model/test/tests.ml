open Model.Base
open Model.String
open Model.Article
(* open Model.Article_ext *)

let main () =    
  let open DStrLit in
  DStrLitFrag.register();
  DStrLitTypeFrag.register();
  assert (Expr.eval (String "sup") = (String "sup"));

  let open DStrProg in
  DStrProgFrag.register();
  DStrProgTypeFrag.register();
  assert (Expr.eval (Concat (String "hello", String " world")) = (String "hello world"));
  let e = Let("x", String "a", Concat(Var "x", Concat(String "b", Var "x"))) in
  assert (Expr.eval e = String "aba");

   (* let l = list TString [String "a"] in
  let e = with_prelude (append TString l l) in
   Printf.printf "%s\n" (Expr.show e);
   Printf.printf "%s\n" (Expr.show (Expr.eval e)); *)

  (* let e = with_prelude (app1 (Var "flatten") (cons l (cons l enil))) in *)
  (* let e = with_prelude (app2 (Var "foreach") (lam1 "x" (Concat (Var "x", String "b"))) l) in
     Printf.printf "%s\n" (Expr.show (Expr.eval e)); *)

  let open DStrTLit in
  DStrTLitFrag.register();
  DStrTLitInlineFrag.register();
  let e = with_prelude (Let("world", String " World", StrTmpl [TStr "Hello"; TExpr (Var "world")])) in
  (* Printf.printf "%s\n" (Expr.show (Expr.desugar e)); *)
  assert (Expr.desugar_eval e = (String "Hello World"));

  let open DStrTProg in
  DStrTProgInlineFrag.register();
  let e = with_prelude (StrTmpl [TForeach (list TString [String "a"; String "b"], "x", TString, [TExpr (Var "x"); TStr "c"])]) in
  assert (Expr.desugar_eval e = (String "acbc"));

  let open DTreeProg in
  let open DTreeTProg in
  DTreeProgFrag.register();
  DTreeTProgFrag.register();
  DTreeTProgInlineFrag.register();
  let e = with_prelude (TreeTmpl [TNode ("p", [
      TStr "Hello";
      TSet ("world", text (String "World"));
      TExpr (Var "world");
      TForeach (
        list TString [String "?"; String "!"], "x",  TString,
        [TNode ("bold", [TExpr (text (Var "x"))])]);    
    ])]) in
  let expected = Expr.eval (with_prelude (nodelist [node (String "p") (nodelist [
      text (String "Hello"); text (String "World");
      node (String "bold") (nodelist [text (String "?")]);
      node (String "bold") (nodelist [text (String "!")])
    ])])) in
   Printf.printf "%s\n" (Expr.show (Expr.desugar_eval e));
   Printf.printf "%s\n" (Expr.show expected);
   
  assert (Expr.desugar_eval e = expected);

  (* let open DTreeTProgNested in *)
  (* DTreeTProgNestedFrag.register();
  DTreeTProgNestedInlineFrag.register();
  Printf.printf "%s\n" (Expr.show (Expr.desugar_eval e));
  Printf.printf "%s\n" (Expr.show expected);
  assert (Expr.desugar_eval e = expected); *)

  (* assert (Expr.is_article (Expr.desugar_eval e)); *)

  (* let open DExtReflow in 
     let e = TreeTmpl [
     TStr "Hello";
     TNode ("h1", [TStr "Intro"]);
     TStr "World"
     ] in
     let e' = Expr.desugar_eval e in
     assert (not (Expr.is_article e'));
     (* Printf.printf "%s\n" (Expr.show (List (reflow {para = []} elems))); *)
     assert (reflow e' = List [
     Node(String "p", List [String "Hello"]);
     Node(String "h1", List [String "Intro"]);
     Node(String "p", List [String "World"])
     ]);
     assert (Expr.is_article (reflow e')); *)



  ()


let () = main ()