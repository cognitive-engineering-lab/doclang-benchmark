open Model.Base
open Model.String
open Model.Article
open Model.Article_ext

let main () =    
  let open DStrLit in
  DStrLitFrag.register();
  assert (Expr.eval (String "sup") = (String "sup"));

  let open DStrProg in
  DStrProgFrag.register();
  assert (Expr.eval (Concat (String "hello", String " world")) = (String "hello world"));
  let e = Let("x", String "a", Concat(Var "x", Concat(String "b", Var "x"))) in
  assert (Expr.eval e = String "aba");

  let open DStrTLit in
  DStrTLitFrag.register();
  DStrTLitInlineFrag.register();
  let e = Let("world", String " World", StrTmpl [TStr "Hello"; TExpr (Var "world")]) in
  assert (Expr.desugar_eval e = (String "Hello World"));

  let open DStrTProg in
  DStrTProgFrag.register();
  DStrTProgInlineFrag.register();
  let e = StrTmpl [TForeach(List [String "a"; String "b"], "x", [TExpr (Var "x"); TStr "c"])] in
  assert (Expr.desugar_eval e = (String "acbc"));

  let open DTreeProg in
  let open DTreeTProg in
  DTreeProgFrag.register();
  DTreeTProgFrag.register();
  DTreeTProgInlineFrag.register();
  let e = TreeTmpl [TNode ("p", [
      TStr "Hello";
      TSet ("world", String "World");
      TExpr (Var "world");
      TForeach (List [String "?"; String "!"], "x", [TNode ("bold", [TExpr (Var "x")])]);    
    ])] in
  assert (Expr.desugar_eval e = List [Node (String "p", List [
      String "Hello"; String "World";
      Node (String "bold", List [String "?"]);
      Node (String "bold", List [String "!"])
    ])]);
  assert (Expr.is_article (Expr.desugar_eval e));

  let open DExtReflow in 
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
  assert (Expr.is_article (reflow e'));



  ()


let () = main ()