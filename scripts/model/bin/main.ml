open Model.Base
open Model.Strings
open Model.Articles

let main () = 
  Printf.printf "\n";

  let open DStrLit in
  DStrLitFrag.register();
  assert (Expr.eval (String "sup") = (String "sup"));

  let open DStrProg in
  DStrProgFrag.register();
  assert (Expr.eval (Concat (String "hello", String " world")) = (String "hello world"));
  assert (Expr.eval (Let("x", String "a", Concat(Var "x", Concat(String "b", Var "x")))) = String "aba");

  let open DStrTLit in
  DStrTLitFrag.register();
  DStrTLitInlineFrag.register();
  assert (Expr.desugar_eval (Let("world", String " World", StrTmpl [TStr "Hello"; TInlineExpr (Var "world")])) = (String "Hello World"));

  let open DStrTProg in
  DStrTProgFrag.register();
  DStrTProgInlineFrag.register();
  Printf.printf "%s\n" (Expr.show (Expr.desugar (StrTmpl [TInlineForeach(List [String "a"; String "b"], "x", [TInlineExpr (Var "x"); TStr "c"])])));
  assert (Expr.desugar_eval (StrTmpl [TInlineForeach(List [String "a"; String "b"], "x", [TInlineExpr (Var "x"); TStr "c"])]) = (String "acbc"));

  let open DTreeTProg in
  DTreeProgFrag.register();
  DTreeTProgFrag.register();
  DTreeTProgInlineFrag.register();
  let tmpl = [TNode ("p", [
      TStr "Hello";
      TInlineSet ("world", String "World");
      TInlineExpr (Var "world");
      TInlineForeach (List [String "?"; String "!"], "x", [TNode ("bold", [TInlineExpr (Var "x")])]);    
    ])] in
  Printf.printf "%s\n" (Expr.show (Expr.desugar (TreeTmpl tmpl)));
  Printf.printf "%s\n" (Expr.show (Expr.desugar_eval (TreeTmpl tmpl)));

  let open DTreeTProgReflow in 
  let tmpl = [
    TStr "Hello";
    TNode ("h1", [TStr "Intro"]);
    TStr "World"
  ] in
  let[@warning "-partial-match"] List elems = Expr.desugar_eval (TreeTmpl tmpl) in
  Printf.printf "%s\n" (Expr.show (List (reflow {para = []} elems)))


let () = main ()