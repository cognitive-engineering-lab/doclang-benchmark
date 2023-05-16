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
  assert (Expr.desugar_eval (StrTmpl [TInlineForeach(List [String "a"; String "b"], "x", [TInlineExpr (Var "x"); TStr "c"])]) = (String "acbc"));

  let open DArtProg in 
  DArtProgFrag.register();
  Printf.printf "%s\n" (Expr.show (Section (List [String "A"], List [Para (List [String "B"])])));

  let open DArtTLit in 
  DArtTLitFrag.register();
  DArtTLitInlineFrag.register();
  DArtTLitBlockFrag.register();
  let inp = ArtTmpl [TSection ([TStr "A"], [TPara ([TStr "B"])])] in
  Printf.printf "%s\n" (Expr.show (Expr.desugar_eval inp));

  let open DArtTProg in 
  DArtTProgBlockFrag.register();
  let inp = ArtTmpl [TBlockForeach (List [String "a"; String "b"], "x", [TPara [TInlineExpr (Var "x")]])] in
  Printf.printf "%s\n" (Expr.show (Expr.desugar_eval inp))


let () = main ()