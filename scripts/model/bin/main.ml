let ex1 () = 
  let open Model.Strings.DStrVar in
  let e = Let("x", String("a"), Concat(Var("x"), Concat(String("b"), Var("x")))) in
  Printf.printf "%s\n" (eval e)    

let ex2 () =
  let open Model.Strings.DStrTmpl1 in 
  let e = Let("x", String("a"), Template([TExpr(Var("x")); TString("b"); TExpr(Var("x"))])) in
  Printf.printf "%s\n" (eval  (desugar_expr e))

let ex3 () = 
  let open Model.Strings.DStrTmpl2 in 
  let e = Template([
      TLet("x", String("a"));
      TExpr(Var("x")); TString("b"); TExpr(Var("x"))
    ]) in
  Printf.printf "%s\n" (eval (desugar_expr e))

let ex4 () =
  let open Model.Strings.DStrTmpl3 in 
  let e = Template([
      TString "<ul>"; 
      TForeach (
        List_ [String "Will"; String "Shriram"], "x", 
        [TString "<li>"; TExpr (Var "x"); TString "</li>"]); 
      TString "</ul>"]) in
  let[@warning "-partial-match"] String s = eval (desugar_expr e) in
  Printf.printf "%s\n" s


let ex5 () = 
  let open Model.Trees.AnchoredTreeDoc in 
  let open Model.Trees.DAnchoredTreeId in 
  let d1 = [Para [Link("a", [])]] in
  let d2 = Section(Some("a"), [], []) :: d1 in
  assert (not (wf d1));
  assert (wf d2)

let ex6 () = 
  let open Model.Trees.TreeDoc in 
  let open Model.Trees.DTreeVar in
  let e = Let("x", EString "a", List_ [EPara (List_ [Var "x"; Var "x"])]) in
  let d = to_doc (eval e) in
  print_endline (show_doc d)

let ex7 () = 
  let open Model.Trees.TreeDoc in 
  let open Model.Trees.DTreeTmpl in 
  let e = Template([
      TBlockLet("x", EString "a");
      TPara [TInlineExpr (Var "x"); TInlineExpr (Var "x")]
    ]) in
  let d = to_doc (eval (desugar e)) in
  print_endline (show_doc d)

let ex8 () = 
  let open Model.Trees.TreeDoc in 
  let open Model.Reactivity.DReactVar in 
  let e = 
    List_ [
      List_ [EPara (List_ [EString "The current state is: "; Var "state"])];
      Concat (Var "state", EString "x")
    ] 
  in
  let s0 = EString "" in
  let ev = EString "click" in 
  let (d1, s1) = step e s0 ev in
  print_endline (show_doc d1);
  print_endline (show_expr s1);
  let (d2, s2) = step e s1 ev in
  print_endline (show_doc d2);
  print_endline (show_expr s2)

let () = 
  ex1();
  ex2();
  ex3();
  ex4();
  ex5();
  ex6();
  ex7();
  ex8()
