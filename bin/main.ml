let () =
  let expr = Stepper.Expr.Fun ("x", Add (Var "x", Var "x")) in
  expr |> Stepper.Expr.string_of_t |> print_endline

let () =
  Stepper.Subst.subst (Add (Var "x", Var "y")) "x" (Int 1) |> Stepper.Expr.string_of_t |> print_endline;
  Stepper.Subst.subst (Fun ("x", Var "x")) "x" (Int 1) |> Stepper.Expr.string_of_t |> print_endline;
  Stepper.Subst.subst (Fun ("x", Add (Var "x", Var "y"))) "y" (Int 1) |> Stepper.Expr.string_of_t |> print_endline

let () =
  Stepper.Eval.eval (App (Fun ("x", Add (Var "x", Var "y")), (Int 1))) |> Stepper.Eval.string_of_t |> print_endline
