module Expr = Stepper.Expr

let%expect_test "recursive_function" =
  let program =
    Stepper.parse
      {|
      let rec fac =
        fun n ->
          if n == 0 then
            1
          else
            n * fac(n - 1)
      in
      fac(5)|}
  in
  let result = program |> Option.map Stepper.eval in
  let result =
    match result with
    | None -> "None"
    | Some (`Error (error, expr)) ->
        let error = Stepper.Error.to_string error in
        let expr = Stepper.Expr.to_string expr in
        Printf.sprintf "Error %s @ %s" error expr
    | Some (`Value value) -> "Value " ^ Stepper.Value.to_string value
  in
  print_string result;
  [%expect {| Value 120 |}]
