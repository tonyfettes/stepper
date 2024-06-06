module Expr = Stepper.Expr

let test_recursive_function () =
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
    | Some value -> "Value " ^ Stepper.Value.to_string value
  in
  Alcotest.check Alcotest.string "same string" "Value 120" result

let test_type_inference (program : string) (expect : string) () =
  let typer = Stepper.Typer.create ~capacity:42 in
  let result =
    match
      program |> Stepper.parse |> Option.map (Stepper.Typer.infer typer)
    with
    | None -> "None"
    | Some ty -> "Some " ^ Stepper.Typer.Type.to_string ty
  in
  Alcotest.check Alcotest.string "same string" ("Some " ^ expect) result

let ( #> ) a b = (a, b)

let () =
  Alcotest.run "Stepper"
    [
      "Recursive function"
      #> [ Alcotest.test_case "Factorial of 5" `Quick test_recursive_function ];
      "Type inference"
      #> [
           Alcotest.test_case "Infer 1 + 2" `Quick
             (test_type_inference "1 + 2" "Int");
           (* Alcotest.test_case "Infer id" `Quick *)
           (*   (test_type_inference "(fun x -> x)" "('0) -> '0"); *)
           (* Alcotest.test_case "Infer if b then x else y" `Quick *)
           (*   (test_type_inference "(fun x -> fun y -> fun b -> if b then y else x)" "('0) -> ('0) -> (Bool) -> ('0)"); *)
           Alcotest.test_case "Infer x -> y -> y x" `Quick
             (test_type_inference "(fun x -> fun y -> y(x))" "");
         ];
    ]
