module Expr = Stepper.Expr
module Forest = Stepper.Forest

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

let test_union_find () =
  let forest = Stepper.Forest.create ~capacity:42 in
  for _ = 0 to 41 do
    Forest.add forest
  done;
  Forest.union forest 0 1;
  Forest.union forest 1 2;
  Alcotest.check Alcotest.int "parent of 0 = parent of 1" (Forest.find forest 0)
    (Forest.find forest 1);
  Alcotest.check Alcotest.int "parent of 0 = parent of 1" (Forest.find forest 1)
    (Forest.find forest 2);
  Alcotest.check Alcotest.int "parent of 3 is itself" 3 (Forest.find forest 3)

let ( |:: ) a b = (a, b)

let () =
  Alcotest.run "Stepper"
    [
      "Recursive function"
      |:: [ Alcotest.test_case "Factorial of 5" `Quick test_recursive_function ];
      "Union find"
      |:: [ Alcotest.test_case "0 union 1 union 2" `Quick test_union_find ];
    ]
