open Stepper
open Syntax

let check message condition = if not condition then failwith message
let int n = Exp.Int (Int32.of_int n)
let flt ?(pat = Pat.Any) act gas : filter = { pat; act; gas }
let residue ?(gas = Gas.All) act lvl : residue = { act; gas; lvl }
let parse source = Option.get (Parse.parse source)

let only exp =
  match Step.decompose exp with
  | [ obj ] -> obj
  | _ -> failwith "expected exactly one candidate"

let take (obj : Step.act_obj) =
  Ctx.compose obj.ctx (Step.transition obj.exp)

let equal_exp expected actual =
  if expected <> actual then
    failwith
      (Printf.sprintf "expected %s, got %s" (Print.exp expected)
         (Print.exp actual))

let value source expected =
  match Step.step ~limit:1000 (parse source) with
  | Val actual -> equal_exp expected actual
  | Exp _ -> failwith "expected a value"

let expect_unbound exp at =
  match Step.decompose exp with
  | exception Step.Unbound_variable actual -> equal_exp at actual
  | _ -> failwith "expected Unbound_variable"

let tests =
  [
    ( "plain candidates are independent and ordered",
      fun () ->
        let exp = parse "(1 + 2) * (3 + 4)" in
        let objs = Step.decompose exp in
        check "both operands" (List.length objs = 2);
        List.iter
          (fun (o : Step.act_obj) ->
            check "default pause" (o.act = Pause && o.act_lvl = 0);
            equal_exp exp (Ctx.compose o.ctx o.exp))
          objs;
        equal_exp (parse "3 * (3 + 4)") (take (List.nth objs 0));
        equal_exp (parse "(1 + 2) * 7") (take (List.nth objs 1)) );
    ( "one persistent residue for a matching ancestor",
      fun () ->
        let f = flt Eval All in
        let body = Exp.Add (Exp.Add (int 1, int 2), int 3) in
        let obj = only (Exp.Filter (f, body)) in
        check "exact context"
          (obj.ctx = Ctx.AddL (Residue (residue Eval 1, Filter (f, Top)), int 3));
        check "action" (obj.act = Eval && obj.act_lvl = 1) );
    ( "one-shot action and persistent residue have separate levels",
      fun () ->
        let outer = flt Pause All and inner = flt Eval One in
        let obj =
          only (Exp.Filter (outer, Filter (inner, Add (int 1, int 2))))
        in
        check "inner action" (obj.act = Eval && obj.act_lvl = 2);
        check "outer residue keeps its own action"
          (obj.ctx
          = Ctx.Residue (residue Pause 1, Filter (inner, Filter (outer, Top))))
    );
    ( "lookup stops at the first persistent match",
      fun () ->
        let outer = flt Eval All and inner = flt Pause All in
        let obj =
          only (Exp.Filter (outer, Filter (inner, Add (int 1, int 2))))
        in
        check "only inner residue"
          (obj.ctx
          = Ctx.Residue (residue Pause 2, Filter (inner, Filter (outer, Top))))
    );
    ( "one-shot matches insert no residue",
      fun () ->
        let f = flt Eval One in
        let obj = only (Exp.Filter (f, Add (int 1, int 2))) in
        check "context" (obj.ctx = Filter (f, Top)) );
    ( "equal-level residue cannot override the action",
      fun () ->
        let obj =
          only
            (Exp.Residue
               (residue Pause 2, Residue (residue Eval 2, Add (int 1, int 2))))
        in
        check "strict comparison" (obj.act = Pause && obj.act_lvl = 2);
        check "duplicate dropped" (obj.ctx = Residue (residue Pause 2, Top)) );
    ( "one-shot residue is spent but still sets the cutoff",
      fun () ->
        let f = flt Eval All in
        let obj =
          only
            (Exp.Filter
               (f, Residue (residue ~gas:One Pause 1, Add (int 1, int 2))))
        in
        check "spent residue" (obj.ctx = Filter (f, Top));
        check "filter below cutoff was not retried" (obj.act = Pause) );
    ( "higher filters remain eligible above a residue cutoff",
      fun () ->
        let outer = flt Eval All and inner = flt Pause One in
        let obj =
          only
            (Exp.Filter
               ( outer,
                 Filter (inner, Residue (residue Eval 1, Add (int 1, int 2))) ))
        in
        check "higher override" (obj.act = Pause && obj.act_lvl = 2) );
    ( "wrapper elimination keeps the original context",
      fun () ->
        let f = flt Pause All in
        let wrapped = Exp.Filter (f, int 4) in
        let obj = only wrapped in
        check "filter silent"
          (obj.act = Eval && obj.ctx = Top && obj.exp = wrapped);
        let wrapped = Exp.Residue (residue Pause 3, int 4) in
        let obj = only wrapped in
        check "residue silent"
          (obj.act = Eval && obj.ctx = Top && obj.exp = wrapped);
        equal_exp (int 4) (take obj) );
    ( "only the chosen context commits new residues",
      fun () ->
        let f = flt ~pat:(Pat.Add (Val, Val)) Eval All in
        let exp =
          Exp.Filter (f, Mul (Add (int 1, int 2), Add (int 3, int 4)))
        in
        let objs = Step.decompose exp in
        equal_exp
          (Exp.Filter
             (f, Mul (Add (int 1, int 2), Residue (residue Eval 1, int 7))))
          (take (List.nth objs 1)) );
    ( "silent right candidate runs before a visible left candidate",
      fun () ->
        let exp = parse "(1 + 2) + (debug eval($e) in 3 + 4)" in
        match Step.step exp with
        | Exp [ obj ] ->
            equal_exp (parse "1 + 2") obj.exp;
            equal_exp (parse "(1 + 2) + 7") (Ctx.compose obj.ctx obj.exp)
        | _ -> failwith "expected one visible left candidate" );
    ( "silent reduction picks the leftmost candidate",
      fun () ->
        let f = flt Eval One in
        let exp =
          Exp.Filter (f, Add (Add (int 1, int 2), Add (int 3, int 4)))
        in
        match Step.step ~limit:1 exp with
        | exception Step.Limit actual ->
            equal_exp (Exp.Filter (f, Add (int 3, Add (int 3, int 4)))) actual
        | _ -> failwith "expected limit after one transition" );
    ( "all visible choices are returned",
      fun () ->
        match Step.step (parse "(1 + 2) + (3 + 4)") with
        | Exp [ _; _ ] -> ()
        | _ -> failwith "expected two choices" );
    ( "persistent eval reaches the value",
      fun () -> value "debug eval($e) in 1 + 2 + 3" (int 6) );
    ( "one-shot pause overrides persistent eval only while matching",
      fun () ->
        let exp =
          parse "debug eval($e) in debug stop(3 + 3 + 4) in 1 + 2 + 3 + 4"
        in
        match Step.step exp with
        | Exp [ obj ] -> (
            equal_exp (parse "3 + 3") obj.exp;
            match Step.step (Step.take obj) with
            | Val e -> equal_exp (int 10) e
            | _ -> failwith "pause outlived its match")
        | _ -> failwith "expected pause at 3 + 3" );
    ( "later ancestor matches do not duplicate residues",
      fun () ->
        let f = flt ~pat:(Pat.Add (Add (Val, Int 1l), Val)) Eval All in
        let exp =
          Exp.Filter (f, Add (Add (Add (int 1, int 1), int 1), int 5))
        in
        let e1 = take (only exp) in
        equal_exp
          (Exp.Filter
             (f, Add (Residue (residue Eval 1, Add (int 2, int 1)), int 5)))
          e1;
        let e2 = take (only e1) in
        equal_exp
          (Exp.Filter (f, Residue (residue Eval 1, Add (int 3, int 5))))
          e2 );
    ( "matching respects alpha equivalence and shadowing",
      fun () ->
        let p = Pat.Fun ("x", Fun ("y", Var "x")) in
        check "alpha" (Matches.matches p (parse "fun a -> fun b -> a"));
        check "shadowing"
          (not (Matches.matches p (parse "fun a -> fun a -> a"))) );
    ( "value wildcard tracks binder source",
      fun () ->
        check "fun bound"
          (Matches.matches (Pat.Fun ("x", Val)) (parse "fun y -> y"));
        check "fix bound"
          (not (Matches.matches (Pat.Fix ("x", Val)) (parse "fix y -> y")));
        check "boolean compatibility"
          (not (Matches.matches Pat.Val (Exp.Bool true))) );
    ( "matching ignores wrappers even inside binders",
      fun () ->
        let e =
          Exp.Fun
            ( "x",
              Filter
                (flt Eval All, Residue (residue Eval 1, Add (Var "x", int 2)))
            )
        in
        check "match" (Matches.matches (Pat.Fun ("y", Add (Var "y", Val))) e) );
    ( "substitution reaches filter patterns and respects binders",
      fun () ->
        let body = parse "debug stop(x + 1) in x + 1" in
        equal_exp
          (parse "debug stop(3 + 1) in 3 + 1")
          (Subst.exp body "x" (int 3));
        check "pattern shadowing"
          (Subst.pat (Pat.Add (Var "x", Fun ("x", Var "x"))) "x" (int 3)
          = Pat.Add (Int 3l, Fun ("x", Var "x"))) );
    ( "substitution strips wrappers when converting to a pattern",
      fun () ->
        check "patternize"
          (Subst.pat (Pat.Var "x") "x" (Exp.Filter (flt Eval All, int 2))
          = Pat.Int 2l) );
    ( "free variables report the offending subexpression",
      fun () ->
        expect_unbound (parse "x + y") (Exp.Var "x");
        let exp = parse "debug eval(y) in 1 + 2" in
        expect_unbound exp exp );
    ( "decomposition does not enter binders or untaken branches",
      fun () ->
        check "lambda value" (Step.decompose (parse "fun x -> free") = []);
        check "fix redex"
          ((only (parse "fix x -> free")).exp = parse "fix x -> free");
        value "debug eval($e) in if true then 1 else free" (int 1) );
    ( "mismatched operands raise on transition",
      fun () ->
        let obj = only (parse "true + 1") in
        match Step.transition obj.exp with
        | exception Step.Mismatched_type e -> equal_exp obj.exp e
        | _ -> failwith "expected Mismatched_type" );
    ( "fixpoint, boolean, arithmetic, and application transitions",
      fun () ->
        value
          "debug eval($e) in let rec fact = fun n -> if n == 0 then 1 else n * \
           fact(n - 1) in fact(5)"
          (int 120);
        value "debug eval($e) in (fun x -> x - 2)(7)" (int 5) );
    ( "integer overflow matches MoonBit Int",
      fun () -> value "debug eval($e) in 2147483647 + 1" (Exp.Int Int32.min_int)
    );
    ( "zero limit permits values and pauses",
      fun () ->
        check "value" (Step.step ~limit:0 (int 1) = Val (int 1));
        check "pause"
          (match Step.step ~limit:0 (parse "1 + 2") with
          | Exp [ _ ] -> true
          | _ -> false) );
    ( "divergence is bounded when requested",
      fun () ->
        match Step.step ~limit:20 (parse "debug eval($e) in fix x -> x") with
        | exception Step.Limit _ -> ()
        | _ -> failwith "expected Limit" );
    ( "step is persistent and stop is one-shot",
      fun () ->
        List.iter
          (fun (name, act, gas) ->
            equal_exp
              (Exp.Filter (flt act gas, int 1))
              (parse ("debug " ^ name ^ "($e) in 1")))
          [
            ("eval", Eval, All);
            ("hide", Eval, One);
            ("stop", Pause, One);
            ("step", Pause, All);
          ] );
    ( "source printer round-trips recursive patterns",
      fun () ->
        List.iter
          (fun s ->
            let exp = parse s in
            equal_exp exp (parse (Print.exp exp)))
          [
            "debug stop(fun x -> x + $v) in (fun y -> y + 1)(2)";
            "debug step(if $e then true else false) in if 1 == 2 then false \
             else true";
            "debug hide(fix f -> $e) in fix x -> fun y -> x(y)";
            "1 - 2 * (3 + -4)";
            "let rec f = fun x -> x in f(2)";
          ] );
    ( "invalid source and out-of-range integers are rejected",
      fun () ->
        List.iter
          (fun s ->
            match Parse.parse s with
            | exception Parse.Error _ -> ()
            | _ -> failwith ("accepted invalid source " ^ s))
          [ "2147483648"; "-2147483649"; "debug eval($e)"; "1 +"; "@" ] );
  ]

let () =
  List.iter
    (fun (name, run) ->
      try
        run ();
        Printf.printf "ok: %s\n%!" name
      with exn ->
        Printf.eprintf "FAILED: %s: %s\n" name (Printexc.to_string exn);
        exit 1)
    tests;
  Printf.printf "%d tests passed\n" (List.length tests)
