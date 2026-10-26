open Stepper
open Syntax
open QCheck2
module Print = Stepper.Print

let rec count pred (e : Exp.t) =
  (if pred e then 1 else 0)
  +
  match e with
  | Var _ | Int _ | Bool _ -> 0
  | Fun (_, e) | Fix (_, e) | Filter (_, e) | Residue (_, e) -> count pred e
  | Eq (l, r) | Add (l, r) | Sub (l, r) | Mul (l, r) | Ap (l, r) ->
      count pred l + count pred r
  | If (c, t, f) -> count pred c + count pred t + count pred f

let is_residue = function Exp.Residue _ -> true | _ -> false
let is_filter = function Exp.Filter _ -> true | _ -> false

let is_redex = function
  | Exp.Fix _ -> true
  | Filter (_, e) | Residue (_, e) -> is_value e
  | Eq (l, r) | Add (l, r) | Sub (l, r) | Mul (l, r) | Ap (l, r) ->
      is_value l && is_value r
  | If (c, _, _) -> is_value c
  | Var _ | Int _ | Bool _ | Fun _ -> false

let take (o : Step.act_obj) = Ctx.compose o.ctx (Step.transition o.exp)

let rec context_levels = function
  | Ctx.Top -> []
  | Residue (r, c) -> context_levels c @ [ r ]
  | Filter (_, c)
  | EqL (c, _)
  | EqR (_, c)
  | AddL (c, _)
  | AddR (_, c)
  | SubL (c, _)
  | SubR (_, c)
  | MulL (c, _)
  | MulR (_, c)
  | ApL (c, _)
  | ApR (_, c)
  | If (c, _, _) ->
      context_levels c

let ascending ctx =
  let rec go previous = function
    | [] -> true
    | (r : residue) :: rest -> r.gas = All && r.lvl > previous && go r.lvl rest
  in
  go 0 (context_levels ctx)

(* Check residues against the enclosing lexical filters, independently of lookup. *)
let instrumented exp =
  let rec go filters (e : Exp.t) =
    match e with
    | Var _ | Int _ | Bool _ -> true
    | Fun (_, e) | Fix (_, e) -> count is_residue e = 0
    | Filter (f, e) -> go (filters @ [ f ]) e
    | Residue (r, e) ->
        r.gas = All && r.lvl > 0
        && r.lvl <= List.length filters
        &&
        let f = List.nth filters (r.lvl - 1) in
        f.gas = All && f.act = r.act && go filters e
    | Eq (l, r) | Add (l, r) | Sub (l, r) | Mul (l, r) | Ap (l, r) ->
        go filters l && go filters r
    | If (c, t, f) -> go filters c && go filters t && go filters f
  in
  go [] exp

let simulation e (o : Step.act_obj) =
  let next = strip (take o) in
  next = strip e
  || List.exists (fun o -> strip (take o) = next) (Step.decompose (strip e))

(* No exception is swallowed here: these runs start from typed programs. *)
let rec along_run remaining index property exp =
  property exp
  &&
  if remaining = 0 then true
  else
    let objs = Step.decompose exp in
    match objs with
    | [] -> true
    | _ ->
        let chosen =
          match
            List.find_opt (fun (o : Step.act_obj) -> o.act = Eval) objs
          with
          | Some o -> o
          | None -> List.nth objs (index mod List.length objs)
        in
        along_run (remaining - 1) (index + 1) property (take chosen)

(* An independent alpha-equivalence oracle: erase wrappers and replace bound
   names with binder depths. Generated source names never contain '#'. *)
let canonical exp =
  let rec go depth bound (e : Exp.t) : Exp.t =
    let under x e =
      go (depth + 1) ((x, "#" ^ string_of_int depth) :: bound) e
    in
    let child = go depth bound in
    match e with
    | Var x -> Var (Option.value (List.assoc_opt x bound) ~default:x)
    | Int _ | Bool _ -> e
    | Fun (x, e) -> Fun ("#" ^ string_of_int depth, under x e)
    | Fix (x, e) -> Fix ("#" ^ string_of_int depth, under x e)
    | Eq (l, r) -> Eq (child l, child r)
    | Add (l, r) -> Add (child l, child r)
    | Sub (l, r) -> Sub (child l, child r)
    | Mul (l, r) -> Mul (child l, child r)
    | Ap (l, r) -> Ap (child l, child r)
    | If (c, t, f) -> If (child c, child t, child f)
    | Filter (_, e) | Residue (_, e) -> child e
  in
  go 0 [] exp

(* Big-step evaluation with closures, independent of decomposition/substitution.
   Used on the terminating, fixpoint-free subset; the result has integer type. *)
type value =
  | Number of int32
  | Boolean of bool
  | Closure of string * Exp.t * (string * value) list

let evaluate exp =
  let rec go env (e : Exp.t) =
    let number e =
      match go env e with
      | Number n -> n
      | _ -> failwith "oracle expected integer"
    in
    match e with
    | Int n -> Number n
    | Bool b -> Boolean b
    | Var x -> List.assoc x env
    | Fun (x, e) -> Closure (x, e, env)
    | Add (l, r) -> Number (Int32.add (number l) (number r))
    | Sub (l, r) -> Number (Int32.sub (number l) (number r))
    | Mul (l, r) -> Number (Int32.mul (number l) (number r))
    | Eq (l, r) -> Boolean (number l = number r)
    | If (c, t, f) -> (
        match go env c with
        | Boolean b -> go env (if b then t else f)
        | _ -> failwith "oracle expected boolean")
    | Ap (f, x) -> (
        let f = go env f in
        let x = go env x in
        match f with
        | Closure (name, body, env) -> go ((name, x) :: env) body
        | _ -> failwith "oracle expected function")
    | Filter (_, e) | Residue (_, e) -> go env e
    | Fix _ -> failwith "fixpoint in terminating generator"
  in
  match go [] exp with
  | Number n -> Exp.Int n
  | _ -> failwith "oracle expected integer result"

let rec finish remaining exp =
  if remaining = 0 then
    Test.fail_report ("execution bound reached: " ^ Print.exp exp);
  match Step.decompose exp with
  | [] -> exp
  | objs ->
      let chosen =
        match
          List.find_opt (fun (o : Step.act_obj) -> o.act = Eval) objs
        with
        | Some o -> o
        | None -> List.hd objs
      in
      finish (remaining - 1) (take chosen)

let property name gen check =
  Test.make ~name ~count:5000 ~long_factor:5 ~print:Print.exp gen check

let all exp check = List.for_all check (Step.decompose exp)

let tests =
  [
    property "recomposition preserves expressions up to residues"
      Generators.arbitrary (fun e ->
        all e (fun o ->
            strip ~filters:false (Ctx.compose o.ctx o.exp)
            = strip ~filters:false e));
    property "only values have no candidates" Generators.arbitrary (fun e ->
        Step.decompose e = [] = is_value e);
    property "every candidate is a redex" Generators.arbitrary (fun e ->
        all e (fun o -> is_redex o.exp));
    property "candidate residue levels strictly ascend" Generators.arbitrary
      (fun e -> all e (fun o -> ascending o.ctx));
    property "decomposition inserts at most one residue per filter"
      Generators.arbitrary (fun e ->
        all e (fun o ->
            count is_residue (Ctx.compose o.ctx o.exp)
            <= count is_residue e + count is_filter e));
    property "typed closed candidates always transition" Generators.typed
      (fun e ->
        all e (fun o ->
            ignore (Step.transition o.exp);
            true));
    property "filtered transitions simulate unfiltered evaluation"
      Generators.typed (fun e -> all e (simulation e));
    property "runs keep residues associated with their lexical filters"
      Generators.typed (fun e ->
        along_run 20 0
          (fun e -> instrumented e && all e (fun o -> ascending o.ctx))
          (strip ~filters:false e));
    property "simulation holds throughout a run" Generators.typed (fun e ->
        along_run 20 0 (fun e -> all e (simulation e)) e);
    property "filter choices preserve the evaluated result"
      Generators.terminating (fun e -> strip (finish 20000 e) = evaluate e);
    property "eval of every redex agrees with the closure evaluator"
      Generators.terminating (fun e ->
        let plain = strip e in
        let wrapped =
          Exp.Filter ({ pat = Pat.Any; act = Eval; gas = All }, plain)
        in
        match Step.step ~limit:20000 wrapped with
        | Val result -> result = evaluate plain
        | Exp _ -> false);
    property "printing source expressions round-trips" Generators.arbitrary
      (fun e ->
        let e = strip ~filters:false e in
        Parse.parse (Print.exp e) = Some e);
    Test.make ~name:"matching ignores wrappers" ~count:5000 ~long_factor:5
      ~print:(fun (p, e) -> Print.pat p ^ " matches " ^ Print.exp e)
      (Gen.pair Generators.patterns Generators.arbitrary)
      (fun (p, e) -> Matches.matches p e = Matches.matches p (strip e));
    Test.make ~name:"patternization matches exactly up to alpha equivalence"
      ~count:5000 ~long_factor:5
      ~print:(fun (a, b) -> Print.exp a ^ " versus " ^ Print.exp b)
      (Gen.oneof
         [
           Generators.renamed;
           Gen.pair Generators.arbitrary Generators.arbitrary;
         ])
      (fun (a, b) ->
        Matches.matches (Subst.to_pat a) b = (canonical a = canonical b));
  ]

let () = QCheck_base_runner.run_tests_main tests
