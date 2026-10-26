open Stepper
open Syntax
module G = QCheck2.Gen

let name = G.oneof_list [ "x"; "y"; "z" ]
let act = G.oneof_list [ Act.Eval; Pause ]
let gas = G.oneof_list [ Gas.One; All ]
let number = G.map Int32.of_int (G.int_range (-3) 5)

let residue =
  G.map3 (fun act gas lvl -> { act; gas; lvl }) act gas (G.int_range 0 4)

(* All binding names have a small domain, so shadowing is exercised often.
   Recursive generators use QCheck2 combinators: shrinking stays attached to
   generated values, including inside binders and filter patterns. *)
let rec pat depth bound =
  let leaf =
    [
      G.pure Pat.Any;
      G.pure Pat.Val;
      G.map (fun n -> Pat.Int n) number;
      G.map (fun b -> Pat.Bool b) G.bool;
    ]
    @
    if bound = [] then []
    else [ G.map (fun x -> Pat.Var x) (G.oneof_list bound) ]
  in
  if depth = 0 then G.oneof leaf
  else
    let child = pat (depth - 1) bound in
    let binary make = G.map2 (fun l r -> make (l, r)) child child in
    let binder make =
      G.bind name (fun x ->
          G.map (fun p -> make (x, p)) (pat (depth - 1) (x :: bound)))
    in
    G.oneof
      (leaf
      @ [
          binary (fun (a, b) -> Pat.Eq (a, b));
          binary (fun (a, b) -> Pat.Add (a, b));
          binary (fun (a, b) -> Pat.Sub (a, b));
          binary (fun (a, b) -> Pat.Mul (a, b));
          binary (fun (a, b) -> Pat.Ap (a, b));
          binder (fun (a, b) -> Pat.Fun (a, b));
          binder (fun (a, b) -> Pat.Fix (a, b));
          G.map3 (fun c t f -> Pat.If (c, t, f)) child child child;
        ])

let filter pattern =
  G.map3 (fun pat act gas -> { pat; act; gas }) pattern act gas

let matching_pattern depth bound =
  G.oneof_weighted
    [
      (3, G.pure Pat.Any);
      (1, G.pure (Pat.Add (Val, Val)));
      (1, G.pure (Pat.Add (Add (Val, Int 1l), Val)));
      (2, pat depth bound);
    ]

(* Syntactically closed, possibly ill-typed expressions, including arbitrary
   pre-existing residues. No rejected samples are needed for decomposition. *)
let rec exp depth bound =
  let leaf =
    [ G.map (fun n -> Exp.Int n) number; G.map (fun b -> Exp.Bool b) G.bool ]
    @
    if bound = [] then []
    else [ G.map (fun x -> Exp.Var x) (G.oneof_list bound) ]
  in
  if depth = 0 then G.oneof leaf
  else
    let child = exp (depth - 1) bound in
    let binary make = G.map2 (fun l r -> make (l, r)) child child in
    let binder make =
      G.bind name (fun x ->
          G.map (fun e -> make (x, e)) (exp (depth - 1) (x :: bound)))
    in
    G.oneof
      (leaf
      @ [
          binary (fun (a, b) -> Exp.Eq (a, b));
          binary (fun (a, b) -> Exp.Add (a, b));
          binary (fun (a, b) -> Exp.Sub (a, b));
          binary (fun (a, b) -> Exp.Mul (a, b));
          binary (fun (a, b) -> Exp.Ap (a, b));
          binder (fun (a, b) -> Exp.Fun (a, b));
          binder (fun (a, b) -> Exp.Fix (a, b));
          G.map3 (fun c t f -> Exp.If (c, t, f)) child child child;
          G.map2
            (fun f e -> Exp.Filter (f, e))
            (filter (matching_pattern (depth - 1) bound))
            child;
          G.map2 (fun r e -> Exp.Residue (r, e)) residue child;
        ])

let arbitrary = G.sized_size (G.int_range 0 5) (fun depth -> exp depth [])
let patterns = G.sized_size (G.int_range 0 4) (fun depth -> pat depth [])

type typ = Int | Bool | Arrow of typ * typ

let rec typ depth =
  if depth = 0 then G.oneof_list [ Int; Bool ]
  else
    G.oneof
      [
        G.pure Int;
        G.pure Bool;
        G.map2 (fun a b -> Arrow (a, b)) (typ (depth - 1)) (typ (depth - 1));
      ]

let vars env typ =
  let rec go seen = function
    | [] -> []
    | (x, t) :: rest ->
        let tail = go (x :: seen) rest in
        if t = typ && not (List.mem x seen) then x :: tail else tail
  in
  go [] env

let rec leaf env = function
  | Int -> G.map (fun n -> Exp.Int n) number
  | Bool -> G.map (fun b -> Exp.Bool b) G.bool
  | Arrow (a, b) ->
      G.bind name (fun x ->
          G.map (fun e -> Exp.Fun (x, e)) (leaf ((x, a) :: env) b))

(* Types and lexical scopes are maintained during generation AND shrinking.
   [fix=false] gives a strongly normalizing subset for complete evaluation. *)
let rec typed_exp ~fix ~residues depth env t =
  let leaves =
    [ leaf env t ]
    @
    match vars env t with
    | [] -> []
    | xs -> [ G.map (fun x -> Exp.Var x) (G.oneof_list xs) ]
  in
  if depth = 0 then G.oneof leaves
  else
    let child t =
      G.delay (fun () -> typed_exp ~fix ~residues (depth - 1) env t)
    in
    let body x a b = typed_exp ~fix ~residues (depth - 1) ((x, a) :: env) b in
    let structural =
      match t with
      | Int ->
          List.map
            (fun make ->
              G.map2 (fun l r -> make (l, r)) (child Int) (child Int))
            [
              (fun (a, b) -> Exp.Add (a, b));
              (fun (a, b) -> Exp.Sub (a, b));
              (fun (a, b) -> Exp.Mul (a, b));
            ]
      | Bool -> [ G.map2 (fun l r -> Exp.Eq (l, r)) (child Int) (child Int) ]
      | Arrow (a, b) ->
          [
            G.bind name (fun x -> G.map (fun e -> Exp.Fun (x, e)) (body x a b));
          ]
    in
    let filters = filter (matching_pattern 2 (List.map fst env)) in
    let branches =
      structural
      @ [
          G.map3
            (fun c a b -> Exp.If (c, a, b))
            (child Bool) (child t) (child t);
          G.bind (typ 1) (fun a ->
              G.map2 (fun f x -> Exp.Ap (f, x)) (child (Arrow (a, t))) (child a));
          G.map2 (fun f e -> Exp.Filter (f, e)) filters (child t);
        ]
    in
    let branches =
      if fix then
        G.bind name (fun x -> G.map (fun e -> Exp.Fix (x, e)) (body x t t))
        :: branches
      else branches
    in
    let branches =
      if residues then
        G.map2 (fun r e -> Exp.Residue (r, e)) residue (child t) :: branches
      else branches
    in
    G.oneof_weighted [ (1, G.oneof leaves); (4, G.oneof branches) ]

let typed =
  G.sized_size (G.int_range 0 4) (fun depth ->
      G.bind (typ 2) (typed_exp ~fix:true ~residues:true depth []))

let terminating =
  G.sized_size (G.int_range 0 4) (fun depth ->
      typed_exp ~fix:false ~residues:false depth [] Int)

(* Rename binders independently, preserving the selected binder's occurrences.
   Reusing names can intentionally capture another variable. The oracle must
   distinguish these cases rather than assuming all renamings are equivalent. *)
let rec rename bound (e : Exp.t) =
  let go = rename bound in
  let binary make l r = G.map2 (fun l r -> make (l, r)) (go l) (go r) in
  match e with
  | Var x -> G.pure (Exp.Var (Option.value (List.assoc_opt x bound) ~default:x))
  | Int _ | Bool _ -> G.pure e
  | Fun (x, e) ->
      G.bind name (fun y ->
          G.map (fun e -> Exp.Fun (y, e)) (rename ((x, y) :: bound) e))
  | Fix (x, e) ->
      G.bind name (fun y ->
          G.map (fun e -> Exp.Fix (y, e)) (rename ((x, y) :: bound) e))
  | Eq (l, r) -> binary (fun (a, b) -> Exp.Eq (a, b)) l r
  | Add (l, r) -> binary (fun (a, b) -> Exp.Add (a, b)) l r
  | Sub (l, r) -> binary (fun (a, b) -> Exp.Sub (a, b)) l r
  | Mul (l, r) -> binary (fun (a, b) -> Exp.Mul (a, b)) l r
  | Ap (l, r) -> binary (fun (a, b) -> Exp.Ap (a, b)) l r
  | If (c, t, f) -> G.map3 (fun c t f -> Exp.If (c, t, f)) (go c) (go t) (go f)
  | Filter (f, e) -> G.map (fun e -> Exp.Filter (f, e)) (go e)
  | Residue (r, e) -> G.map (fun e -> Exp.Residue (r, e)) (go e)

let renamed =
  G.bind arbitrary (fun e -> G.map (fun other -> (e, other)) (rename [] e))
