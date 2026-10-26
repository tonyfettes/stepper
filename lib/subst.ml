open Syntax

let rec to_pat (exp : Exp.t) : Pat.t =
  match exp with
  | Var x -> Var x
  | Int n -> Int n
  | Bool b -> Bool b
  | Eq (l, r) -> Eq (to_pat l, to_pat r)
  | Add (l, r) -> Add (to_pat l, to_pat r)
  | Sub (l, r) -> Sub (to_pat l, to_pat r)
  | Mul (l, r) -> Mul (to_pat l, to_pat r)
  | Ap (l, r) -> Ap (to_pat l, to_pat r)
  | Fun (x, e) -> Fun (x, to_pat e)
  | Fix (x, e) -> Fix (x, to_pat e)
  | If (c, t, f) -> If (to_pat c, to_pat t, to_pat f)
  | Filter (_, e) | Residue (_, e) -> to_pat e

let rec pat (body : Pat.t) x exp : Pat.t =
  let go p = pat p x exp in
  match body with
  | Any | Val | Int _ | Bool _ -> body
  | Var y -> if y = x then to_pat exp else body
  | Eq (l, r) -> Eq (go l, go r)
  | Add (l, r) -> Add (go l, go r)
  | Sub (l, r) -> Sub (go l, go r)
  | Mul (l, r) -> Mul (go l, go r)
  | Ap (l, r) -> Ap (go l, go r)
  | Fun (y, p) -> if y = x then body else Fun (y, go p)
  | Fix (y, p) -> if y = x then body else Fix (y, go p)
  | If (c, t, f) -> If (go c, go t, go f)

(* Like MoonBit, substitution relies on closed replacement expressions during
   evaluation. It stops at shadowing binders, including inside patterns. *)
let rec exp (body : Exp.t) x replacement : Exp.t =
  let go e = exp e x replacement in
  match body with
  | Int _ | Bool _ -> body
  | Var y -> if y = x then replacement else body
  | Eq (l, r) -> Eq (go l, go r)
  | Add (l, r) -> Add (go l, go r)
  | Sub (l, r) -> Sub (go l, go r)
  | Mul (l, r) -> Mul (go l, go r)
  | Ap (l, r) -> Ap (go l, go r)
  | Fun (y, e) -> if y = x then body else Fun (y, go e)
  | Fix (y, e) -> if y = x then body else Fix (y, go e)
  | If (c, t, f) -> If (go c, go t, go f)
  | Filter (flt, e) ->
      Filter ({ flt with pat = pat flt.pat x replacement }, go e)
  | Residue (residue, e) -> Residue (residue, go e)
