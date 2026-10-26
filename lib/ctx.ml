open Syntax

(* Hole-first: each constructor records the next frame towards the root. *)
type t =
  | Top
  | EqL of t * Exp.t
  | EqR of Exp.t * t
  | AddL of t * Exp.t
  | AddR of Exp.t * t
  | SubL of t * Exp.t
  | SubR of Exp.t * t
  | MulL of t * Exp.t
  | MulR of Exp.t * t
  | ApL of t * Exp.t
  | ApR of Exp.t * t
  | If of t * Exp.t * Exp.t
  | Filter of filter * t
  | Residue of residue * t

let rec compose (ctx : t) (exp : Exp.t) : Exp.t =
  match ctx with
  | Top -> exp
  | EqL (c, r) -> compose c (Eq (exp, r))
  | EqR (l, c) -> compose c (Eq (l, exp))
  | AddL (c, r) -> compose c (Add (exp, r))
  | AddR (l, c) -> compose c (Add (l, exp))
  | SubL (c, r) -> compose c (Sub (exp, r))
  | SubR (l, c) -> compose c (Sub (l, exp))
  | MulL (c, r) -> compose c (Mul (exp, r))
  | MulR (l, c) -> compose c (Mul (l, exp))
  | ApL (c, r) -> compose c (Ap (exp, r))
  | ApR (l, c) -> compose c (Ap (l, exp))
  | If (c, t, f) -> compose c (If (exp, t, f))
  | Filter (flt, c) -> compose c (Filter (flt, exp))
  | Residue (residue, c) -> compose c (Residue (residue, exp))
