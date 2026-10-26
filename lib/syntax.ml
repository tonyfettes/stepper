module Act = struct
  type t = Eval | Pause
end

module Gas = struct
  type t = One | All
end

module Pat = struct
  type t =
    | Any
    | Val
    | Var of string
    | Int of int32
    | Bool of bool
    | Eq of t * t
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Ap of t * t
    | Fun of string * t
    | Fix of string * t
    | If of t * t * t
end

type filter = { pat : Pat.t; act : Act.t; gas : Gas.t }
type residue = { act : Act.t; gas : Gas.t; lvl : int }

module Exp = struct
  type t =
    | Var of string
    | Int of int32
    | Bool of bool
    | Eq of t * t
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Ap of t * t
    | Fun of string * t
    | Fix of string * t
    | If of t * t * t
    | Filter of filter * t
    | Residue of residue * t
end

let to_keyword act gas =
  match (act, gas) with
  | Act.Eval, Gas.One -> "hide"
  | Eval, All -> "eval"
  | Pause, One -> "stop"
  | Pause, All -> "step"

let is_value = function Exp.Int _ | Bool _ | Fun _ -> true | _ -> false

let rec strip ?(filters = true) (exp : Exp.t) : Exp.t =
  let go = strip ~filters in
  match exp with
  | Var _ | Int _ | Bool _ -> exp
  | Filter (flt, e) -> if filters then go e else Filter (flt, go e)
  | Residue (_, e) -> go e
  | Fun (x, e) -> Fun (x, go e)
  | Fix (x, e) -> Fix (x, go e)
  | Eq (l, r) -> Eq (go l, go r)
  | Add (l, r) -> Add (go l, go r)
  | Sub (l, r) -> Sub (go l, go r)
  | Mul (l, r) -> Mul (go l, go r)
  | Ap (l, r) -> Ap (go l, go r)
  | If (c, t, f) -> If (go c, go t, go f)
