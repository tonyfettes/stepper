type t =
  | Var of string
  | Int of int
  | Add of t * t
  | App of t * t
  | Fun of string * t
  | Fix of string * t

val string_of_t : t -> string
