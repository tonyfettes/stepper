type t =
  | Value of Expr.t
  | Indet of Expr.t

val eval : Expr.t -> t

val string_of_t : t -> string
