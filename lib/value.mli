type t =
  | Int of int
  | Fun of string * Expr.t

val value_of_expr : Expr.t -> t option

val expr_of_value : t -> Expr.t
