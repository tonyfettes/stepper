type t =
  | Top
  | Add_l of t * Expr.t
  | Add_r of Expr.t * t
  | App_l of t * Expr.t
  | App_r of Expr.t * t

val decompose : Expr.t -> (t * Expr.t) list

val compose : t -> Expr.t -> Expr.t
