type t =
  | Int of int
  | Fun of string * Expr.t

let value_of_expr (expr : Expr.t) =
  match expr with
  | Var _
  | Add _
  | App _
  | Fix _ -> None
  | Int int -> Some (Int int)
  | Fun (x, d) -> Some (Fun (x, d))

let expr_of_value (value : t) =
  match value with
  | Int int -> Expr.Int int
  | Fun (x, d) -> Expr.Fun (x, d)
