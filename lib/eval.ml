type t =
  | Value of Expr.t
  | Indet of Expr.t

let string_of_t =
  function
  | Value expr -> Printf.sprintf "Value %s" (Expr.string_of_t expr)
  | Indet expr -> Printf.sprintf "Indet %s" (Expr.string_of_t expr)

let expr_of_t =
  function
  | Value value -> value
  | Indet expr -> expr

let rec eval (expr : Expr.t) =
  match expr with
  | Var var -> Indet (Var var)
  | Int int -> Value (Int int)
  | Add (e_l, e_r) ->
    begin match (eval e_l, eval e_r) with
    | Value (Int n_l), Value (Int n_r) -> Value (Int (n_l + n_r))
    | r_l, r_r ->
      let e_l = expr_of_t r_l in
      let e_r = expr_of_t r_r in
      Indet (Add (e_l, e_r))
    end
  | App (e_l, e_r) ->
    begin match (eval e_l, eval e_r) with
    | Value (Fun (x, e)), Value v_r ->
      eval (Subst.subst e x v_r)
    | r_l, r_r ->
      let e_l = expr_of_t r_l in
      let e_r = expr_of_t r_r in
      Indet (App (e_l, e_r))
    end
  | Fun (x, d) -> Value (Fun (x, d))
  | Fix (x, d) -> Value (Fun (x, Subst.subst d x d))
