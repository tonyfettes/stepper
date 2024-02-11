let rec subst (body : Expr.t) (var : string) (expr : Expr.t) =
  match body with
  | Var body_var ->
    if body_var == var then
      expr
    else
      Var body_var
  | Int int -> Int int
  | Add (d1 , d2) ->
    let d1 = subst d1 var expr in
    let d2 = subst d2 var expr in
    Add (d1, d2)
  | App (d1, d2) ->
    let d1 = subst d1 var expr in
    let d2 = subst d2 var expr in
    App (d1, d2)
  | Fun (fun_var, body)
  | Fix (fun_var, body) ->
    if fun_var == var then
      Fun (fun_var, body)
    else
      Fun (fun_var, subst body var expr)

