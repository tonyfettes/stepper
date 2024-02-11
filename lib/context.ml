type t =
  | Top
  | Add_l of t * Expr.t
  | Add_r of Expr.t * t
  | App_l of t * Expr.t
  | App_r of Expr.t * t

let rec decompose (expr : Expr.t) =
  match expr with
  | Var var -> (Top, Expr.Var var) :: []
  | Int _ -> []
  | Add (e_l, e_r) ->
    let c_l = decompose e_l in
    let c_r = decompose e_r in
    List.map (fun (c, e_l') -> (Add_l (c, e_r), e_l')) c_l
    @ List.map (fun (c, e_r') -> (Add_r (e_l, c), e_r')) c_r
  | App (e_l, e_r) ->
    let c_l = decompose e_l in
    let c_r = decompose e_r in
    List.map (fun (c, e_l') -> (App_l (c, e_r), e_l')) c_l
    @ List.map (fun (c, e_r') -> (App_r (e_l, c), e_r')) c_r
  | Fun _ -> []
  | Fix (x, d) -> (Top, Expr.Fix (x, d)) :: []

let rec compose (context : t) (expr : Expr.t) =
  match context with
  | Top -> expr
  | Add_l (c_l, e_r) ->
    Add (compose c_l expr, e_r)
  | Add_r (e_l, c_r) ->
    Add (e_l, compose c_r expr)
  | App_l (c_l, e_r) ->
    App (compose c_l expr, e_r)
  | App_r (e_l, c_r) ->
    App (e_l, compose c_r expr)
