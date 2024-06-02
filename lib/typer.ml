module Expr = Syntax.Expr

module Type = struct
  type t = Int | Bool | Fun of t * t | Var of int

  let rec equal (t_l : t) (t_r : t) : bool =
    match (t_l, t_r) with
    | Int, Int -> true
    | Int, _ -> false
    | Bool, Bool -> true
    | Bool, _ -> false
    | Fun (x_l, e_l), Fun (x_r, e_r) -> equal x_l x_r && equal e_l e_r
    | Fun _, _ -> false
    | Var i_l, Var i_r -> Int.equal i_l i_r
    | Var _, _ -> false
end

module Context = struct
  type t = { var : (string, Type.t) Hashtbl.t; set : int Vector.t }

  let search (context : t) (var : string) : Type.t =
    Hashtbl.find context.var var

  let rec locate (context : t) (ty : Type.t) : Type.t =
    match ty with
    | Int -> Int
    | Bool -> Bool
    | Fun (t_x, t_e) -> Fun (locate context t_x, locate context t_e)
    | Var i ->
        let set = Vector.get context.set i in
        let ty = Hashtbl.find context.var var in
        let ty = locate context ty in
        Hashtbl.replace context.var var ty;
        ty

  let insert (context : t) (var : string) (ty : Type.t) : unit =
    Hashtbl.add context.var var ty

  let replace (context : t) (var : string) (ty : Type.t) : unit =
    Hashtbl.replace context.var var ty

  let remove (context : t) (var : string) : unit =
    Hashtbl.remove context.var var

  let length (context : t) : int = Hashtbl.length context.var
end

exception Mismatched_type of { expect : Type.t; actual : Type.t }

let rec unify (context : Context.t) (actual : Type.t) (expect : Type.t) : Type.t
    =
  match (actual, expect) with
  | Type.Var actual_idx, Type.Var expect_idx ->
      let actual_var = Vector.get context.idx actual_idx in
      let actual = Context.search context actual_var in
      let expect_var = Vector.get context.idx expect_idx in
      let expect = Context.search context expect_var in
      let ty = unify context actual expect in
      Context.replace context actual_var ty;
      Context.replace context expect_var ty;
      ty
  | actual, Type.Var idx ->
      let var = Vector.get context.idx idx in
      Context.replace context var actual;
      actual
  | Type.Var idx, expect ->
      let var = Vector.get context.idx idx in
      Context.replace context var expect;
      expect
  | Type.Int, Type.Int -> Type.Int
  | Type.Int, _ -> raise (Mismatched_type { actual; expect })
  | Type.Bool, Type.Bool -> Type.Bool
  | Type.Bool, _ -> raise (Mismatched_type { actual; expect })
  | Type.Fun (a_x, a_e), Type.Fun (e_x, e_e) ->
      let t_x = unify context a_x e_x in
      let t_e = unify context a_e e_e in
      Type.Fun (t_x, t_e)
  | Type.Fun _, _ -> raise (Mismatched_type { actual; expect })

let rec check (context : Context.t) (expr : Expr.t) (expect : Type.t) : unit =
  match expr with
  | Expr.Var _ | Expr.Int _ | Expr.Bool _
  | Expr.Eq (_, _)
  | Expr.And (_, _)
  | Expr.Or (_, _)
  | Expr.Add (_, _)
  | Expr.Sub (_, _)
  | Expr.Mul (_, _) ->
      let actual = infer context expr in
      if not (expect = actual) then raise (Mismatched_type { expect; actual })
  | Expr.Ap (e_l, e_r) ->
      let t_r = infer context e_r in
      check context e_l (Fun (t_r, expect))
  | Expr.Fun (_, _)
  | Expr.Fix (_, _)
  | Expr.If (_, _, _)
  | Expr.Filter (_, _, _, _)
  | Expr.Residue (_, _, _, _) ->
      ()

and infer (context : Context.t) (expr : Expr.t) : Type.t =
  match expr with
  | Expr.Var var -> Context.search context var
  | Expr.Int _ -> Type.Int
  | Expr.Bool _ -> Type.Bool
  | Expr.Eq (e_l, e_r) | Expr.And (e_l, e_r) | Expr.Or (e_l, e_r) ->
      check context e_l Bool;
      check context e_r Bool;
      Type.Bool
  | Expr.Add (e_l, e_r) | Expr.Sub (e_l, e_r) | Expr.Mul (e_l, e_r) ->
      check context e_l Int;
      check context e_r Int;
      Type.Int
  | Expr.Ap (e_l, e_r) -> (
      let t_l = infer context e_l in
      let t_r = infer context e_r in
      match infer context e_l with
      | Fun (t_x, t_e) ->
          check context e_r t_x;
          t_e
      | t_l ->
          let t_r = infer context e_r in
          let t_e = Type.Var (Context.length context) in
          raise (Mismatched_type { expect = Fun (t_r, t_e); actual = t_l }))
  | Expr.Fun (x, e) ->
      let t_x = Type.Var (Context.length context) in
      Context.insert context x t_x;
      let t = infer context e in
      Context.remove context x;
      t
  | Expr.Fix (x, e) ->
      let t_x = Type.Var (Context.length context) in
      Context.insert context x t_x;
      let t = infer context e in
      Context.remove context x;
      t
  | Expr.If (_, _, _) | Expr.Filter (_, _, _, _) | Expr.Residue (_, _, _, _) ->
      Var 0
