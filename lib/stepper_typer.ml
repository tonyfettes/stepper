module Syntax = Stepper_syntax
module Expr = Syntax.Expr

module Type = struct
  type t = Int | Bool | Fun of t * t | Ref of t ref

  let rec find (t : t) : t =
    match t with
    | Int -> Int
    | Bool -> Bool
    | Fun (x, e) -> Fun (find x, find e)
    | Ref r when !r == t -> t
    | Ref r ->
        r := find !r;
        !r

  exception Mismatched of t * t

  let rec unify (l : t) (r : t) : t =
    match (find l, find r) with
    | Ref l, Ref r ->
        r := !l;
        !r
    | Ref l, _ ->
        l := r;
        r
    | _, Ref r ->
        r := l;
        l
    | Int, Int -> Int
    | Int, _ -> raise (Mismatched (l, r))
    | Bool, Bool -> Bool
    | Bool, _ -> raise (Mismatched (l, r))
    | Fun (x_l, e_l), Fun (x_r, e_r) -> Fun (unify x_l x_r, unify e_l e_r)
    | Fun _, _ -> raise (Mismatched (l, r))

  let rec to_string (t : t) : string =
    match t with
    | Int -> "Int"
    | Bool -> "Bool"
    | Fun (x, e) -> Printf.sprintf "(%s) -> %s" (to_string x) (to_string e)
    | Ref r when !r == t -> Printf.sprintf "&%d" (Obj.magic t)
    | Ref r -> to_string !r
end

exception Mismatched = Type.Mismatched

type t = (string, Type.t) Hashtbl.t

let create ~(capacity : int) : t = Hashtbl.create capacity
let lookup (context : t) (var : string) : Type.t = Hashtbl.find context var

let insert (context : t) (var : string) (ty : Type.t) : unit =
  Hashtbl.add context var ty

let delete (context : t) (var : string) : unit = Hashtbl.remove context var

let alloc () : Type.t =
  let rec ty = ref (Type.Ref ty) in
  !ty

let rec check (context : t) ~(expect : Type.t) (expr : Expr.t) : unit =
  match expr with
  | Expr.Var _ | Expr.Int _ | Expr.Bool _
  | Expr.Eq (_, _)
  | Expr.And (_, _)
  | Expr.Or (_, _)
  | Expr.Add (_, _)
  | Expr.Sub (_, _)
  | Expr.Mul (_, _) ->
      expr |> infer context |> Type.unify expect |> ignore
  | Expr.Ap (e_l, e_r) ->
      let t_r = infer context e_r in
      check context e_l ~expect:(Fun (t_r, expect))
  | Expr.Fun (_, _)
  | Expr.Fix (_, _)
  | Expr.If (_, _, _)
  | Expr.Filter (_, _, _, _)
  | Expr.Residue (_, _, _, _) ->
      ()

and infer (context : t) (expr : Expr.t) : Type.t =
  Printf.eprintf "infer: expr = %s\n" (Expr.to_string expr);
  let ty =
    match expr with
    | Expr.Var var ->
        Printf.eprintf "infer: looking up %s (%d)\n"
          (Type.to_string (lookup context var))
          (Obj.magic (lookup context var));
        lookup context var
    | Expr.Int _ -> Type.Int
    | Expr.Bool _ -> Type.Bool
    | Expr.Eq (e_l, e_r) | Expr.And (e_l, e_r) | Expr.Or (e_l, e_r) ->
        check context e_l ~expect:Bool;
        check context e_r ~expect:Bool;
        Type.Bool
    | Expr.Add (e_l, e_r) | Expr.Sub (e_l, e_r) | Expr.Mul (e_l, e_r) ->
        check context e_l ~expect:Int;
        check context e_r ~expect:Int;
        Type.Int
    | Expr.Ap (e_l, e_r) ->
        let t_l = infer context e_l in
        let t_r = infer context e_r in
        let t_e = alloc () in
        Type.unify (Fun (t_r, t_e)) t_l |> ignore;
        Type.find t_e
    | Expr.Fun (x, e) ->
        let t_x = alloc () in
        Printf.eprintf "infer: opening %s (%d)\n" (Type.to_string t_x)
          (Obj.magic t_x);
        insert context x t_x;
        let t_e = infer context e in
        delete context x;
        Fun (t_x, t_e)
    | Expr.Fix (x, e) ->
        let t_x = alloc () in
        insert context x t_x;
        let t_e = infer context e in
        delete context x;
        t_e
    | Expr.If (c, t, f) ->
        check context c ~expect:Bool;
        let t_t = infer context t in
        let t_f = infer context f in
        Type.unify t_t t_f
    | Expr.Filter (_, _, _, e) | Expr.Residue (_, _, _, e) -> infer context e
  in
  Printf.eprintf "infer: expr = %s, ty = %s\n" (Expr.to_string expr)
    (Type.to_string ty);
  ty
