open Syntax

type source = Fun | Fix
type bound = { pat : string; exp : string; source : source }

let rec index name = function
  | [] -> None
  | x :: _ when name x -> Some 0
  | _ :: xs -> Option.map (( + ) 1) (index name xs)

let matches pat exp =
  let rec go bound (pat : Pat.t) (exp : Exp.t) =
    match (pat, exp) with
    | _, (Filter (_, e) | Residue (_, e)) -> go bound pat e
    | Any, _ -> true
    | Var x, Var y -> (
        match
          (index (fun b -> b.pat = x) bound, index (fun b -> b.exp = y) bound)
        with
        | Some i, Some j -> i = j
        | None, None -> x = y
        | _ -> false)
    | Val, (Int _ | Fun _) -> true
    | Val, Var x -> (
        match List.find_opt (fun b -> b.exp = x) bound with
        | Some { source = Fun; _ } -> true
        | _ -> false)
    (* Deliberately follows MoonBit: Bool is a runtime value, but $v does not
       match boolean literals. See README for this compatibility detail. *)
    | Int x, Int y -> x = y
    | Bool x, Bool y -> x = y
    | Eq (p, q), Eq (e, f)
    | Add (p, q), Add (e, f)
    | Sub (p, q), Sub (e, f)
    | Mul (p, q), Mul (e, f)
    | Ap (p, q), Ap (e, f) ->
        go bound p e && go bound q f
    | Fun (x, p), Fun (y, e) ->
        go ({ pat = x; exp = y; source = Fun } :: bound) p e
    | Fix (x, p), Fix (y, e) ->
        go ({ pat = x; exp = y; source = Fix } :: bound) p e
    | If (p, q, r), If (c, t, f) -> go bound p c && go bound q t && go bound r f
    | _ -> false
  in
  go [] pat exp

let is_closed pat =
  let rec go bound = function
    | Pat.Any | Val | Int _ | Bool _ -> true
    | Var x -> List.mem x bound
    | Eq (l, r) | Add (l, r) | Sub (l, r) | Mul (l, r) | Ap (l, r) ->
        go bound l && go bound r
    | Fun (x, p) | Fix (x, p) -> go (x :: bound) p
    | If (c, t, f) -> go bound c && go bound t && go bound f
  in
  go [] pat
