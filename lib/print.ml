open Syntax

(* Fully parenthesized output makes candidate contexts and scope unambiguous. *)
let rec pat = function
  | Pat.Any -> "$e"
  | Val -> "$v"
  | Var x -> x
  | Int n -> Int32.to_string n
  | Bool b -> string_of_bool b
  | Eq (l, r) -> binary "==" (pat l) (pat r)
  | Add (l, r) -> binary "+" (pat l) (pat r)
  | Sub (l, r) -> binary "-" (pat l) (pat r)
  | Mul (l, r) -> binary "*" (pat l) (pat r)
  | Ap (l, r) -> Printf.sprintf "(%s(%s))" (pat l) (pat r)
  | Fun (x, p) -> Printf.sprintf "(fun %s -> %s)" x (pat p)
  | Fix (x, p) -> Printf.sprintf "(fix %s -> %s)" x (pat p)
  | If (c, t, f) -> conditional (pat c) (pat t) (pat f)

and binary op l r = Printf.sprintf "(%s %s %s)" l op r
and conditional c t f = Printf.sprintf "(if %s then %s else %s)" c t f

let filter (flt : filter) body =
  Printf.sprintf "(debug %s(%s) in %s)"
    (to_keyword flt.act flt.gas)
    (pat flt.pat) body

let residue (r : residue) body =
  Printf.sprintf "%s#%d[%s]" (to_keyword r.act r.gas) r.lvl body

let rec exp = function
  | Exp.Var x -> x
  | Int n -> Int32.to_string n
  | Bool b -> string_of_bool b
  | Eq (l, r) -> binary "==" (exp l) (exp r)
  | Add (l, r) -> binary "+" (exp l) (exp r)
  | Sub (l, r) -> binary "-" (exp l) (exp r)
  | Mul (l, r) -> binary "*" (exp l) (exp r)
  | Ap (l, r) -> Printf.sprintf "(%s(%s))" (exp l) (exp r)
  | Fun (x, e) -> Printf.sprintf "(fun %s -> %s)" x (exp e)
  | Fix (x, e) -> Printf.sprintf "(fix %s -> %s)" x (exp e)
  | If (c, t, f) -> conditional (exp c) (exp t) (exp f)
  | Filter (flt, e) -> filter flt (exp e)
  | Residue (r, e) -> residue r (exp e)

let ctx context =
  let rec go context hole =
    match context with
    | Ctx.Top -> hole
    | EqL (c, r) -> go c (binary "==" hole (exp r))
    | EqR (l, c) -> go c (binary "==" (exp l) hole)
    | AddL (c, r) -> go c (binary "+" hole (exp r))
    | AddR (l, c) -> go c (binary "+" (exp l) hole)
    | SubL (c, r) -> go c (binary "-" hole (exp r))
    | SubR (l, c) -> go c (binary "-" (exp l) hole)
    | MulL (c, r) -> go c (binary "*" hole (exp r))
    | MulR (l, c) -> go c (binary "*" (exp l) hole)
    | ApL (c, r) -> go c (Printf.sprintf "(%s(%s))" hole (exp r))
    | ApR (l, c) -> go c (Printf.sprintf "(%s(%s))" (exp l) hole)
    | If (c, t, f) -> go c (conditional hole (exp t) (exp f))
    | Filter (flt, c) -> go c (filter flt hole)
    | Residue (r, c) -> go c (residue r hole)
  in
  go context "@"
