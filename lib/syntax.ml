module Act = struct
  type t = Eval | Pause

  let to_string = function Eval -> "eval" | Pause -> "pause"
end

module Gas = struct
  type t = One | All

  let to_string = function One -> "one" | All -> "all"
end

let to_keyword ?(short = false) (act : Act.t) (gas : Gas.t) =
  match (act, gas) with
  | Eval, One -> if short then "h" else "hide"
  | Eval, All -> if short then "e" else "eval"
  | Pause, One -> if short then "p" else "pause"
  | Pause, All -> if short then "d" else "debug"

module rec Pat : sig
  type t =
    | Any
    | Val
    | Var of string
    | Int of int
    | Bool of bool
    | Eq of t * t
    | And of t * t
    | Or of t * t
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Ap of t * t
    | Fun of string * Expr.t
    | Fun_any of Expr.t
    | If of t * t * t

  val to_string : t -> string
  val pretty_print : t -> PPrint.document
  val subst : t -> string -> Expr.t -> t
  val matches : t -> Expr.t -> bool
end = struct
  type t =
    | Any
    | Val
    | Var of string
    | Int of int
    | Bool of bool
    | Eq of t * t
    | And of t * t
    | Or of t * t
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Ap of t * t
    | Fun of string * Expr.t
    | Fun_any of Expr.t
    | If of t * t * t

  let rec to_string = function
    | Any -> "$e"
    | Val -> "$v"
    | Var var -> var
    | Int int -> string_of_int int
    | Bool bool -> string_of_bool bool
    | Eq (e_l, e_r) ->
        Printf.sprintf "(%s == %s)" (to_string e_l) (to_string e_r)
    | And (e_l, e_r) ->
        Printf.sprintf "(%s && %s)" (to_string e_l) (to_string e_r)
    | Or (e_l, e_r) ->
        Printf.sprintf "(%s || %s)" (to_string e_l) (to_string e_r)
    | Add (e_l, e_r) ->
        Printf.sprintf "(%s + %s)" (to_string e_l) (to_string e_r)
    | Sub (e_l, e_r) ->
        Printf.sprintf "(%s - %s)" (to_string e_l) (to_string e_r)
    | Mul (e_l, e_r) ->
        Printf.sprintf "(%s * %s)" (to_string e_l) (to_string e_r)
    | Ap (e_l, e_r) -> Printf.sprintf "%s(%s)" (to_string e_l) (to_string e_r)
    | Fun (x, e) -> Printf.sprintf "(fun %s -> %s)" x (Expr.to_string e)
    | Fun_any e -> Printf.sprintf "(fun $x -> %s)" (Expr.to_string e)
    | If (p, t, f) ->
        Printf.sprintf "(if %s then %s else %s)" (to_string p) (to_string t)
          (to_string f)

  let pretty_print (pat : t) : PPrint.document = PPrint.string (to_string pat)

  let rec subst (pat : t) (var : string) (value : Expr.t) =
    match pat with
    | Any -> Any
    | Val -> Val
    | Var pat_var -> if pat_var == var then Expr.to_pat value else Var pat_var
    | Int int -> Int int
    | Bool bool -> Bool bool
    | Eq (p_l, p_r) -> Eq (subst p_l var value, subst p_r var value)
    | And (p_l, p_r) -> And (subst p_l var value, subst p_r var value)
    | Or (p_l, p_r) -> Or (subst p_l var value, subst p_r var value)
    | Add (p_l, p_r) -> Add (subst p_l var value, subst p_r var value)
    | Sub (p_l, p_r) -> Sub (subst p_l var value, subst p_r var value)
    | Mul (p_l, p_r) -> Mul (subst p_l var value, subst p_r var value)
    | Ap (p_l, p_r) -> Ap (subst p_l var value, subst p_r var value)
    | Fun (x, e) -> Fun (x, Expr.subst e var value)
    | Fun_any e -> Fun_any (Expr.subst e var value)
    | If (p, t, f) ->
        If (subst p var value, subst t var value, subst f var value)

  let rec matches (pat : t) (exp : Expr.t) =
    match (pat, exp) with
    | pat, Residue (_, _, _, exp) -> matches pat exp
    | Any, _ -> true
    | Val, Int _ | Val, Fun _ -> true
    | Val, _ -> false
    | Var _, _ -> failwith ("Pattern " ^ to_string pat ^ " is value")
    | Int n_p, Int n_e -> n_p == n_e
    | Int _, _ -> false
    | Bool b_p, Bool b_e -> b_p == b_e
    | Bool _, _ -> false
    | Eq (p_l, p_r), Eq (e_l, e_r) -> matches p_l e_l && matches p_r e_r
    | Eq _, _ -> false
    | And (p_l, p_r), And (e_l, e_r) -> matches p_l e_l && matches p_r e_r
    | And _, _ -> false
    | Or (p_l, p_r), Or (e_l, e_r) -> matches p_l e_l && matches p_r e_r
    | Or _, _ -> false
    | Add (p_l, p_r), Add (e_l, e_r) -> matches p_l e_l && matches p_r e_r
    | Add _, _ -> false
    | Sub (p_l, p_r), Sub (e_l, e_r) -> matches p_l e_l && matches p_r e_r
    | Sub _, _ -> false
    | Mul (p_l, p_r), Mul (e_l, e_r) -> matches p_l e_l && matches p_r e_r
    | Mul _, _ -> false
    | Ap (p_l, p_r), Ap (e_l, e_r) -> matches p_l e_l && matches p_r e_r
    | Ap _, _ -> false
    | Fun (x_p, e_p), Fun (x_e, e_e) ->
        x_p == x_e && Expr.strip e_p == Expr.strip e_e
    | Fun _, _ -> false
    | Fun_any e_p, Fun (_, e_e) -> Expr.strip e_p == Expr.strip e_e
    | Fun_any _, _ -> false
    | If (p_p, p_t, p_f), If (e_p, e_t, e_f) ->
        matches p_p e_p && matches p_t e_t && matches p_f e_f
    | If _, _ -> false
end

and Expr : sig
  type t =
    | Var of string
    | Int of int
    | Bool of bool
    | Eq of t * t
    | And of t * t
    | Or of t * t
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Ap of t * t
    | Fun of string * t
    | Fix of string * t
    | If of t * t * t
    | Filter of Pat.t * Act.t * Gas.t * t
    | Residue of Act.t * Gas.t * int * t

  val pretty_print :
    ?short:bool -> ?residue:bool -> ?prec:int -> t -> PPrint.document

  val to_string : t -> string
  val to_pat : t -> Pat.t
  val to_value : t -> Value.t option
  val take_prec : ?residue:bool -> t -> int
  val strip : t -> t
  val subst : t -> string -> t -> t
end = struct
  type t =
    | Var of string
    | Int of int
    | Bool of bool
    | Eq of t * t
    | And of t * t
    | Or of t * t
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Ap of t * t
    | Fun of string * t
    | Fix of string * t
    | If of t * t * t
    | Filter of Pat.t * Act.t * Gas.t * t
    | Residue of Act.t * Gas.t * int * t

  let rec pretty_print ?(short = true) ?(residue = false) ?(prec = 0)
      (expr : Expr.t) =
    let taken_prec = Expr.take_prec ~residue expr in
    let pretty_print ?(residue = residue) ?(prec = taken_prec) =
      pretty_print ~residue ~prec
    in
    let prec_parens document =
      if taken_prec < prec then PPrint.(parens document)
      else PPrint.group document
    in
    match expr with
    | Var var -> PPrint.string var |> prec_parens
    | Int int -> PPrint.string (string_of_int int) |> prec_parens
    | Bool bool -> PPrint.string (string_of_bool bool) |> prec_parens
    | Eq (e_l, e_r) ->
        PPrint.(pretty_print e_l ^/^ string "==" ^/^ pretty_print e_r)
        |> prec_parens
    | And (e_l, e_r) ->
        PPrint.(pretty_print e_l ^/^ string "&&" ^/^ pretty_print e_r)
        |> prec_parens
    | Or (e_l, e_r) ->
        PPrint.(pretty_print e_l ^/^ string "||" ^/^ pretty_print e_r)
        |> prec_parens
    | Add (e_l, e_r) ->
        PPrint.(
          pretty_print e_l ^/^ string "+"
          ^/^ pretty_print ~prec:(taken_prec + 1) e_r)
        |> prec_parens
    | Sub (e_l, e_r) ->
        PPrint.(pretty_print e_l ^/^ string "-" ^/^ pretty_print e_r)
        |> prec_parens
    | Mul (e_l, e_r) ->
        PPrint.(pretty_print e_l ^/^ string "*" ^/^ pretty_print e_r)
        |> prec_parens
    | Ap (e_l, e_r) ->
        PPrint.(
          pretty_print e_l ^^ break 0 ^^ parens (pretty_print ~prec:0 e_r))
        |> prec_parens
    | Fun (x, e) ->
        PPrint.(
          group (string "fun" ^/^ string x ^/^ string "->")
          ^^ nest 2 (break 1 ^^ pretty_print e))
        |> prec_parens
    | Fix (x, e) ->
        PPrint.(
          group (string "fix" ^/^ string x ^/^ string "->")
          ^^ nest 2 (break 1 ^^ pretty_print e))
        |> prec_parens
    | If (p, t, f) ->
        PPrint.(
          string "if" ^/^ pretty_print p ^/^ string "then" ^/^ pretty_print t
          ^/^ string "else" ^/^ pretty_print f)
        |> prec_parens
    | Filter (p, a, g, e) ->
        let keyword = to_keyword a g in
        PPrint.(
          string keyword
          ^/^ string (Pat.to_string p)
          ^/^ string "in" ^/^ pretty_print e)
        |> prec_parens
    | Residue (a, g, l, e) ->
        if residue then
          let keyword = to_keyword ~short a g in
          PPrint.(
            string keyword ^^ string "#"
            ^^ string (string_of_int l)
            ^^ string "[" ^^ break 0 ^^ pretty_print ~prec:0 e ^^ break 0
            ^^ string "]")
          |> prec_parens
        else pretty_print ~prec e

  let rec to_string = function
    | Var var -> var
    | Int int -> string_of_int int
    | Bool bool -> string_of_bool bool
    | Eq (e_l, e_r) ->
        Printf.sprintf "(%s == %s)" (to_string e_l) (to_string e_r)
    | And (e_l, e_r) ->
        Printf.sprintf "(%s && %s)" (to_string e_l) (to_string e_r)
    | Or (e_l, e_r) ->
        Printf.sprintf "(%s || %s)" (to_string e_l) (to_string e_r)
    | Add (e_l, e_r) ->
        Printf.sprintf "(%s + %s)" (to_string e_l) (to_string e_r)
    | Sub (e_l, e_r) ->
        Printf.sprintf "(%s - %s)" (to_string e_l) (to_string e_r)
    | Mul (e_l, e_r) ->
        Printf.sprintf "(%s * %s)" (to_string e_l) (to_string e_r)
    | Ap (e_l, e_r) -> Printf.sprintf "%s(%s)" (to_string e_l) (to_string e_r)
    | Fun (x, e) -> Printf.sprintf "(fun %s -> %s)" x (to_string e)
    | Fix (x, e) -> Printf.sprintf "(fix %s -> %s)" x (to_string e)
    | If (p, t, f) ->
        Printf.sprintf "(if %s then %s else %s)" (to_string p) (to_string t)
          (to_string f)
    | Filter (p, a, g, e) ->
        let keyword = to_keyword a g in
        Printf.sprintf "(%s %s in %s)" keyword (Pat.to_string p) (to_string e)
    | Residue (a, g, l, e) ->
        let keyword = to_keyword a g in
        Printf.sprintf "(%s #%d in %s)" keyword l (to_string e)

  let rec to_pat = function
    | Var var -> Pat.Var var
    | Int int -> Pat.Int int
    | Bool bool -> Pat.Bool bool
    | Eq (e_l, e_r) -> Pat.(Eq (to_pat e_l, to_pat e_r))
    | And (e_l, e_r) -> Pat.(And (to_pat e_l, to_pat e_r))
    | Or (e_l, e_r) -> Pat.(Or (to_pat e_l, to_pat e_r))
    | Add (e_l, e_r) -> Pat.(Add (to_pat e_l, to_pat e_r))
    | Sub (e_l, e_r) -> Pat.(Sub (to_pat e_l, to_pat e_r))
    | Mul (e_l, e_r) -> Pat.(Mul (to_pat e_l, to_pat e_r))
    | Ap (e_l, e_r) -> Pat.(Ap (to_pat e_l, to_pat e_r))
    | Fun (x, e) -> Pat.Fun (x, e)
    | Fix (x, e) -> Pat.Fun (x, e)
    | If (p, t, f) -> Pat.If (to_pat p, to_pat t, to_pat f)
    | Filter (_, _, _, e) -> to_pat e
    | Residue (_, _, _, e) -> to_pat e

  let to_value (expr : t) =
    match expr with
    | Int int -> Some (Value.Int int)
    | Bool bool -> Some (Value.Bool bool)
    | Fun (x, e) -> Some (Value.Fun (x, e))
    | Var _ | Eq _ | And _ | Or _ | Add _ | Sub _ | Mul _ | Ap _ | Fix _ | If _
    | Filter _ | Residue _ ->
        None

  let rec take_prec ?(residue = false) (expr : t) =
    match expr with
    | Var _ | Int _ | Bool _ -> 7
    | Eq _ -> 3
    | And _ -> 2
    | Or _ -> 1
    | Add _ -> 4
    | Sub _ -> 4
    | Mul _ -> 5
    | Ap _ -> 6
    | Fun _ -> 0
    | Fix _ -> 0
    | If _ -> 0
    | Filter _ -> 0
    | Residue (_, _, _, e) -> if residue then 6 else take_prec e

  let rec strip = function
    | Filter (_, _, _, e) -> strip e
    | Residue (_, _, _, e) -> strip e
    | e -> e

  let rec subst (body : t) (var : string) (value : t) : t =
    match body with
    | Var body_var -> if body_var == var then value else Var body_var
    | Int int -> Int int
    | Bool bool -> Bool bool
    | Eq (d1, d2) ->
        let d1 = subst d1 var value in
        let d2 = subst d2 var value in
        Eq (d1, d2)
    | And (d1, d2) ->
        let d1 = subst d1 var value in
        let d2 = subst d2 var value in
        And (d1, d2)
    | Or (d1, d2) ->
        let d1 = subst d1 var value in
        let d2 = subst d2 var value in
        Or (d1, d2)
    | Add (d1, d2) ->
        let d1 = subst d1 var value in
        let d2 = subst d2 var value in
        Add (d1, d2)
    | Sub (d1, d2) ->
        let d1 = subst d1 var value in
        let d2 = subst d2 var value in
        Sub (d1, d2)
    | Mul (d1, d2) ->
        let d1 = subst d1 var value in
        let d2 = subst d2 var value in
        Mul (d1, d2)
    | Ap (d1, d2) ->
        let d1 = subst d1 var value in
        let d2 = subst d2 var value in
        Ap (d1, d2)
    | Fun (fun_var, body) ->
        if fun_var == var then Fun (fun_var, body)
        else Fun (fun_var, subst body var value)
    | Fix (fun_var, body) ->
        if fun_var == var then Fix (fun_var, body)
        else Fix (fun_var, subst body var value)
    | If (p, t, f) ->
        let p = subst p var value in
        let t = subst t var value in
        let f = subst f var value in
        If (p, t, f)
    | Filter (p, a, g, e) ->
        Filter (Pat.subst p var value, a, g, subst e var value)
    | Residue (a, g, l, e) -> Residue (a, g, l, subst e var value)
end

and Value : sig
  type t = Int of int | Bool of bool | Fun of string * Expr.t

  val pretty_print : ?prec:int -> t -> PPrint.document
  val to_string : t -> string
  val to_expr : t -> Expr.t
  val to_pat : t -> Pat.t
end = struct
  type t = Int of int | Bool of bool | Fun of string * Expr.t

  let take_prec (value : t) =
    match value with Int _ | Bool _ -> 7 | Fun _ -> 0

  let pretty_print ?(prec = 0) (value : t) =
    let taken_prec = take_prec value in
    let prec_parens document =
      if taken_prec < prec then PPrint.(parens (nest 1 document))
      else PPrint.group document
    in
    match value with
    | Int int -> PPrint.string (string_of_int int)
    | Bool bool -> PPrint.string (string_of_bool bool)
    | Fun (x, e) ->
        PPrint.(
          string "fun" ^/^ string x ^/^ string "->"
          ^/^ Expr.pretty_print ~prec:0 e)
        |> prec_parens

  let to_string (value : t) =
    match value with
    | Int int -> string_of_int int
    | Bool bool -> string_of_bool bool
    | Fun (x, e) -> Printf.sprintf "(fun %s -> %s)" x (Expr.to_string e)

  let to_expr (value : t) =
    match value with
    | Int int -> Expr.Int int
    | Bool bool -> Bool bool
    | Fun (x, e) -> Fun (x, e)

  let to_pat (value : t) =
    match value with
    | Int int -> Pat.Int int
    | Bool bool -> Pat.Bool bool
    | Fun (x, e) -> Pat.Fun (x, e)
end
