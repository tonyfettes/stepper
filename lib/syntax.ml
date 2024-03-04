module Act = struct
  type t = Eval | Pause

  let to_string = function Eval -> "eval" | Pause -> "pause"
end

module Gas = struct
  type t = One | All

  let to_string = function One -> "one" | All -> "all"
end

let to_keyword (act : Act.t) (gas : Gas.t) =
  match (act, gas) with
  | Eval, One -> "hide"
  | Eval, All -> "eval"
  | Pause, One -> "pause"
  | Pause, All -> "debug"

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
    | Fun of string * Exp.t
    | Fun_any of Exp.t
    | If of t * t * t

  val to_string : t -> string
  val subst : t -> string -> Exp.t -> t
  val matches : t -> Exp.t -> bool
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
    | Fun of string * Exp.t
    | Fun_any of Exp.t
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
    | Fun (x, e) -> Printf.sprintf "(fun %s -> %s)" x (Exp.to_string e)
    | Fun_any e -> Printf.sprintf "(fun $x -> %s)" (Exp.to_string e)
    | If (p, t, f) ->
        Printf.sprintf "(if %s then %s else %s)" (to_string p) (to_string t)
          (to_string f)

  let rec subst (pat : t) (var : string) (exp : Exp.t) =
    match pat with
    | Any -> Any
    | Val -> Val
    | Var pat_var -> if pat_var == var then Exp.to_pat exp else Var pat_var
    | Int int -> Int int
    | Bool bool -> Bool bool
    | Eq (p_l, p_r) -> Eq (subst p_l var exp, subst p_r var exp)
    | And (p_l, p_r) -> And (subst p_l var exp, subst p_r var exp)
    | Or (p_l, p_r) -> Or (subst p_l var exp, subst p_r var exp)
    | Add (p_l, p_r) -> Add (subst p_l var exp, subst p_r var exp)
    | Sub (p_l, p_r) -> Sub (subst p_l var exp, subst p_r var exp)
    | Mul (p_l, p_r) -> Mul (subst p_l var exp, subst p_r var exp)
    | Ap (p_l, p_r) -> Ap (subst p_l var exp, subst p_r var exp)
    | Fun (x, e) -> Fun (x, Exp.subst e var exp)
    | Fun_any e -> Fun_any (Exp.subst e var exp)
    | If (p, t, f) -> If (subst p var exp, subst t var exp, subst f var exp)

  let rec matches (pat : t) (exp : Exp.t) =
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
    | Fun (x_p, e_p), Fun (x_e, e_e) -> x_p == x_e && Exp.eq e_p e_e
    | Fun _, _ -> false
    | Fun_any e_p, Fun (_, e_e) -> Exp.eq e_p e_e
    | Fun_any _, _ -> false
    | If (p_p, p_t, p_f), If (e_p, e_t, e_f) ->
        matches p_p e_p && matches p_t e_t && matches p_f e_f
    | If _, _ -> false
end

and Exp : sig
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

  val to_string : ?residue:bool -> t -> string
  val to_pat : t -> Pat.t
  val eq : t -> t -> bool
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

  let rec to_string ?(residue = false) (exp : Exp.t) =
    let to_string = to_string ~residue in
    match exp with
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
          Printf.sprintf "(%s %s in %s)" keyword (Pat.to_string p)
            (to_string e)
    | Residue (a, g, l, e) ->
        if residue then
          Printf.sprintf "(do %s for %s at %d in %s)" (Act.to_string a)
            (Gas.to_string g) l (to_string e)
        else to_string e

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

  let rec eq (this : t) (that : t) =
    match (this, that) with
    | Var this, Var that -> this == that
    | Var _, _ -> false
    | Int this, Int that -> this == that
    | Int _, _ -> false
    | Bool this, Bool that -> this == that
    | Bool _, _ -> false
    | Eq (this_l, this_r), Eq (that_l, that_r) ->
        eq this_l that_l && eq this_r that_r
    | Eq _, _ -> false
    | And (this_l, this_r), And (that_l, that_r) ->
        eq this_l that_l && eq this_r that_r
    | And _, _ -> false
    | Or (this_l, this_r), Or (that_l, that_r) ->
        eq this_l that_l && eq this_r that_r
    | Or _, _ -> false
    | Add (this_l, this_r), Add (that_l, that_r) ->
        eq this_l that_l && eq this_r that_r
    | Add _, _ -> false
    | Sub (this_l, this_r), Sub (that_l, that_r) ->
        eq this_l that_l && eq this_r that_r
    | Sub _, _ -> false
    | Mul (this_l, this_r), Mul (that_l, that_r) ->
        eq this_l that_l && eq this_r that_r
    | Mul _, _ -> false
    | Ap (this_l, this_r), Ap (that_l, that_r) ->
        eq this_l that_l && eq this_r that_r
    | Ap _, _ -> false
    | Fun (this_x, this_e), Fun (that_x, that_e) ->
        this_x == that_x && eq this_e that_e
    | Fun _, _ -> false
    | Fix (this_x, this_e), Fix (that_x, that_e) ->
        this_x == that_x && eq this_e that_e
    | Fix _, _ -> false
    | If (this_p, this_t, this_f), If (that_p, that_t, that_f) ->
        eq this_p that_p && eq this_t that_t && eq this_f that_f
    | If _, _ -> false
    | ( (Filter (_, _, _, this) | Residue (_, _, _, this)),
        (Filter (_, _, _, that) | Residue (_, _, _, that)) ) ->
        eq this that
    | (Filter _ | Residue _), _ -> false

  let rec subst (body : t) (var : string) (expr : Exp.t) =
    match body with
    | Var body_var -> if body_var == var then expr else Var body_var
    | Int int -> Int int
    | Bool bool -> Bool bool
    | Eq (d1, d2) ->
        let d1 = subst d1 var expr in
        let d2 = subst d2 var expr in
        Eq (d1, d2)
    | And (d1, d2) ->
        let d1 = subst d1 var expr in
        let d2 = subst d2 var expr in
        And (d1, d2)
    | Or (d1, d2) ->
        let d1 = subst d1 var expr in
        let d2 = subst d2 var expr in
        Or (d1, d2)
    | Add (d1, d2) ->
        let d1 = subst d1 var expr in
        let d2 = subst d2 var expr in
        Add (d1, d2)
    | Sub (d1, d2) ->
        let d1 = subst d1 var expr in
        let d2 = subst d2 var expr in
        Sub (d1, d2)
    | Mul (d1, d2) ->
        let d1 = subst d1 var expr in
        let d2 = subst d2 var expr in
        Mul (d1, d2)
    | Ap (d1, d2) ->
        let d1 = subst d1 var expr in
        let d2 = subst d2 var expr in
        Ap (d1, d2)
    | Fun (fun_var, body) ->
        if fun_var == var then Fun (fun_var, body)
        else Fun (fun_var, subst body var expr)
    | Fix (fun_var, body) ->
        if fun_var == var then Fix (fun_var, body)
        else Fix (fun_var, subst body var expr)
    | If (p, t, f) ->
        let p = subst p var expr in
        let t = subst t var expr in
        let f = subst f var expr in
        If (p, t, f)
    | Filter (p, a, g, e) ->
        Filter (Pat.subst p var expr, a, g, subst e var expr)
    | Residue (a, g, l, e) -> Residue (a, g, l, subst e var expr)
end

and Val : sig
  type t = Int of int | Fun of string * Exp.t

  val to_string : t -> string
  val to_exp : t -> Exp.t
end = struct
  type t = Int of int | Fun of string * Exp.t

  let to_string (value : t) =
    match value with
    | Int int -> string_of_int int
    | Fun (x, e) -> Printf.sprintf "(fun %s -> %s)" x (Exp.to_string e)

  let to_exp (value : t) =
    match value with Int int -> Exp.Int int | Fun (x, e) -> Fun (x, e)
end
