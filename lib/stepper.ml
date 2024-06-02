module Lexer = Lexer
module Parser = Parser
module Printer = Printer
module Forest = Forest
include Syntax

let parse (source : string) : Expr.t option =
  let buffer = Lexing.from_string source in
  Parser.top Lexer.lex buffer

module Error = struct
  type t = Unbound_variable | Mismatched_type | Not_a_value

  exception Error of t * Expr.t

  let to_string = function
    | Unbound_variable -> "Unbound variable"
    | Mismatched_type -> "Mismatch type"
    | Not_a_value -> "Not a value"
end

module Context = struct
  type t =
    | Top
    | Eq_l of t * Expr.t
    | Eq_r of Expr.t * t
    | And_l of t * Expr.t
    | And_r of Expr.t * t
    | Or_l of t * Expr.t
    | Or_r of Expr.t * t
    | Add_l of t * Expr.t
    | Add_r of Expr.t * t
    | Sub_l of t * Expr.t
    | Sub_r of Expr.t * t
    | Mul_l of t * Expr.t
    | Mul_r of Expr.t * t
    | Ap_l of t * Expr.t
    | Ap_r of Expr.t * t
    | If of t * Expr.t * Expr.t
    | Filter of Pat.t * Act.t * Gas.t * t
    | Residue of Act.t * Gas.t * int * t

  let take_prec = function
    | Top -> 0
    | Eq_l _ | Eq_r _ -> 3
    | And_l _ | And_r _ -> 2
    | Or_l _ | Or_r _ -> 1
    | Add_l _ | Add_r _ -> 4
    | Sub_l _ | Sub_r _ -> 4
    | Mul_l _ | Mul_r _ -> 5
    | Ap_l _ | Ap_r _ -> 6
    | If _ -> 0
    | Filter _ -> 0
    | Residue _ -> 0

  let rec pretty_print ?(residue = false) ?(prec = 0) ?expr (context : t) =
    let taken_prec = take_prec context in
    let pretty_print ?(residue = residue) ?(prec = taken_prec) =
      pretty_print ~residue ~prec ?expr
    in
    let expr_pretty_print ?(residue = residue) ?(prec = taken_prec) =
      Expr.pretty_print ~residue ~prec
    in
    let document =
      match context with
      | Top -> (
          match expr with
          | None -> PPrint.(string "@")
          | Some expr -> PPrint.(braces (expr_pretty_print expr)))
      | Eq_l (c, e) ->
          PPrint.(pretty_print c ^/^ string "==" ^/^ expr_pretty_print e)
      | Eq_r (e, c) ->
          PPrint.(expr_pretty_print e ^/^ string "==" ^/^ pretty_print c)
      | And_l (c, e) ->
          PPrint.(pretty_print c ^/^ string "&&" ^/^ expr_pretty_print e)
      | And_r (e, c) ->
          PPrint.(expr_pretty_print e ^/^ string "&&" ^/^ pretty_print c)
      | Or_l (c, e) ->
          PPrint.(pretty_print c ^/^ string "||" ^/^ expr_pretty_print e)
      | Or_r (e, c) ->
          PPrint.(expr_pretty_print e ^/^ string "||" ^/^ pretty_print c)
      | Add_l (c, e) ->
          PPrint.(
            pretty_print c ^/^ string "+"
            ^/^ expr_pretty_print ~prec:(taken_prec + 1) e)
      | Add_r (e, c) ->
          PPrint.(
            expr_pretty_print e ^/^ string "+"
            ^/^ pretty_print ~prec:(taken_prec + 1) c)
      | Sub_l (c, e) ->
          PPrint.(pretty_print c ^/^ string "-" ^/^ expr_pretty_print e)
      | Sub_r (e, c) ->
          PPrint.(expr_pretty_print e ^/^ string "-" ^/^ pretty_print c)
      | Mul_l (c, e) ->
          PPrint.(pretty_print c ^/^ string "*" ^/^ expr_pretty_print e)
      | Mul_r (e, c) ->
          PPrint.(expr_pretty_print e ^/^ string "*" ^/^ pretty_print c)
      | Ap_l (c, e) ->
          PPrint.(
            pretty_print c ^^ break 0
            ^^ PPrint.parens (expr_pretty_print ~prec:0 e))
      | Ap_r (e, c) ->
          PPrint.(
            expr_pretty_print e ^^ break 0
            ^^ PPrint.parens (pretty_print ~prec:0 c))
      | If (c, t, f) ->
          PPrint.(
            group (string "if" ^/^ pretty_print c ^/^ string "then")
            ^^ nest 2 (hardline ^^ expr_pretty_print t)
            ^^ hardline ^^ string "else"
            ^^ nest 2 (hardline ^^ expr_pretty_print f))
      | Filter (p, a, g, c) ->
          PPrint.(
            string (Syntax.to_keyword a g)
            ^/^ string (Pat.to_string p)
            ^/^ string "in" ^/^ pretty_print c)
      | Residue (a, g, l, c) ->
          if residue then
            let keyword = to_keyword a g in
            PPrint.(
              string keyword ^^ string "#"
              ^^ string (string_of_int l)
              ^^ string "[" ^^ break 0 ^^ pretty_print c ^^ break 0
              ^^ string "]")
          else pretty_print ~prec c
    in
    if taken_prec < prec then PPrint.(parens (nest 1 document))
    else PPrint.group document

  and to_string (context : t) =
    match context with
    | Top -> "@"
    | Eq_l (c, e) -> Printf.sprintf "%s = %s" (to_string c) (Expr.to_string e)
    | Eq_r (e, c) -> Printf.sprintf "%s = %s" (Expr.to_string e) (to_string c)
    | And_l (c, e) -> Printf.sprintf "%s && %s" (to_string c) (Expr.to_string e)
    | And_r (e, c) -> Printf.sprintf "%s && %s" (Expr.to_string e) (to_string c)
    | Or_l (c, e) -> Printf.sprintf "%s || %s" (to_string c) (Expr.to_string e)
    | Or_r (e, c) -> Printf.sprintf "%s || %s" (Expr.to_string e) (to_string c)
    | Add_l (c, e) -> Printf.sprintf "%s + %s" (to_string c) (Expr.to_string e)
    | Add_r (e, c) -> Printf.sprintf "%s + %s" (Expr.to_string e) (to_string c)
    | Sub_l (c, e) -> Printf.sprintf "%s - %s" (to_string c) (Expr.to_string e)
    | Sub_r (e, c) -> Printf.sprintf "%s - %s" (Expr.to_string e) (to_string c)
    | Mul_l (c, e) -> Printf.sprintf "%s * %s" (to_string c) (Expr.to_string e)
    | Mul_r (e, c) -> Printf.sprintf "%s * %s" (Expr.to_string e) (to_string c)
    | Ap_l (c, e) -> Printf.sprintf "%s(%s)" (to_string c) (Expr.to_string e)
    | Ap_r (e, c) -> Printf.sprintf "%s(%s)" (Expr.to_string e) (to_string c)
    | If (c, t, f) ->
        Printf.sprintf "if %s then %s else %s" (to_string c) (Expr.to_string t)
          (Expr.to_string f)
    | Filter (p, a, g, c) ->
        let keyword = Syntax.to_keyword a g in
        Printf.sprintf "%s %s in %s" keyword (Pat.to_string p) (to_string c)
    | Residue (a, g, l, c) ->
        let keyword = to_keyword a g in
        Printf.sprintf "%s #%d in %s" keyword l (to_string c)

  let rec decompose (exp : Expr.t) =
    match exp with
    | Var var -> (Top, Expr.Var var) :: []
    | Int _ | Bool _ | Fun _ -> []
    | Eq (e_l, e_r) ->
        let c_l = decompose e_l in
        let c_r = decompose e_r in
        let cs =
          List.map (fun (c, e_l') -> (Eq_l (c, e_r), e_l')) c_l
          @ List.map (fun (c, e_r') -> (Eq_r (e_l, c), e_r')) c_r
        in
        if cs = [] then (Top, Eq (e_l, e_r)) :: [] else cs
    | And (e_l, e_r) ->
        let c_l = decompose e_l in
        let c_r = decompose e_r in
        let cs =
          List.map (fun (c, e_l') -> (And_l (c, e_r), e_l')) c_l
          @ List.map (fun (c, e_r') -> (And_r (e_l, c), e_r')) c_r
        in
        if cs = [] then (Top, And (e_l, e_r)) :: [] else cs
    | Or (e_l, e_r) ->
        let c_l = decompose e_l in
        let c_r = decompose e_r in
        let cs =
          List.map (fun (c, e_l') -> (Or_l (c, e_r), e_l')) c_l
          @ List.map (fun (c, e_r') -> (Or_r (e_l, c), e_r')) c_r
        in
        if cs = [] then (Top, Or (e_l, e_r)) :: [] else cs
    | Add (e_l, e_r) ->
        let c_l = decompose e_l in
        let c_r = decompose e_r in
        let cs =
          List.map (fun (c, e_l') -> (Add_l (c, e_r), e_l')) c_l
          @ List.map (fun (c, e_r') -> (Add_r (e_l, c), e_r')) c_r
        in
        if cs = [] then (Top, Add (e_l, e_r)) :: [] else cs
    | Sub (e_l, e_r) ->
        let c_l =
          decompose e_l |> List.map @@ fun (c, e_l') -> (Sub_l (c, e_r), e_l')
        in
        let c_r =
          decompose e_r |> List.map @@ fun (c, e_r') -> (Sub_r (e_l, c), e_r')
        in
        let cs = c_l @ c_r in
        if cs = [] then (Top, Sub (e_l, e_r)) :: [] else cs
    | Mul (e_l, e_r) ->
        let c_l = decompose e_l in
        let c_r = decompose e_r in
        let cs =
          List.map (fun (c, e_l') -> (Mul_l (c, e_r), e_l')) c_l
          @ List.map (fun (c, e_r') -> (Mul_r (e_l, c), e_r')) c_r
        in
        if cs = [] then (Top, Mul (e_l, e_r)) :: [] else cs
    | Ap (e_l, e_r) ->
        let c_l = decompose e_l in
        let c_r = decompose e_r in
        let cs =
          List.map (fun (c, e_l') -> (Ap_l (c, e_r), e_l')) c_l
          @ List.map (fun (c, e_r') -> (Ap_r (e_l, c), e_r')) c_r
        in
        if cs = [] then (Top, Ap (e_l, e_r)) :: [] else cs
    | Fix (x, d) -> (Top, Expr.Fix (x, d)) :: []
    | If (e, t, f) ->
        let c = decompose e |> List.map (fun (c, e) -> (If (c, t, f), e)) in
        if c = [] then (Top, If (e, t, f)) :: [] else c
    | Filter (p, a, g, e) ->
        let c =
          decompose e |> List.map (fun (c, e) -> (Filter (p, a, g, c), e))
        in
        if c = [] then (Top, Filter (p, a, g, e)) :: [] else c
    | Residue (a, g, l, e) ->
        let c =
          decompose e |> List.map (fun (c, e) -> (Residue (a, g, l, c), e))
        in
        if c = [] then (Top, Residue (a, g, l, e)) :: [] else c

  let rec compose (ctx : t) (exp : Expr.t) =
    match ctx with
    | Top -> exp
    | Eq_l (c_l, e_r) -> Eq (compose c_l exp, e_r)
    | Eq_r (e_l, c_r) -> Eq (e_l, compose c_r exp)
    | And_l (c_l, e_r) -> And (compose c_l exp, e_r)
    | And_r (e_l, c_r) -> And (e_l, compose c_r exp)
    | Or_l (c_l, e_r) -> Or (compose c_l exp, e_r)
    | Or_r (e_l, c_r) -> Or (e_l, compose c_r exp)
    | Add_l (c_l, e_r) -> Add (compose c_l exp, e_r)
    | Add_r (e_l, c_r) -> Add (e_l, compose c_r exp)
    | Sub_l (c_l, e_r) -> Sub (compose c_l exp, e_r)
    | Sub_r (e_l, c_r) -> Sub (e_l, compose c_r exp)
    | Mul_l (c_l, e_r) -> Mul (compose c_l exp, e_r)
    | Mul_r (e_l, c_r) -> Mul (e_l, compose c_r exp)
    | Ap_l (c_l, e_r) -> Ap (compose c_l exp, e_r)
    | Ap_r (e_l, c_r) -> Ap (e_l, compose c_r exp)
    | If (e, t, f) -> If (compose e exp, t, f)
    | Filter (p, a, g, c) -> Filter (p, a, g, compose c exp)
    | Residue (a, g, l, c) -> Residue (a, g, l, compose c exp)
end

module Result = struct
  type 'a t = Value of Value.t | Expr of 'a
end

let rec transition (exp : Expr.t) : Expr.t Result.t =
  let error (error : Error.t) (expr : Expr.t) =
    raise (Error.Error (error, expr))
  in
  match exp with
  | Var var -> error Unbound_variable (Var var)
  | Int int -> Value (Int int)
  | Bool bool -> Value (Bool bool)
  | Eq (e_l, e_r) -> (
      match transition e_l with
      | Expr e_l -> Expr (Eq (e_l, e_r))
      | Value (Int n_l) -> (
          match transition e_r with
          | Expr e_r -> Expr (Eq (e_l, e_r))
          | Value (Int n_r) -> Expr (Bool (n_l = n_r))
          | Value e_r -> error Mismatched_type (Eq (e_l, Value.to_expr e_r)))
      | Value e_l -> error Mismatched_type (Eq (Value.to_expr e_l, e_r)))
  | And (e_l, e_r) -> (
      match transition e_l with
      | Expr e_l -> Expr (And (e_l, e_r))
      | Value (Bool b_l) -> (
          match transition e_r with
          | Expr e_r -> Expr (And (e_l, e_r))
          | Value (Bool b_r) -> Expr (Bool (b_l && b_r))
          | Value e_r -> error Mismatched_type (And (e_l, Value.to_expr e_r)))
      | Value e_l -> error Mismatched_type (And (Value.to_expr e_l, e_r)))
  | Or (e_l, e_r) -> (
      match transition e_l with
      | Expr e_l -> Expr (Or (e_l, e_r))
      | Value (Bool b_l) -> (
          match transition e_r with
          | Expr e_r -> Expr (Or (e_l, e_r))
          | Value (Bool b_r) -> Expr (Bool (b_l || b_r))
          | Value e_r -> error Mismatched_type (Or (e_l, Value.to_expr e_r)))
      | Value e_l -> error Mismatched_type (Or (Value.to_expr e_l, e_r)))
  | Add (e_l, e_r) -> (
      match transition e_l with
      | Expr e_l -> Expr (Add (e_l, e_r))
      | Value (Int n_l) -> (
          match transition e_r with
          | Expr e_r -> Expr (Add (e_l, e_r))
          | Value (Int n_r) -> Expr (Int (n_l + n_r))
          | Value e_r -> error Mismatched_type (Add (e_l, Value.to_expr e_r)))
      | Value e_l -> error Mismatched_type (Add (Value.to_expr e_l, e_r)))
  | Sub (e_l, e_r) -> (
      match transition e_l with
      | Expr e_l -> Expr (Sub (e_l, e_r))
      | Value (Int n_l) -> (
          match transition e_r with
          | Expr e_r -> Expr (Sub (e_l, e_r))
          | Value (Int n_r) -> Expr (Int (n_l - n_r))
          | Value e_r -> error Mismatched_type (Sub (e_l, Value.to_expr e_r)))
      | Value e_l -> error Mismatched_type (Sub (Value.to_expr e_l, e_r)))
  | Mul (e_l, e_r) -> (
      match transition e_l with
      | Expr e_l -> Expr (Mul (e_l, e_r))
      | Value (Int n_l) -> (
          match transition e_r with
          | Expr e_r -> Expr (Mul (e_l, e_r))
          | Value (Int n_r) -> Expr (Int (n_l * n_r))
          | Value e_r -> error Mismatched_type (Mul (e_l, Value.to_expr e_r)))
      | Value e_l -> error Mismatched_type (Mul (Value.to_expr e_l, e_r)))
  | Ap (e_l, e_r) -> (
      match transition e_l with
      | Expr e_l -> Expr (Ap (e_l, e_r))
      | Value (Fun (x, e_b)) -> (
          match transition e_r with
          | Expr e_r -> Expr (Ap (e_l, e_r))
          | Value e_r -> Expr (Expr.subst e_b x (Value.to_expr e_r)))
      | Value e_l -> error Mismatched_type (Ap (Value.to_expr e_l, e_r)))
  | Fun (x, e) -> Value (Fun (x, e))
  | Fix (x, Fun (y, e)) -> Value (Fun (y, Expr.subst e x (Fix (x, Fun (y, e)))))
  | Fix (x, e) -> error Mismatched_type (Fix (x, e))
  | If (e, t, f) -> (
      match transition e with
      | Expr e -> Expr (If (e, t, f))
      | Value (Bool true) -> Expr t
      | Value (Bool false) -> Expr f
      | Value _ -> error Mismatched_type (If (e, t, f)))
  | Filter (p, a, g, e) -> (
      match transition e with
      | Value value -> Expr (Value.to_expr value)
      | Expr exp -> Expr (Filter (p, a, g, exp)))
  | Residue (a, g, l, e) -> (
      match transition e with
      | Value exp -> Expr (Value.to_expr exp)
      | Expr exp -> Expr (Residue (a, g, l, exp)))

let rec eval (expr : Expr.t) : Value.t =
  let error (error : Error.t) (expr : Expr.t) = raise (Error.Error (error, expr)) in
  match expr with
  | Var var -> error Unbound_variable (Var var)
  | Int int -> Int int
  | Bool bool -> Bool bool
  | Eq (e_l, e_r) -> (
      match (eval e_l, eval e_r) with
      | Int n_l, Int n_r -> Bool (n_l = n_r)
      | _, _ -> error Mismatched_type expr)
  | And (e_l, e_r) -> (
      match (eval e_l, eval e_r) with
      | Bool b_l, Bool b_r -> Bool (b_l && b_r)
      | _, _ -> error Mismatched_type expr)
  | Or (e_l, e_r) -> (
      match (eval e_l, eval e_r) with
      | Bool b_l, Bool b_r -> Bool (b_l || b_r)
      | _, _ -> error Mismatched_type expr)
  | Add (e_l, e_r) -> (
      match (eval e_l, eval e_r) with
      | Int n_l, Int n_r -> Int (n_l + n_r)
      | _, _ -> error Mismatched_type expr)
  | Sub (e_l, e_r) -> (
      match (eval e_l, eval e_r) with
      | Int n_l, Int n_r -> Int (n_l - n_r)
      | _, _ -> error Mismatched_type expr)
  | Mul (e_l, e_r) -> (
      match (eval e_l, eval e_r) with
      | Int n_l, Int n_r -> Int (n_l * n_r)
      | _, _ -> error Mismatched_type expr)
  | Ap (e_l, e_r) -> (
      match (eval e_l, eval e_r) with
      | Fun (x, e), v_r -> eval (Expr.subst e x (Value.to_expr v_r))
      | _, _ -> error Mismatched_type expr)
  | Fun (x, e) -> Fun (x, e)
  | Fix (x, Fun (y, e)) -> Fun (y, Expr.subst e x (Fix (x, Fun (y, e))))
  | Fix (x, e) -> error Error.Mismatched_type (Fix (x, e))
  | If (e, t, f) -> (
      match eval e with
      | Bool true -> eval t
      | Bool false -> eval f
      | _ -> error Mismatched_type expr)
  | Filter (_, _, _, e) -> eval e
  | Residue (_, _, _, e) -> eval e

let rec instr (pat : Pat.t) (act : Act.t) (gas : Gas.t) (lvl : int)
    (exp : Expr.t) =
  let wrap exp =
    if Pat.matches pat exp then Expr.Residue (act, gas, lvl, exp) else exp
  in
  match transition exp with
  | Value _ -> exp
  | Expr _ -> (
      match exp with
      | Var var -> wrap (Var var)
      | Int int -> Int int
      | Bool bool -> Bool bool
      | Eq (e_l, e_r) ->
          let e_l = instr pat act gas lvl e_l in
          let e_r = instr pat act gas lvl e_r in
          wrap (Eq (e_l, e_r))
      | And (e_l, e_r) ->
          let e_l = instr pat act gas lvl e_l in
          let e_r = instr pat act gas lvl e_r in
          wrap (And (e_l, e_r))
      | Or (e_l, e_r) ->
          let e_l = instr pat act gas lvl e_l in
          let e_r = instr pat act gas lvl e_r in
          wrap (Or (e_l, e_r))
      | Add (e_l, e_r) ->
          let e_l = instr pat act gas lvl e_l in
          let e_r = instr pat act gas lvl e_r in
          wrap (Add (e_l, e_r))
      | Ap (e_l, e_r) ->
          let e_l = instr pat act gas lvl e_l in
          let e_r = instr pat act gas lvl e_r in
          wrap (Ap (e_l, e_r))
      | Sub (e_l, e_r) ->
          let e_l = instr pat act gas lvl e_l in
          let e_r = instr pat act gas lvl e_r in
          wrap (Sub (e_l, e_r))
      | Mul (e_l, e_r) ->
          let e_l = instr pat act gas lvl e_l in
          let e_r = instr pat act gas lvl e_r in
          wrap (Mul (e_l, e_r))
      | Fun (x, e) -> Fun (x, e)
      | Fix (x, e) -> wrap (Fix (x, e))
      | If (e, t, f) ->
          let e = instr pat act gas lvl e in
          let t = instr pat act gas lvl t in
          let f = instr pat act gas lvl f in
          wrap (If (e, t, f))
      | Filter (p, a, g, e) ->
          let e = instr pat act gas lvl e in
          Filter (p, a, g, instr p a g (lvl + 1) e)
      | Residue (a, g, l, e) ->
          let e = instr pat act gas lvl e in
          Residue (a, g, l, e))

let rec annot (act : Act.t) (lvl : int) (ctx : Context.t) =
  match ctx with
  | Top -> act
  | Eq_l (c, _) -> annot act lvl c
  | Eq_r (_, c) -> annot act lvl c
  | And_l (c, _) -> annot act lvl c
  | And_r (_, c) -> annot act lvl c
  | Or_l (c, _) -> annot act lvl c
  | Or_r (_, c) -> annot act lvl c
  | Add_l (c, _) -> annot act lvl c
  | Add_r (_, c) -> annot act lvl c
  | Sub_l (c, _) -> annot act lvl c
  | Sub_r (_, c) -> annot act lvl c
  | Mul_l (c, _) -> annot act lvl c
  | Mul_r (_, c) -> annot act lvl c
  | Ap_l (c, _) -> annot act lvl c
  | Ap_r (_, c) -> annot act lvl c
  | If (c, _, _) -> annot act lvl c
  | Filter (_, _, _, c) -> annot act lvl c
  | Residue (a, One, l, c) -> if l > lvl then annot a l c else annot act lvl c
  | Residue (a, All, l, c) -> if l > lvl then annot a l c else annot act lvl c

let rec decay (ctx : Context.t) =
  match ctx with
  | Top -> Context.Top
  | Eq_l (c, e) -> Eq_l (decay c, e)
  | Eq_r (e, c) -> Eq_r (e, decay c)
  | And_l (c, e) -> And_l (decay c, e)
  | And_r (e, c) -> And_r (e, decay c)
  | Or_l (c, e) -> Or_l (decay c, e)
  | Or_r (e, c) -> Or_r (e, decay c)
  | Add_l (c, e) -> Add_l (decay c, e)
  | Add_r (e, c) -> Add_r (e, decay c)
  | Sub_l (c, e) -> Sub_l (decay c, e)
  | Sub_r (e, c) -> Sub_r (e, decay c)
  | Mul_l (c, e) -> Mul_l (decay c, e)
  | Mul_r (e, c) -> Mul_r (e, decay c)
  | Ap_l (c, e) -> Ap_l (decay c, e)
  | Ap_r (e, c) -> Ap_r (e, decay c)
  | If (c, t, f) -> If (decay c, t, f)
  | Filter (p, a, g, c) -> Filter (p, a, g, decay c)
  | Residue (_, One, _, c) -> decay c
  | Residue (a, All, l, c) -> Residue (a, All, l, decay c)

let rec step (expr : Expr.t) : (Context.t * Expr.t) list Result.t =
  let instrumented = instr Any Pause One 0 expr in
  let decomposed = Context.decompose instrumented in
  let annotated =
    decomposed
    |> List.map @@ fun (ctx, expr) ->
       match expr with
       | Syntax.Expr.Filter _ | Syntax.Expr.Residue _ -> (Act.Eval, ctx, expr)
       | _ ->
           let act = annot Pause 0 ctx in
           (act, decay ctx, expr)
  in
  match List.find_opt (fun (act, _, _) -> act == Act.Eval) annotated with
  | None -> (
      match annotated |> List.map (fun (_, c, e) -> (c, e)) with
      | [] -> (
          match Expr.to_value expr with
          | None -> raise (Error.Error (Not_a_value, expr))
          | Some value -> Value value)
      | expr -> Expr expr)
  | Some (_, ctx, expr) -> (
      match transition expr with
      | Value value -> Value value
      | Expr exp -> step (Context.compose ctx exp))
