module Act = struct
  type t = Skip | Pause

  let to_string = function Skip -> "skip" | Pause -> "pause"
end

module Gas = struct
  type t = One | All

  let to_string = function One -> "one" | All -> "all"
end

module rec Pat : sig
  type t =
    | Any
    | Val
    | Int of int
    | Add of t * t
    | App of t * t
    | Fun of string * Exp.t

  val to_string : t -> string
  val subst : t -> string -> Exp.t -> t
  val matches : t -> Exp.t -> bool
end = struct
  type t =
    | Any
    | Val
    | Int of int
    | Add of t * t
    | App of t * t
    | Fun of string * Exp.t

  let rec to_string = function
    | Any -> "$e"
    | Val -> "$v"
    | Int int -> string_of_int int
    | Add (e_l, e_r) ->
        Printf.sprintf "(%s + %s)" (to_string e_l) (to_string e_r)
    | App (e_l, e_r) -> Printf.sprintf "(%s %s)" (to_string e_l) (to_string e_r)
    | Fun (x, e) -> Printf.sprintf "(fun %s -> %s)" x (Exp.to_string e)

  let rec subst (pat : t) (var : string) (exp : Exp.t) =
    match pat with
    | Any -> Any
    | Val -> Val
    | Int int -> Int int
    | Add (p_l, p_r) -> Add (subst p_l var exp, subst p_r var exp)
    | App (p_l, p_r) -> App (subst p_l var exp, subst p_r var exp)
    | Fun (x, e) -> Fun (x, Exp.subst e var exp)

  let rec matches (pat : t) (exp : Exp.t) =
    match (pat, exp) with
    | Any, _ -> true
    | Val, Int _ | Val, Fun _ -> true
    | Val, _ -> false
    | Int n_p, Int n_e -> n_p == n_e
    | Int _, _ -> false
    | Add (p_l, p_r), Add (e_l, e_r) -> matches p_l e_l && matches p_r e_r
    | Add _, _ -> false
    | App (p_l, p_r), App (e_l, e_r) -> matches p_l e_l && matches p_r e_r
    | App _, _ -> false
    | Fun (x_p, e_p), Fun (x_e, e_e) -> x_p == x_e && Exp.eq e_p e_e
    | Fun _, _ -> false
end

and Exp : sig
  type t =
    | Var of string
    | Int of int
    | Add of t * t
    | App of t * t
    | Fun of string * t
    | Fix of string * t
    | Filter of Pat.t * Act.t * Gas.t * t
    | Residue of Act.t * Gas.t * t

  val to_string : t -> string
  val eq : t -> t -> bool
  val subst : t -> string -> t -> t
end = struct
  type t =
    | Var of string
    | Int of int
    | Add of t * t
    | App of t * t
    | Fun of string * t
    | Fix of string * t
    | Filter of Pat.t * Act.t * Gas.t * t
    | Residue of Act.t * Gas.t * t

  let rec to_string = function
    | Var var -> var
    | Int int -> string_of_int int
    | Add (e_l, e_r) ->
        Printf.sprintf "(%s + %s)" (to_string e_l) (to_string e_r)
    | App (e_l, e_r) -> Printf.sprintf "(%s %s)" (to_string e_l) (to_string e_r)
    | Fun (x, e) -> Printf.sprintf "(fun %s -> %s)" x (to_string e)
    | Fix (x, e) -> Printf.sprintf "(fix %s -> %s)" x (to_string e)
    | Filter (p, a, g, e) ->
        Printf.sprintf "(filter %s do %s for %s in %s)" (Pat.to_string p)
          (Act.to_string a) (Gas.to_string g) (to_string e)
    | Residue (a, g, e) ->
        Printf.sprintf "(do %s for %s in %s)" (Act.to_string a)
          (Gas.to_string g) (to_string e)

  let rec eq (this : t) (that : t) =
    match (this, that) with
    | Var this, Var that -> this == that
    | Var _, _ -> false
    | Int this, Int that -> this == that
    | Int _, _ -> false
    | Add (this_l, this_r), Add (that_l, that_r) ->
        eq this_l that_l && eq this_r that_r
    | Add _, _ -> false
    | App (this_l, this_r), App (that_l, that_r) ->
        eq this_l that_l && eq this_r that_r
    | App _, _ -> false
    | Fun (this_x, this_e), Fun (that_x, that_e) ->
        this_x == that_x && eq this_e that_e
    | Fun _, _ -> false
    | Fix (this_x, this_e), Fix (that_x, that_e) ->
        this_x == that_x && eq this_e that_e
    | Fix _, _ -> false
    | ( (Filter (_, _, _, this) | Residue (_, _, this)),
        (Filter (_, _, _, that) | Residue (_, _, that)) ) ->
        eq this that
    | (Filter _ | Residue _), _ -> false

  let rec subst (body : t) (var : string) (expr : Exp.t) =
    match body with
    | Var body_var -> if body_var == var then expr else Var body_var
    | Int int -> Int int
    | Add (d1, d2) ->
        let d1 = subst d1 var expr in
        let d2 = subst d2 var expr in
        Add (d1, d2)
    | App (d1, d2) ->
        let d1 = subst d1 var expr in
        let d2 = subst d2 var expr in
        App (d1, d2)
    | Fun (fun_var, body) | Fix (fun_var, body) ->
        if fun_var == var then Fun (fun_var, body)
        else Fun (fun_var, subst body var expr)
    | Filter (p, a, g, e) -> Filter (p, a, g, subst e var expr)
    | Residue (a, g, e) -> Residue (a, g, subst e var expr)
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

module Err = struct
  type t = Unbound_variable | Mismatched_type

  let to_string = function
    | Unbound_variable -> "Unbound variable"
    | Mismatched_type -> "Mismatch type"
end

module Result = struct
  let to_string = function
    | `Val exp -> Printf.sprintf "Val %s" (Exp.to_string exp)
    | `Exp exp -> Printf.sprintf "Exp %s" (Exp.to_string exp)
    | `Err (err, exp) ->
        Printf.sprintf "Err %s: %s" (Err.to_string err) (Exp.to_string exp)

  let rec transition (exp : Exp.t) =
    match exp with
    | Var var -> `Err (Err.Unbound_variable, Exp.Var var)
    | Int int -> `Val (Exp.Int int)
    | Add (e_l, e_r) -> (
        match transition e_l with
        | `Err (err, exp) -> `Err (err, exp)
        | `Exp e_l -> `Exp (Exp.Add (e_l, e_r))
        | `Val (Int n_l) -> (
            match transition e_r with
            | `Err (err, exp) -> `Err (err, exp)
            | `Exp e_r -> `Exp (Add (e_l, e_r))
            | `Val (Int n_r) -> `Exp (Int (n_l + n_r))
            | `Val e_r -> `Err (Mismatched_type, Add (e_l, e_r)))
        | `Val e_l -> `Err (Mismatched_type, Add (e_l, e_r)))
    | App (e_l, e_r) -> (
        match transition e_l with
        | `Err (err, exp) -> `Err (err, exp)
        | `Exp e_l -> `Exp (App (e_l, e_r))
        | `Val (Fun (x, e)) -> (
            match transition e_r with
            | `Err (err, exp) -> `Err (err, exp)
            | `Exp e_r -> `Exp (App (e_l, e_r))
            | `Val e_r -> `Exp (Exp.subst e_l x e_r))
        | `Val e_l -> `Err (Mismatched_type, Add (e_l, e_r)))
    | Fun (x, d) -> `Val (Fun (x, d))
    | Fix (x, e) -> `Exp (Fun (x, Fix (x, e)))
    | Filter (p, a, g, e) -> (
        match transition e with
        | `Err (err, exp) -> `Err (err, exp)
        | `Val value -> `Exp value
        | `Exp exp -> `Exp (Filter (p, a, g, exp)))
    | Residue (a, g, e) -> (
        match transition e with
        | `Err (err, exp) -> `Err (err, exp)
        | `Val exp -> `Exp exp
        | `Exp exp -> `Exp (Residue (a, g, exp)))

  let rec eval (expr : Exp.t) =
    match expr with
    | Var var -> `Err (Err.Unbound_variable, Exp.Var var)
    | Int int -> `Val (Exp.Int int)
    | Add (e_l, e_r) -> (
        match (eval e_l, eval e_r) with
        | `Val (Int n_l), `Val (Int n_r) -> `Val (Int (n_l + n_r))
        | r_l, r_r -> `Err (Mismatched_type, expr))
    | App (e_l, e_r) -> (
        match (eval e_l, eval e_r) with
        | `Err (err, exp), _ -> `Err (err, exp)
        | `Val _, `Err (err, exp) -> `Err (err, exp)
        | `Val (Fun (x, e)), `Val v_r -> eval (Exp.subst e x v_r)
        | `Val e_l, `Val e_r -> `Err (Mismatched_type, expr))
    | Fun (x, e) -> `Val (Fun (x, e))
    | Fix (x, e) -> `Val (Fun (x, Exp.subst e x e))
    | Filter (_, _, _, e) -> eval e
    | Residue (_, _, e) -> eval e
end

module Ctx = struct
  type t =
    | Top
    | Add_l of t * Exp.t
    | Add_r of Exp.t * t
    | App_l of t * Exp.t
    | App_r of Exp.t * t
    | Filter of Pat.t * Act.t * Gas.t * t
    | Residue of Act.t * Gas.t * t

  let rec to_string (ctx : t) =
    match ctx with
    | Top -> "∘"
    | Add_l (c, e) -> Printf.sprintf "(%s + %s)" (to_string c) (Exp.to_string e)
    | Add_r (e, c) -> Printf.sprintf "(%s + %s)" (Exp.to_string e) (to_string c)
    | App_l (c, e) -> Printf.sprintf "(%s %s)" (to_string c) (Exp.to_string e)
    | App_r (e, c) -> Printf.sprintf "(%s %s)" (Exp.to_string e) (to_string c)
    | Filter (p, a, g, c) ->
        Printf.sprintf "filter %s do %s for %s in %s" (Pat.to_string p)
          (Act.to_string a) (Gas.to_string g) (to_string c)
    | Residue (a, g, c) ->
        Printf.sprintf "do %s for %s in %s" (Act.to_string a) (Gas.to_string g)
          (to_string c)

  let rec decompose (exp : Exp.t) =
    match exp with
    | Var var -> (Top, Exp.Var var) :: []
    | Int _ | Fun _ -> []
    | Add (e_l, e_r) ->
        let c_l = decompose e_l in
        let c_r = decompose e_r in
        let cs =
          List.map (fun (c, e_l') -> (Add_l (c, e_r), e_l')) c_l
          @ List.map (fun (c, e_r') -> (Add_r (e_l, c), e_r')) c_r
        in
        if cs = [] then (Top, Add (e_l, e_r)) :: [] else cs
    | App (e_l, e_r) ->
        let c_l = decompose e_l in
        let c_r = decompose e_r in
        let cs =
          List.map (fun (c, e_l') -> (App_l (c, e_r), e_l')) c_l
          @ List.map (fun (c, e_r') -> (App_r (e_l, c), e_r')) c_r
        in
        if cs = [] then (Top, App (e_l, e_r)) :: [] else cs
    | Fix (x, d) -> (Top, Exp.Fix (x, d)) :: []
    | Filter (p, a, g, e) ->
        decompose e |> List.map (fun (c, e) -> (Filter (p, a, g, c), e))
    | Residue (a, g, e) ->
        decompose e |> List.map (fun (c, e) -> (Residue (a, g, c), e))

  let rec compose (ctx : t) (exp : Exp.t) =
    match ctx with
    | Top -> exp
    | Add_l (c_l, e_r) -> Add (compose c_l exp, e_r)
    | Add_r (e_l, c_r) -> Add (e_l, compose c_r exp)
    | App_l (c_l, e_r) -> App (compose c_l exp, e_r)
    | App_r (e_l, c_r) -> App (e_l, compose c_r exp)
    | Filter (p, a, g, c) -> Filter (p, a, g, compose c exp)
    | Residue (a, g, c) -> Residue (a, g, compose c exp)
end

let rec instr (pat : Pat.t) (act : Act.t) (gas : Gas.t) (exp : Exp.t) =
  let wrap exp =
    if Pat.matches pat exp then Exp.Residue (act, gas, exp) else exp
  in
  match Result.transition exp with
  | `Err _ -> exp
  | `Val value -> value
  | `Exp _ -> (
      match exp with
      | Var var -> wrap (Var var)
      | Int int -> Int int
      | Add (e_l, e_r) ->
          let e_l = instr pat act gas e_l in
          let e_r = instr pat act gas e_r in
          wrap (Add (e_l, e_r))
      | App (e_l, e_r) ->
          let e_l = instr pat act gas e_l in
          let e_r = instr pat act gas e_r in
          wrap (App (e_l, e_r))
      | Fun (x, e) -> Fun (x, e)
      | Fix (x, e) -> wrap (Fix (x, e))
      | Filter (p, a, g, e) ->
          let e = instr pat act gas e in
          Filter (p, a, g, instr p a g e)
      | Residue (a, g, e) ->
          let e = instr pat act gas e in
          Residue (a, g, e))

let rec annot (act : Act.t) (gas : Gas.t) (ctx : Ctx.t) =
  match ctx with
  | Top -> (act, gas, ctx)
  | Add_l (c, e) ->
    let (act, gas, c) = annot act gas c in
    (act, gas, Add_l (c, e))
  | Add_r (e, c) -> 
    let (act, gas, c) = annot act gas c in
    (act, gas, Add_r (e, c))
  | App_l (c, e) ->
    let (act, gas, c) = annot act gas c in
    (act, gas, App_l (c, e))
  | App_r (e, c) ->
    let (act, gas, c) = annot act gas c in
    (act, gas, App_r (e, c))
  | Filter (p, a, g, c) ->
    let (act, gas, c) = annot act gas c in
    (act, gas, Filter (p, a, g, c))
  | Residue (a, One, c) -> annot a One c
  | Residue (a, All, c) ->
    let (act, gas, c) = annot act gas c in
    (act, gas, Residue (a, All, c))

let rec step (exp : Exp.t) =
  Printf.printf "stepping %s\n" (Exp.to_string exp);
  let instr'd = instr Any Pause One exp in
  Printf.printf "instruct %s\n" (Exp.to_string instr'd);
  let decomposed = Ctx.decompose instr'd in
  Printf.printf "decomposed\n";
  decomposed |>
  List.iteri (fun i (c, e) -> Printf.printf "%2d: %s { %s }\n" i (Ctx.to_string c) (Exp.to_string e));
  let annot'd = decomposed |> List.map (fun (ctx, exp) ->
    let (act, gas, ctx) = annot Pause One ctx in 
    (act, gas, ctx, exp)) in
  Printf.printf "annot'd\n";
  annot'd |>
    List.iteri (fun i (_, _, c, e) -> Printf.printf "%2d: %s { %s }\n" i (Ctx.to_string c) (Exp.to_string e));
  match List.find_opt (fun (act, gas, ctx, exp) -> act == Act.Skip) annot'd with
  | None -> annot'd
  | Some (_, _, ctx, exp) ->
    match Result.transition exp with
    | `Err _ -> failwith "transition -> Err"
    | `Val _ -> failwith "transition -> Val"
    | `Exp exp -> step (Ctx.compose ctx exp)

let () =
  let expr = Exp.Fun ("x", Add (Var "x", Var "x")) in
  expr |> Exp.to_string |> print_endline

let () =
  Exp.subst (Add (Var "x", Var "y")) "x" (Int 1)
  |> Exp.to_string |> print_endline;
  Exp.subst (Fun ("x", Var "x")) "x" (Int 1) |> Exp.to_string |> print_endline;
  Exp.subst (Fun ("x", Add (Var "x", Var "y"))) "y" (Int 1)
  |> Exp.to_string |> print_endline

let () =
  Result.eval (App (App (Fun ("x", Add (Var "x", Var "y")), Int 1), Int 2))
  |> Result.to_string |> print_endline

let () =
  Ctx.decompose (Add (Add (Int 1, Int 2), Add (Int 3, Int 4)))
  |> List.iter (fun (c, e) ->
         Printf.printf "%s { %s }\n" (Ctx.to_string c) (Exp.to_string e))

let () =
  let body = Exp.Add (Add (Int 1, Int 2), Add (Int 3, Int 4)) in
  let exp = Exp.Filter (Pat.(Add (Val, Val)), Act.Pause, One, body) in
  let instr'ed = instr Any Skip All exp in
  instr'ed |> Exp.to_string |> print_endline;
  let objs = instr'ed |> Ctx.decompose in
  objs |> List.iter (fun (c, e) ->
    Printf.printf "%s { %s }\n" (Ctx.to_string c) (Exp.to_string e));
  objs |> List.iter (fun (c, e) ->
    let (act, gas, c) = annot Skip All c in
    Printf.printf "act = %s, gas = %s, %s { %s }\n" (Act.to_string act) (Gas.to_string gas) (Ctx.to_string c) (Exp.to_string e))

let () =
  let body = Exp.Add (Add (Int 1, Int 2), Add (Int 1, Int 2)) in
  let exp = Exp.Filter (Pat.(Add ((Int 1), (Int 2))), Act.Skip, One, body) in
  let trunk = step exp |> List.map (fun (_, _, c, e) ->
    Printf.sprintf "%s { %s }" (Ctx.to_string c) (Exp.to_string e)) |> String.concat "\n" in
  Printf.printf "========\n%s\n---------\n" trunk
