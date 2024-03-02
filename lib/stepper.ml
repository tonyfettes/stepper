open Syntax

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
end

let rec transition (exp : Exp.t) =
  match exp with
  | Var var -> `Err (Err.Unbound_variable, Exp.Var var)
  | Int int -> `Val (Exp.Int int)
  | Bool bool -> `Val (Exp.Bool bool)
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
  | Sub (e_l, e_r) -> (
      match transition e_l with
      | `Err (err, exp) -> `Err (err, exp)
      | `Exp e_l -> `Exp (Sub (e_l, e_r))
      | `Val (Int n_l) -> (
          match transition e_r with
          | `Err (err, exp) -> `Err (err, exp)
          | `Exp e_r -> `Exp (Sub (e_l, e_r))
          | `Val (Int n_r) -> `Exp (Int (n_l - n_r))
          | `Val e_r -> `Err (Mismatched_type, Sub (e_l, e_r)))
      | `Val e_l -> `Err (Mismatched_type, Sub (e_l, e_r)))
  | Mul (e_l, e_r) -> (
      match transition e_l with
      | `Err (err, exp) -> `Err (err, exp)
      | `Exp e_l -> `Exp (Mul (e_l, e_r))
      | `Val (Int n_l) -> (
          match transition e_r with
          | `Err (err, exp) -> `Err (err, exp)
          | `Exp e_r -> `Exp (Mul (e_l, e_r))
          | `Val (Int n_r) -> `Exp (Int (n_l * n_r))
          | `Val e_r -> `Err (Mismatched_type, Mul (e_l, e_r)))
      | `Val e_l -> `Err (Mismatched_type, Mul (e_l, e_r)))
  | App (e_l, e_r) -> (
      match transition e_l with
      | `Err (err, exp) -> `Err (err, exp)
      | `Exp e_l -> `Exp (App (e_l, e_r))
      | `Val (Fun (x, e_b)) -> (
          match transition e_r with
          | `Err (err, exp) -> `Err (err, exp)
          | `Exp e_r -> `Exp (App (e_l, e_r))
          | `Val e_r -> `Exp (Exp.subst e_b x e_r))
      | `Val e_l -> `Err (Mismatched_type, Add (e_l, e_r)))
  | Fun (x, d) -> `Val (Fun (x, d))
  | Fix (x, e) -> `Exp (Fun (x, Fix (x, e)))
  | Filter (p, a, g, e) -> (
      match transition e with
      | `Err (err, exp) -> `Err (err, exp)
      | `Val value -> `Exp value
      | `Exp exp -> `Exp (Filter (p, a, g, exp)))
  | Residue (a, g, l, e) -> (
      match transition e with
      | `Err (err, exp) -> `Err (err, exp)
      | `Val exp -> `Exp exp
      | `Exp exp -> `Exp (Residue (a, g, l, exp)))

let rec eval (expr : Exp.t) =
  match expr with
  | Var var -> `Err (Err.Unbound_variable, Exp.Var var)
  | Int int -> `Val (Exp.Int int)
  | Bool bool -> `Val (Exp.Bool bool)
  | Add (e_l, e_r) -> (
      match (eval e_l, eval e_r) with
      | `Val (Int n_l), `Val (Int n_r) -> `Val (Int (n_l + n_r))
      | _, _ -> `Err (Mismatched_type, expr))
  | Sub (e_l, e_r) -> (
      match (eval e_l, eval e_r) with
      | `Val (Int n_l), `Val (Int n_r) -> `Val (Int (n_l - n_r))
      | _, _ -> `Err (Mismatched_type, expr))
  | Mul (e_l, e_r) -> (
      match (eval e_l, eval e_r) with
      | `Val (Int n_l), `Val (Int n_r) -> `Val (Int (n_l * n_r))
      | _, _ -> `Err (Mismatched_type, expr))
  | App (e_l, e_r) -> (
      match (eval e_l, eval e_r) with
      | `Err (err, exp), _ -> `Err (err, exp)
      | `Val _, `Err (err, exp) -> `Err (err, exp)
      | `Val (Fun (x, e)), `Val v_r -> eval (Exp.subst e x v_r)
      | `Val _, `Val _ -> `Err (Mismatched_type, expr))
  | Fun (x, e) -> `Val (Fun (x, e))
  | Fix (x, e) -> `Val (Fun (x, Exp.subst e x e))
  | Filter (_, _, _, e) -> eval e
  | Residue (_, _, _, e) -> eval e

module Ctx = struct
  type t =
    | Top
    | Add_l of t * Exp.t
    | Add_r of Exp.t * t
    | Sub_l of t * Exp.t
    | Sub_r of Exp.t * t
    | Mul_l of t * Exp.t
    | Mul_r of Exp.t * t
    | App_l of t * Exp.t
    | App_r of Exp.t * t
    | Filter of Pat.t * Act.t * Gas.t * t
    | Residue of Act.t * Gas.t * int * t

  let rec to_string (ctx : t) =
    match ctx with
    | Top -> "@"
    | Add_l (c, e) -> Printf.sprintf "(%s + %s)" (to_string c) (Exp.to_string e)
    | Add_r (e, c) -> Printf.sprintf "(%s + %s)" (Exp.to_string e) (to_string c)
    | Sub_l (c, e) -> Printf.sprintf "(%s - %s)" (to_string c) (Exp.to_string e)
    | Sub_r (e, c) -> Printf.sprintf "(%s - %s)" (Exp.to_string e) (to_string c)
    | Mul_l (c, e) -> Printf.sprintf "(%s * %s)" (to_string c) (Exp.to_string e)
    | Mul_r (e, c) -> Printf.sprintf "(%s * %s)" (Exp.to_string e) (to_string c)
    | App_l (c, e) -> Printf.sprintf "(%s %s)" (to_string c) (Exp.to_string e)
    | App_r (e, c) -> Printf.sprintf "(%s %s)" (Exp.to_string e) (to_string c)
    | Filter (p, a, g, c) ->
        Printf.sprintf "(filter %s do %s for %s in %s)" (Pat.to_string p)
          (Act.to_string a) (Gas.to_string g) (to_string c)
    | Residue (a, g, l, c) ->
        Printf.sprintf "(do %s for %s at %d in %s)" (Act.to_string a)
          (Gas.to_string g) l (to_string c)

  let rec decompose (exp : Exp.t) =
    match exp with
    | Var var -> (Top, Exp.Var var) :: []
    | Int _ | Bool _ | Fun _ -> []
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
        let c =
          decompose e |> List.map (fun (c, e) -> (Filter (p, a, g, c), e))
        in
        if c = [] then (Top, Filter (p, a, g, e)) :: [] else c
    | Residue (a, g, l, e) ->
        let c =
          decompose e |> List.map (fun (c, e) -> (Residue (a, g, l, c), e))
        in
        if c = [] then (Top, Residue (a, g, l, e)) :: [] else c

  let rec compose (ctx : t) (exp : Exp.t) =
    match ctx with
    | Top -> exp
    | Add_l (c_l, e_r) -> Add (compose c_l exp, e_r)
    | Add_r (e_l, c_r) -> Add (e_l, compose c_r exp)
    | Sub_l (c_l, e_r) -> Sub (compose c_l exp, e_r)
    | Sub_r (e_l, c_r) -> Sub (e_l, compose c_r exp)
    | Mul_l (c_l, e_r) -> Mul (compose c_l exp, e_r)
    | Mul_r (e_l, c_r) -> Mul (e_l, compose c_r exp)
    | App_l (c_l, e_r) -> App (compose c_l exp, e_r)
    | App_r (e_l, c_r) -> App (e_l, compose c_r exp)
    | Filter (p, a, g, c) -> Filter (p, a, g, compose c exp)
    | Residue (a, g, l, c) -> Residue (a, g, l, compose c exp)
end

let rec instr (pat : Pat.t) (act : Act.t) (gas : Gas.t) (lvl : int)
    (exp : Exp.t) =
  let wrap exp =
    if Pat.matches pat exp then Exp.Residue (act, gas, lvl, exp) else exp
  in
  match transition exp with
  | `Err _ -> exp
  | `Val _ -> exp
  | `Exp _ -> (
      match exp with
      | Var var -> wrap (Var var)
      | Int int -> Int int
      | Bool bool -> Bool bool
      | Add (e_l, e_r) ->
          let e_l = instr pat act gas lvl e_l in
          let e_r = instr pat act gas lvl e_r in
          wrap (Add (e_l, e_r))
      | App (e_l, e_r) ->
          let e_l = instr pat act gas lvl e_l in
          let e_r = instr pat act gas lvl e_r in
          wrap (App (e_l, e_r))
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
      | Filter (p, a, g, e) ->
          let e = instr pat act gas lvl e in
          Filter (p, a, g, instr p a g (lvl + 1) e)
      | Residue (a, g, l, e) ->
          let e = instr pat act gas lvl e in
          Residue (a, g, l, e))

let rec annot (act : Act.t) (lvl : int) (ctx : Ctx.t) =
  match ctx with
  | Top -> (act, ctx)
  | Add_l (c, e) ->
      let act, c = annot act lvl c in
      (act, Add_l (c, e))
  | Add_r (e, c) ->
      let act, c = annot act lvl c in
      (act, Add_r (e, c))
  | App_l (c, e) ->
      let act, c = annot act lvl c in
      (act, App_l (c, e))
  | App_r (e, c) ->
      let act, c = annot act lvl c in
      (act, App_r (e, c))
  | Sub_l (c, e) ->
      let act, c = annot act lvl c in
      (act, Sub_l (c, e))
  | Sub_r (e, c) ->
      let act, c = annot act lvl c in
      (act, Sub_r (e, c))
  | Mul_l (c, e) ->
      let act, c = annot act lvl c in
      (act, Mul_l (c, e))
  | Mul_r (e, c) ->
      let act, c = annot act lvl c in
      (act, Mul_r (e, c))
  | Filter (p, a, g, c) ->
      let act, c = annot act lvl c in
      (act, Filter (p, a, g, c))
  | Residue (a, One, l, c) -> if l > lvl then annot a l c else annot act lvl c
  | Residue (a, All, l, c) ->
      if l > lvl then
        let act, c = annot a l c in
        (act, Residue (a, All, l, c))
      else
        let act, c = annot act lvl c in
        (act, Residue (a, All, l, c))

let rec step (exp : Exp.t) =
  let instr'd = instr Any Pause One 0 exp in
  let decomposed = Ctx.decompose instr'd in
  let annot'd =
    decomposed
    |> List.map @@ fun (ctx, exp) ->
       match exp with
       | Syntax.Exp.Filter _ | Syntax.Exp.Residue _ -> (Act.Eval, ctx, exp)
       | _ ->
           let act, ctx = annot Pause 0 ctx in
           (act, ctx, exp)
  in
  match List.find_opt (fun (act, _, _) -> act == Act.Eval) annot'd with
  | None -> (
      match annot'd |> List.map (fun (_, c, e) -> (c, e)) with
      | [] -> `Val exp
      | exp -> `Exp exp)
  | Some (_, ctx, exp) -> (
      match transition exp with
      | `Err _ -> failwith "transition -> Err"
      | `Val exp -> `Val exp
      | `Exp exp -> step (Ctx.compose ctx exp))
