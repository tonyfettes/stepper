open Syntax

exception Unbound_variable of Exp.t
exception Mismatched_type of Exp.t
exception Limit of Exp.t

type act_obj = { ctx : Ctx.t; exp : Exp.t; act : Act.t; act_lvl : int }
type obj = { ctx : Ctx.t; exp : Exp.t }
type step = Val of Exp.t | Exp of obj list
type state = { act : Act.t; act_lvl : int; ins_lvl : int; ctx : Ctx.t }

(* flt_env is innermost first; its entries carry their lexical level. *)
let rec lookup flt_env exp (state : state) =
  match flt_env with
  | [] -> state
  | (lvl, _) :: _ when lvl <= state.ins_lvl -> state
  | (lvl, (flt : filter)) :: rest -> (
      if not (Matches.matches flt.pat exp) then lookup rest exp state
      else
        let state =
          if lvl > state.act_lvl then
            { state with act = flt.act; act_lvl = lvl }
          else state
        in
        match flt.gas with
        | One -> lookup rest exp state
        | All ->
            (* The residue records THIS filter's act, even if a higher-level
               one-shot filter already chose a different act. *)
            {
              state with
              ins_lvl = lvl;
              ctx = Residue ({ act = flt.act; gas = All; lvl }, state.ctx);
            })

let decompose exp =
  let rec go flt_env depth (state : state) (exp : Exp.t) =
    match exp with
    | Int _ | Bool _ | Fun _ -> []
    | Var _ -> raise (Unbound_variable exp)
    | Filter (flt, _) when not (Matches.is_closed flt.pat) ->
        raise (Unbound_variable exp)
    | Filter (flt, body) ->
        let found =
          go
            ((depth + 1, flt) :: flt_env)
            (depth + 1)
            { state with ctx = Filter (flt, state.ctx) }
            body
        in
        if found = [] then
          [ { ctx = state.ctx; exp; act = Eval; act_lvl = state.act_lvl } ]
        else found
    | Residue (residue, body) ->
        let act, act_lvl =
          if residue.lvl > state.act_lvl then (residue.act, residue.lvl)
          else (state.act, state.act_lvl)
        in
        let ctx =
          if residue.gas = All && residue.lvl > state.ins_lvl then
            Ctx.Residue (residue, state.ctx)
          else state.ctx
        in
        let found =
          go flt_env depth
            { act; act_lvl; ins_lvl = max state.ins_lvl residue.lvl; ctx }
            body
        in
        (* Wrapper elimination uses the original context and whole wrapper. *)
        if found = [] then [ { ctx = state.ctx; exp; act = Eval; act_lvl } ]
        else found
    | Eq _ | Add _ | Sub _ | Mul _ | Ap _ | If _ | Fix _ ->
        let state = lookup flt_env exp state in
        let under ctx e = go flt_env depth { state with ctx } e in
        let both ctx_l l ctx_r r =
          (* Explicit sequencing preserves MoonBit's left-to-right errors. *)
          let left = under ctx_l l in
          let right = under ctx_r r in
          left @ right
        in
        let c = state.ctx in
        let found =
          match exp with
          | Eq (l, r) -> both (EqL (c, r)) l (EqR (l, c)) r
          | Add (l, r) -> both (AddL (c, r)) l (AddR (l, c)) r
          | Sub (l, r) -> both (SubL (c, r)) l (SubR (l, c)) r
          | Mul (l, r) -> both (MulL (c, r)) l (MulR (l, c)) r
          | Ap (l, r) -> both (ApL (c, r)) l (ApR (l, c)) r
          | If (c, t, f) -> under (If (state.ctx, t, f)) c
          | _ -> []
        in
        if found = [] then
          [ { ctx = state.ctx; exp; act = state.act; act_lvl = state.act_lvl } ]
        else found
  in
  go [] 0 { act = Pause; act_lvl = 0; ins_lvl = 0; ctx = Top } exp

let transition (exp : Exp.t) : Exp.t =
  match exp with
  | Eq (Int l, Int r) -> Bool (l = r)
  | Add (Int l, Int r) -> Int (Int32.add l r)
  | Sub (Int l, Int r) -> Int (Int32.sub l r)
  | Mul (Int l, Int r) -> Int (Int32.mul l r)
  | Ap (Fun (x, body), arg) -> Subst.exp body x arg
  | Fix (x, body) -> Subst.exp body x exp
  | If (Bool true, t, _) -> t
  | If (Bool false, _, f) -> f
  | Var _ -> raise (Unbound_variable exp)
  | Filter (flt, _) when not (Matches.is_closed flt.pat) ->
      raise (Unbound_variable exp)
  | Filter (_, e) | Residue (_, e) -> e
  | _ -> raise (Mismatched_type exp)

let step ?limit exp =
  Option.iter (fun n -> if n < 0 then invalid_arg "step: negative limit") limit;
  let rec go remaining exp =
    let objs = decompose exp in
    match List.find_opt (fun (obj : act_obj) -> obj.act = Eval) objs with
    | None ->
        if objs = [] then Val exp
        else
          Exp
            (List.map
               (fun (obj : act_obj) -> { ctx = obj.ctx; exp = obj.exp })
               objs)
    | Some obj ->
        if remaining = Some 0 then raise (Limit exp);
        go
          (Option.map (fun n -> n - 1) remaining)
          (Ctx.compose obj.ctx (transition obj.exp))
  in
  go limit exp

let take (obj : obj) = Ctx.compose obj.ctx (transition obj.exp)
