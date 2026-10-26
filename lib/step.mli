exception Unbound_variable of Syntax.Exp.t
exception Mismatched_type of Syntax.Exp.t
exception Limit of Syntax.Exp.t

type act_obj = {
  ctx : Ctx.t;
  exp : Syntax.Exp.t;
  act : Syntax.Act.t;
  act_lvl : int;
}

type obj = { ctx : Ctx.t; exp : Syntax.Exp.t }
type step = Val of Syntax.Exp.t | Exp of obj list

val decompose : Syntax.Exp.t -> act_obj list
(** All candidates, left to right, with independent contexts. Raises
    [Unbound_variable] for a reached free variable or non-closed filter pattern.
*)

val transition : Syntax.Exp.t -> Syntax.Exp.t
(** One instruction transition. Supply a redex returned by [decompose]. *)

val step : ?limit:int -> Syntax.Exp.t -> step
(** Repeatedly takes the leftmost silent candidate. Returns all visible choices
    only when none is silent, or [Val] when done. [limit] bounds silent
    transitions in this call; exhaustion raises [Limit] with the current
    expression. Without a limit this may diverge, just as MoonBit's [step] can.
*)

val take : obj -> Syntax.Exp.t
(** Commit one offered visible choice: transition its redex and recompose. *)
