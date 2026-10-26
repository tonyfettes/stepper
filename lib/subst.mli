val to_pat : Syntax.Exp.t -> Syntax.Pat.t
val pat : Syntax.Pat.t -> string -> Syntax.Exp.t -> Syntax.Pat.t

val exp : Syntax.Exp.t -> string -> Syntax.Exp.t -> Syntax.Exp.t
(** Substitution for closed replacements, as used by MoonBit's evaluator. *)
