val matches : Syntax.Pat.t -> Syntax.Exp.t -> bool
(** Binder-aware matching, ignoring filters and residues in the expression. *)

val is_closed : Syntax.Pat.t -> bool
