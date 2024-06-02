module Act : sig
  type t = Eval | Pause

  val to_string : t -> string
end

module Gas : sig
  type t = One | All

  val to_string : t -> string
end

val to_keyword : ?short:bool -> Act.t -> Gas.t -> string

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
end

and Value : sig
  type t = Int of int | Bool of bool | Fun of string * Expr.t

  val pretty_print : ?prec:int -> t -> PPrint.document
  val to_string : t -> string
  val to_expr : t -> Expr.t
  val to_pat : t -> Pat.t
end
