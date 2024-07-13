module Type : sig
  type t = Int | Bool | Fun of t * t | Ref of t ref

  val to_string : t -> string
end

exception Mismatched of Type.t * Type.t

type t

val create : capacity:int -> t
val check : t -> expect:Type.t -> Stepper_syntax.Expr.t -> unit
val infer : t -> Stepper_syntax.Expr.t -> Type.t
