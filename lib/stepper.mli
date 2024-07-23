module Printer = Stepper_printer
module Lexer = Stepper_lexer
module Parser = Stepper_parser
module Typer = Stepper_typer
module Syntax = Stepper_syntax
module Expr = Syntax.Expr
module Value = Syntax.Value

val parse : string -> Expr.t option

exception Unbound_variable of Expr.t
exception Mismatched_type of Expr.t

module Context : sig
  type t

  val pretty_print :
    ?residue:bool -> ?prec:int -> ?expr:Expr.t -> t -> PPrint.document

  val to_string : t -> string
end

module Result : sig
  type 'a t = private Value of Value.t | Expr of 'a
end

val decompose : Expr.t -> (Context.t * Expr.t) list
val compose : Context.t -> Expr.t -> Expr.t
val transition : Expr.t -> Expr.t Result.t
val evaluate : Expr.t -> Value.t
val step : ?limit:int -> Expr.t -> (Context.t * Expr.t) list Result.t
