open Stepper.Syntax

module Doc = struct
  include PPrint

  let text = string
  let line = hardline

  let render document =
    let buffer = Buffer.create 128 in
    PPrint.ToBuffer.pretty 1.0 64 buffer document;
    Buffer.contents buffer
end

open Doc

let keyword ?(short = false) act gas =
  if not short then to_keyword act gas
  else
    match (act, gas) with
    | Act.Eval, Gas.One -> "h"
    | Eval, All -> "e"
    | Pause, One -> "p"
    | Pause, All -> "d"

let rec pattern ?(body = false) (pat : Pat.t) =
  let atom value = if body then "(" ^ value ^ ")" else value in
  let binary op left right =
    "(" ^ pattern ~body left ^ " " ^ op ^ " " ^ pattern ~body right ^ ")"
  in
  match pat with
  | Any -> atom "$e"
  | Val -> atom "$v"
  | Var name -> atom name
  | Int value -> atom (Int32.to_string value)
  | Bool value -> atom (string_of_bool value)
  | Eq (left, right) -> binary "==" left right
  | Add (left, right) -> binary "+" left right
  | Sub (left, right) -> binary "-" left right
  | Mul (left, right) -> binary "*" left right
  | Ap (left, right) -> pattern ~body left ^ pattern ~body right
  | Fun (name, value) -> "(fun " ^ name ^ " -> " ^ pattern ~body:true value ^ ")"
  | Fix (name, value) -> "(fix " ^ name ^ " -> " ^ pattern ~body:true value ^ ")"
  | If (condition, yes, no) ->
      "(if " ^ pattern ~body condition ^ " then " ^ pattern ~body yes
      ^ " else " ^ pattern ~body no ^ ")"

let rec precedence ~residue (value : Exp.t) =
  match value with
  | Var _ | Int _ | Bool _ -> 7
  | Eq _ -> 3
  | Add _ | Sub _ -> 4
  | Mul _ -> 5
  | Ap _ -> 6
  | Fun _ | Fix _ | If _ | Filter _ -> 0
  | Residue (_, value) -> if residue then 6 else precedence ~residue value

let rec expression_doc ~residue ~prec (value : Exp.t) =
  let taken = precedence ~residue value in
  let child ?(prec = taken) value = expression_doc ~residue ~prec value in
  let wrap doc = if taken < prec then parens doc else group doc in
  match value with
  | Var name -> wrap (text name)
  | Int value -> wrap (text (Int32.to_string value))
  | Bool value -> wrap (text (string_of_bool value))
  | Eq (left, right) -> wrap (child left ^/^ text "==" ^/^ child right)
  | Add (left, right) ->
      wrap (child left ^/^ text "+" ^/^ child ~prec:(taken + 1) right)
  | Sub (left, right) -> wrap (child left ^/^ text "-" ^/^ child right)
  | Mul (left, right) -> wrap (child left ^/^ text "*" ^/^ child right)
  | Ap (left, right) -> wrap (child left ^^ break 0 ^^ parens (child ~prec:0 right))
  | Fun (name, value) ->
      wrap (group (text "fun" ^/^ text name ^/^ text "->")
            ^^ nest 2 (break 1 ^^ child value))
  | Fix (name, value) ->
      wrap (group (text "fix" ^/^ text name ^/^ text "->")
            ^^ nest 2 (break 1 ^^ child value))
  | If (condition, yes, no) ->
      wrap (group (text "if" ^/^ child condition ^/^ text "then")
            ^^ nest 2 (break 1 ^^ child yes)
            ^/^ text "else" ^^ nest 2 (break 1 ^^ child no))
  | Filter (filter, value) ->
      wrap (group (group (text "debug" ^/^ text (keyword filter.act filter.gas)
                           ^^ parens (text (pattern filter.pat)) ^^ text "in")
                   ^/^ child value))
  | Residue (annotation, value) ->
      if residue then
        wrap (text (keyword ~short:true annotation.act annotation.gas)
              ^^ text "#" ^^ text (string_of_int annotation.lvl)
              ^^ text "[" ^^ break 0 ^^ child ~prec:0 value ^^ break 0 ^^ text "]")
      else child ~prec value

type focused =
  | Selected
  | Left of string * int * focused * Exp.t
  | Right of string * int * Exp.t * focused
  | Function of focused * Exp.t
  | Argument of Exp.t * focused
  | Condition of focused * Exp.t * Exp.t
  | Filtered of filter * focused
  | Residual of residue * focused

(* Current contexts run from the hole outwards; the old printer runs root first. *)
let rec focus (ctx : Stepper.Ctx.t) selected =
  match ctx with
  | Top -> selected
  | EqL (outer, right) -> focus outer (Left ("==", 3, selected, right))
  | EqR (left, outer) -> focus outer (Right ("==", 3, left, selected))
  | AddL (outer, right) -> focus outer (Left ("+", 4, selected, right))
  | AddR (left, outer) -> focus outer (Right ("+", 4, left, selected))
  | SubL (outer, right) -> focus outer (Left ("-", 4, selected, right))
  | SubR (left, outer) -> focus outer (Right ("-", 4, left, selected))
  | MulL (outer, right) -> focus outer (Left ("*", 5, selected, right))
  | MulR (left, outer) -> focus outer (Right ("*", 5, left, selected))
  | ApL (outer, right) -> focus outer (Function (selected, right))
  | ApR (left, outer) -> focus outer (Argument (left, selected))
  | If (outer, yes, no) -> focus outer (Condition (selected, yes, no))
  | Filter (filter, outer) -> focus outer (Filtered (filter, selected))
  | Residue (annotation, outer) -> focus outer (Residual (annotation, selected))

let rec context_doc ~residue ~expr ~prec focused =
  let taken =
    match focused with
    | Selected | Condition _ | Filtered _ | Residual _ -> 0
    | Left (_, taken, _, _) | Right (_, taken, _, _) -> taken
    | Function _ | Argument _ -> 6
  in
  let child ?(prec = taken) focused = context_doc ~residue ~expr ~prec focused in
  let value ?(prec = taken) exp = expression_doc ~residue ~prec exp in
  let document =
    match focused with
    | Selected -> braces (value expr)
    | Left (op, _, selected, right) ->
        let right_prec = if op = "+" then taken + 1 else taken in
        child selected ^/^ text op ^/^ value ~prec:right_prec right
    | Right (op, _, left, selected) ->
        let right_prec = if op = "+" then taken + 1 else taken in
        value left ^/^ text op ^/^ child ~prec:right_prec selected
    | Function (selected, argument) ->
        child selected ^^ break 0 ^^ parens (value ~prec:0 argument)
    | Argument (fn, selected) ->
        value fn ^^ break 0 ^^ parens (child ~prec:0 selected)
    | Condition (selected, yes, no) ->
        group (text "if" ^/^ child selected ^/^ text "then")
        ^^ nest 2 (line ^^ value yes)
        ^^ line ^^ text "else" ^^ nest 2 (line ^^ value no)
    | Filtered (filter, selected) ->
        group (text "debug" ^/^ text (keyword filter.act filter.gas)
               ^^ parens (text (pattern filter.pat)) ^/^ text "in")
        ^/^ child selected
    | Residual (annotation, selected) ->
        if residue then
          text (keyword annotation.act annotation.gas)
          ^^ text "#" ^^ text (string_of_int annotation.lvl)
          ^^ text "[" ^^ break 0 ^^ child selected ^^ break 0 ^^ text "]"
        else child ~prec selected
  in
  if taken < prec then parens (nest 1 document) else group document

let exp ?(residue = false) value = render (expression_doc ~residue ~prec:0 value)

let value (value : Exp.t) =
  match value with
  | Fun (name, body) ->
      render (group (text "fun" ^/^ text name ^/^ text "->"
                     ^/^ expression_doc ~residue:false ~prec:0 body))
  | _ -> exp value

let context ?(residue = false) ~expr ctx =
  render (context_doc ~residue ~expr ~prec:0 (focus ctx Selected))
