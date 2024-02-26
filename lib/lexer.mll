{
open Parser

exception Error of string
}

let white = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | digit | '_')*

rule lex =
  parse
  | white { lex lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "fun" { FUN }
  | "->" { THIN_ARROW }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "eval" { EVAL }
  | "hide" { HIDE }
  | "pause" { PAUSE }
  | "debug" { DEBUG }
  | "filter" { FILTER }
  | "one" { ONE }
  | "all" { ALL }
  | "do" { DO }
  | "for" { FOR }
  | "at" { AT }
  | "in" { IN }
  | "$e" { DOLLAR_E }
  | "$v" { DOLLAR_V }
  | "in" { IN }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
  | _ { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
