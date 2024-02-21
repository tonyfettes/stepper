{
open Parser
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
  | "filter" { FILTER }
  | "in" { IN }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
