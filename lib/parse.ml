exception Error of int * string

let parse source =
  let lexbuf = Lexing.from_string source in
  try Parser.top Lexer.lex lexbuf with
  | Lexer.Error message -> raise (Error (Lexing.lexeme_start lexbuf, message))
  | Parsing.Parse_error ->
      raise
        (Error (Lexing.lexeme_start lexbuf, "Expected an expression or pattern"))
