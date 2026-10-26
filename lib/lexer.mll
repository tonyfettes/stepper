{
open Parser
exception Error of string
}

let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule lex = parse
| [' ' '\t' '\r' '\n']+ { lex lexbuf }
| "(" { LPAREN }
| ")" { RPAREN }
| "->" { THIN_ARROW }
| "+" { PLUS }
| "-" { MINUS }
| "*" { TIMES }
| "==" { EQEQ }
| "=" { EQ }
| "$e" { DOLLAR_E }
| "$v" { DOLLAR_V }
| "fun" { FUN }
| "fix" { FIX }
| "true" { TRUE }
| "false" { FALSE }
| "debug" { DEBUG }
| "eval" { EVAL }
| "hide" { HIDE }
| "stop" { STOP }
| "step" { STEP }
| "in" { IN }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "let" { LET }
| "rec" { REC }
| ident as name { IDENT name }
| '-'? digit+ as text {
    try INT (Int32.of_string text)
    with Failure _ -> raise (Error ("Invalid 32-bit integer: " ^ text))
  }
| eof { EOF }
| _ as c { raise (Error (Printf.sprintf "Unexpected character: %C" c)) }
