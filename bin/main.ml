let () =
  let input = In_channel.input_all In_channel.stdin in
  let ast = Stepper.Parser.top Stepper.Lexer.lex (Lexing.from_string input) in
  let ast = Option.get ast in
  print_endline (Stepper.Syntax.Exp.to_string ast)
