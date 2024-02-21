module App = {
  // This sample forces an import of Belt.*, so that CI builds can ensure that
  // Melange has been installed correctly for JS bundlers to be able to find it.
  [@react.component]
  let make = () => {
    let (input, setInput) = React.useState(() => "");
    let (result, setResult) = React.useState(() => "");
    <div>
      <input
        type_="text"
        value=input
        onChange={e => {
          let source = e->ReactEvent.Form.target##value;
          setInput(_ => source)
          print_endline("source: " ++ source);
          let buf = Lexing.from_string(source);
          switch (Stepper.Parser.top(Stepper.Lexer.lex, buf)) {
          | exception Stepper.Lexer.Error(message) =>
            setResult(_ => "Lexing error: " ++ message)
          | exception Stepper.Parser.Error | None =>
            setResult(_ => "Syntax error")
          | Some(ast) =>
            let result = Stepper.Evaluator.eval(ast);
            setResult(_ => Stepper.Evaluator.Result.to_string(result))
          };
        }}
      />
      <p>{React.string(result)}</p>
    </div>
  }
};

ReactDOM.querySelector("#root")
->(
    fun
    | Some(root) => ReactDOM.render(<App />, root)
    | None =>
      Js.Console.error(
        "Failed to start React: couldn't find the #root element",
      )
  );
