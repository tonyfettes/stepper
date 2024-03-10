[@react.component]
let make = () => {
  let (input, setInput) = React.useState(() => "");
  let (history, setHistory) = React.useState(() => []);
  let (result, setResult) = React.useState(() => `Exp([]));

  let updateResult = input => {
    print_endline("source: " ++ input);
    let buf = Lexing.from_string(input);
    switch (Parser.top(Lexer.lex, buf)) {
    | exception (Lexer.Error(message)) =>
      setResult(_ => `Err("Lexing error: " ++ message))
    | exception Parser.Error
    | None => setResult(_ => `Err("Syntax error"))
    | Some(exp) =>
      let result =
        switch (Stepper.step(exp)) {
        | `Err(err, exp) =>
          `Err(
            Printf.sprintf(
              "%s: %s",
              exp->Syntax.Exp.to_string,
              err->Stepper.Err.to_string,
            ),
          )
        | `Val(exp) => `Val(exp)
        | `Exp(exp) => `Exp(exp)
        };
      setResult(_ => result);
    };
  };

  React.useEffect0(() => {
    Dom.Storage.(localStorage |> getItem("stepper-input"))
    ->Belt.Option.forEach(input => {
        setInput(_ => input);
        updateResult(input);
      });
    None;
  });

  let onStep = (ctx, exp) => {
    setHistory(history => [(ctx, exp), ...history]);
    switch (Stepper.transition(exp)) {
    | `Err(err, exp) =>
      let message =
        Printf.sprintf(
          "%s: %s",
          exp->Syntax.Exp.to_string,
          err->Stepper.Err.to_string,
        );
      setResult(_ => `Err(message));
    | `Exp(exp)
    | `Val(exp) =>
      let exp = Stepper.Ctx.compose(ctx, exp);
      let exp = exp |> Syntax.Exp.to_string(~residue=true);
      updateResult(exp);
    };
  };

  <div>
    <Input
      value=input
      onChange={input => {
        Dom.Storage.(localStorage |> setItem("stepper-input", input));
        setHistory(_ => []);
        setInput(_ => input);
        updateResult(input);
      }}
    />
    <hr />
    <History history={history->List.rev} />
    <hr />
    <Result value=result onClick=onStep />
  </div>;
};
