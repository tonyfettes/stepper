[@react.component]
let make = () => {
  let (input, setInput) = React.useState(() => "");
  let (history, setHistory) = React.useState(() => []);
  let (result, setResult) = React.useState(() => `Expr([]));
  let (settings, setSettings) = React.useState(() => Settings.default);

  let updateResult = (expr: Stepper.Expr.t) => {
    let result =
      switch (Stepper.step(expr)) {
      | `Error(error, expr) =>
        `Error(
          Printf.sprintf(
            "%s: %s",
            expr->Stepper.Expr.to_string,
            error->Stepper.Error.to_string,
          ),
        )
      | `Value(value) => `Value(value)
      | `Expr(expr) => `Expr(expr)
      };
    setResult(_ => result);
  };

  let inputResult = (input: string) => {
    switch (Stepper.parse(input)) {
    | exception (Stepper.Lexer.Error(message)) =>
      setResult(_ => `Error("Lexing error: " ++ message))
    | exception Stepper.Parser.Error
    | None => setResult(_ => `Error("Syntax error"))
    | Some(expr) => updateResult(expr)
    };
  };

  React.useEffect0(() => {
    Dom.Storage.localStorage
    |> Dom.Storage.getItem("stepper-input")
    |> Option.iter(input => {
         setInput(_ => input);
         inputResult(input);
       });
    None;
  });

  let onStep = (context, expr) => {
    setHistory(history => [(context, expr), ...history]);
    switch (Stepper.transition(expr)) {
    | `Error(error, expr) =>
      let message =
        Printf.sprintf(
          "%s: %s",
          expr->Stepper.Expr.to_string,
          error->Stepper.Error.to_string,
        );
      setResult(_ => `Error(message));
    | `Expr(expr) => expr |> Stepper.Context.compose(context) |> updateResult
    | `Value(value) =>
      value
      |> Stepper.Value.to_expr
      |> Stepper.Context.compose(context)
      |> updateResult
    };
  };

  <div>
    <Settings
      value=settings
      onChange={settings => setSettings(_ => settings)}
    />
    <Input
      value=input
      onChange={input => {
        Dom.Storage.localStorage
        |> Dom.Storage.setItem("stepper-input", input);
        setHistory(_ => []);
        setInput(_ => input);
        inputResult(input);
      }}
    />
    <hr />
    <History settings history={history->List.rev} />
    <hr />
    <Result settings value=result onClick=onStep />
  </div>;
};
