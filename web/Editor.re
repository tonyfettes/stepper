[@react.component]
let make = () => {
  let (input, setInput) = React.useState(() => "");
  let (history, setHistory) = React.useState(() => []);
  let (result, setResult) = React.useState(() => `Expr([]));
  let (settings, setSettings) = React.useState(() => Settings.default);

  let updateResult = (expr: Stepper.Expr.t) => {
    let result =
      switch (Stepper.step(expr)) {
      | exception (Stepper.Unbound_variable(expr) as exn)
      | exception (Stepper.Mismatched_type(expr) as exn) =>
        let expr = {
          let buffer = Buffer.create(42);
          expr
          |> Stepper.Expr.pretty_print
          |> Stepper.Printer.to_buffer(buffer);
          Buffer.contents(buffer);
        };
        Printf.sprintf(
          "%s: %s",
          expr,
          switch (exn) {
          | Stepper.Unbound_variable(_) => "Unbound_variable"
          | Stepper.Mismatched_type(_) => "Mismatched_type"
          | _ => ""
          },
        )
        ->`Error;
      | Value(value) => `Value(value)
      | Expr(expr) => `Expr(expr)
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
    | exception (Stepper.Unbound_variable(expr) as exn)
    | exception (Stepper.Mismatched_type(expr) as exn) =>
      let message =
        Printf.sprintf(
          "%s: %s",
          expr->Stepper.Expr.to_string,
          switch (exn) {
          | Stepper.Unbound_variable(_) => "Unbound_variable"
          | Stepper.Mismatched_type(_) => "Mismatched_type"
          | _ => ""
          },
        );
      setResult(_ => `Error(message));
    | Expr(expr) => expr |> Stepper.compose(context) |> updateResult
    | Value(value) =>
      value
      |> Stepper.Value.to_expr
      |> Stepper.compose(context)
      |> updateResult
    };
  };

  <>
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
    <Settings
      value=settings
      onChange={settings => setSettings(_ => settings)}
    />
    <History settings history={history->List.rev} />
    <Output settings value=result onClick=onStep />
  </>;
};
