[@react.component]
let make = () => {
  let (input, setInput) = React.useState(() => "");
  let (history, setHistory) = React.useState(() => []);
  let (result, setResult) = React.useState(() => StepperResult.Waiting);
  let (settings, setSettings) =
    React.useState(() =>
      {StepperReactSettings.showResidue: false, optimize: true}
    );
  let (worker, setWorker) = React.useState(() => None);
  let (trigger, setTrigger) = React.useState(() => 0);

  React.useEffect1(
    () => {
      let worker =
        Webapi.Dom.Worker.makeWithUrl(
          Webapi.Url.makeWith(
            "./StepperWorker.js",
            ~base=[%mel.raw "import.meta.url"],
          ),
        );
      let listener = (event: Webapi.Dom.MessageEvent.t) => {
        let data: StepperResult.t = event |> Webapi.Dom.MessageEvent.data;
        setResult(_ => data);
      };
      worker |> Webapi.Dom.Worker.addMessageEventListener(listener);
      switch (result) {
      | StepperResult.Pending(expr) =>
        worker
        |> Webapi.Dom.Worker.postMessage({
             StepperWorkerMessage.expr,
             optimize: settings.optimize,
           })
      | _ => ()
      };
      setWorker(_ => Some(worker));
      Some(
        () =>
          worker |> Webapi.Dom.Worker.removeMessageEventListener(listener),
      );
    },
    [|trigger|],
  );

  let updateResult = (expr: Stepper.Expr.t) => {
    switch (worker, result) {
    | (None, _) =>
      setResult(_ => Pending(expr));
      setTrigger(trigger => trigger + 1);
    | (Some(worker), Value(_) | Expr(_) | Error(_) | Waiting) =>
      setResult(_ => Pending(expr));
      worker
      |> Webapi.Dom.Worker.postMessage({
           StepperWorkerMessage.expr,
           optimize: settings.optimize,
         });
    | (Some(worker), Pending(_)) =>
      setResult(_ => Pending(expr));
      worker |> Webapi.Dom.Worker.terminate();
      setTrigger(trigger => trigger + 1);
    };
  };

  let inputResult = (input: string) => {
    switch (Stepper.parse(input)) {
    | exception (Stepper.Lexer.Error(message)) =>
      setResult(_ => Error("Lexing error: " ++ message))
    | exception Stepper.Parser.Error
    | exception _
    | None => setResult(_ => Error("Syntax error"))
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
      setResult(_ => Error(message));
    | Expr(expr) => expr |> Stepper.compose(context) |> updateResult
    | Value(value) =>
      value
      |> Stepper.Value.to_expr
      |> Stepper.compose(context)
      |> updateResult
    };
  };

  <>
    <StepperReactInput
      value=input
      onChange={input => {
        Dom.Storage.localStorage
        |> Dom.Storage.setItem("stepper-input", input);
        setHistory(_ => []);
        setInput(_ => input);
        inputResult(input);
      }}
    />
    <StepperReactSettings
      value=settings
      onChange={newSettings => setSettings(oldSettings => {
        if (oldSettings.optimize != newSettings.optimize) {
          setTrigger(trigger => trigger + 1)
        }
        newSettings
      })}
    />
    <StepperReactHistory settings history={history->Belt.List.reverse} />
    <StepperReactOutput settings value=result onClick=onStep />
  </>;
};
