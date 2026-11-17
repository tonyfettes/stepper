[@react.component]
let make = () => {
  let (input, setInput) = React.useState(() => "");
  let (history, setHistory) = React.useState(() => []);
  let (result, setResult) = React.useState(() => StepperResult.Waiting);
  let (settings, setSettings) =
    React.useState(() => {StepperReactSettings.showResidue: false});
  let workerRef = React.useRef(None: option(WebWorker.Worker.t));
  let requestId = React.useRef(0);
  let workerGeneration = React.useRef(0);
  let pending = React.useRef(false);
  let resultRef = React.useRef(StepperResult.Waiting);

  let setCurrentResult = value => {
    resultRef.current = value;
    setResult(_ => value);
  };

  let stopWorker = () => {
    requestId.current = requestId.current + 1;
    workerGeneration.current = workerGeneration.current + 1;
    pending.current = false;
    workerRef.current |> Option.iter(WebWorker.Worker.terminate);
    workerRef.current = None;
  };

  let updateResult = (expr: Stepper.Syntax.Exp.t) => {
    if (pending.current) {
      stopWorker();
    };
    requestId.current = requestId.current + 1;
    let id = requestId.current;
    setCurrentResult(Pending(expr));
    pending.current = true;
    try({
      let worker =
        switch (workerRef.current) {
        | Some(worker) => worker
        | None =>
          let generation = workerGeneration.current;
          let worker =
            WebWorker.Worker.makeWithUrl(
              Webapi.Url.makeWith(
                "./StepperWorker.re",
                ~base=[%mel.raw "import.meta.url"],
              ),
              {"type": "module"},
            );
          worker
          |> WebWorker.Worker.addMessageEventListener(
               (event: WebWorker.MessageEvent.t) => {
               let data: StepperWorkerMessage.response =
                 event |> WebWorker.MessageEvent.data;
               if (
                 generation == workerGeneration.current
                 && data.id == requestId.current
               ) {
                 pending.current = false;
                 setCurrentResult(data.result);
               };
             });
          worker
          |> WebWorker.Worker.addErrorEventListener(
               (event: WebWorker.ErrorEvent.t) => {
               if (generation == workerGeneration.current) {
                 stopWorker();
                 setCurrentResult(Error(WebWorker.ErrorEvent.message(event)));
               };
             });
          workerRef.current = Some(worker);
          worker
        };
      worker
      |> WebWorker.Worker.postMessage({StepperWorkerMessage.id, expr});
    }) {
    | exn =>
      stopWorker();
      setCurrentResult(Error(Printexc.to_string(exn)));
    };
  };

  let inputResult = (input: string) => {
    switch (Stepper.Parse.parse(input)) {
    | exception _
    | None =>
      stopWorker();
      setCurrentResult(Error("Syntax error"));
    | Some(expr) => updateResult(expr)
    };
  };

  React.useEffect0(() => {
    try({
      Dom.Storage.localStorage
      |> Dom.Storage.getItem("stepper-input")
      |> Option.iter(input => {
           setInput(_ => input);
           inputResult(input);
         });
    }) {
    | _ => ()
    };
    Some(() => stopWorker());
  });

  let onStep = (context, expr) => {
    let current =
      switch (resultRef.current) {
      | Expr(objects) =>
        List.exists(((c, e)) => c === context && e === expr, objects)
      | _ => false
      };
    if (!pending.current && current) {
      setHistory(history => [(context, expr), ...history]);
      switch (Stepper.Step.take({ctx: context, exp: expr})) {
      | exception (Stepper.Step.Unbound_variable(expr) as exn)
      | exception (Stepper.Step.Mismatched_type(expr) as exn) =>
        let message =
          Printf.sprintf(
            "%s: %s",
            Display.exp(expr),
            switch (exn) {
            | Stepper.Step.Unbound_variable(_) => "Unbound_variable"
            | Stepper.Step.Mismatched_type(_) => "Mismatched_type"
            | _ => ""
            },
          );
        setCurrentResult(Error(message));
      | exception exn => setCurrentResult(Error(Printexc.to_string(exn)))
      | expr => updateResult(expr)
      };
    };
  };

  <>
    <StepperReactInput
      value=input
      onChange={input => {
        try({
          Dom.Storage.localStorage
          |> Dom.Storage.setItem("stepper-input", input);
        }) {
        | _ => ()
        };
        setHistory(_ => []);
        setInput(_ => input);
        inputResult(input);
      }}
    />
    <StepperReactSettings
      value=settings
      onChange={newSettings => setSettings(_ => newSettings)}
    />
    <StepperReactHistory settings history={history->Belt.List.reverse} />
    <StepperReactOutput settings value=result onClick=onStep />
  </>;
};
