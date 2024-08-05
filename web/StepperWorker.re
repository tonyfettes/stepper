Webapi.Dom.Window.setOnMessage(
  [%mel.raw "self"],
  (event: Webapi.Dom.MessageEvent.t) => {
    let data: StepperWorkerMessage.t =
      event |> Webapi.Dom.MessageEvent.data;
    let result: StepperResult.t =
      switch (Stepper.step(~opt=data.optimize, data.expr)) {
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
        ->Error;
      | Value(value) => Value(value)
      | Expr(expr) => Expr(expr)
      };
    [%mel.raw "self"] |> Webapi.Dom.Window.postMessage(result);
  },
);
