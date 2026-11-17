WebWorker.dedicatedWorkerGlobalScope
|> WebWorker.DedicatedWorkerGlobalScope.addMessageEventListener(
     (event: WebWorker.MessageEvent.t) => {
     let data: StepperWorkerMessage.t = event |> WebWorker.MessageEvent.data;
     let result: StepperResult.t =
       switch (Stepper.Step.step(data.expr)) {
       | exception (Stepper.Step.Unbound_variable(expr) as exn)
       | exception (Stepper.Step.Mismatched_type(expr) as exn) =>
         Printf.sprintf(
           "%s: %s",
           Display.exp(expr),
           switch (exn) {
           | Stepper.Step.Unbound_variable(_) => "Unbound_variable"
           | Stepper.Step.Mismatched_type(_) => "Mismatched_type"
           | _ => ""
           },
         )
         ->Error
       | exception exn => Error(Printexc.to_string(exn))
       | Val(value) => Value(value)
       | Exp(objects) =>
         Expr(
           objects
           |> List.map((obj: Stepper.Step.obj) => (obj.ctx, obj.exp)),
         )
       };
     WebWorker.dedicatedWorkerGlobalScope
     |> WebWorker.DedicatedWorkerGlobalScope.postMessage({
          StepperWorkerMessage.id: data.id,
          result,
        });
   });
