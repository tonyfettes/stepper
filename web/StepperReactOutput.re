[@react.component]
let make =
    (
      ~settings: StepperReactSettings.t,
      ~value: StepperResult.t,
      ~onClick: (Stepper.Context.t, Stepper.Syntax.Expr.t) => unit,
    ) =>
  switch (value) {
  | Waiting =>
    <p className="whitespace-pre font-mono text-gray-500">
      "Waiting for input..."->React.string
    </p>
  | Pending(_) =>
    <p className="whitespace-pre font-mono text-gray-500">
      "Pending"->React.string
    </p>
  | Error(value) =>
    <p className="whitespace-pre font-mono text-red-500">
      value->React.string
    </p>
  | Value(value) =>
    let buffer = Buffer.create(42);
    value |> Stepper.Value.pretty_print |> Stepper.Printer.to_buffer(buffer);
    <p className="whitespace-pre font-mono text-green-500">
      {buffer->Buffer.contents->React.string}
    </p>;
  | Expr(expr) =>
    let futures =
      expr
      ->Belt.List.mapWithIndex((i, (context, expr)) =>
          <StepperReactObject
            settings
            key={i->Belt.Int.toString}
            context
            expr
            onClick
          />
        )
      ->Belt.List.toArray;
    <ul> futures->React.array </ul>;
  };
