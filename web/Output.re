[@react.component]
let make =
    (
      ~settings: Settings.t,
      ~value: [<
         | `Error(string)
         | `Expr(list((Stepper.Context.t, Stepper.Expr.t)))
         | `Value(Stepper.Value.t)
       ],
      ~onClick: (Stepper.Context.t, Stepper__.Syntax.Expr.t) => unit,
    ) => {
  switch (value) {
  | `Error(value) =>
    <p className="whitespace-pre font-mono text-red-500">
      value->React.string
    </p>
  | `Value(value) =>
    let buffer = Buffer.create(42);
    value |> Stepper.Value.pretty_print |> Stepper.Printer.to_buffer(buffer);
    <p className="whitespace-pre font-mono text-green-500">
      {buffer->Buffer.contents->React.string}
    </p>;
  | `Expr(expr) =>
    let futures =
      expr
      ->Belt.List.mapWithIndex((i, (context, expr)) =>
          <Object settings key={i->Belt.Int.toString} context expr onClick />
        )
      ->Belt.List.toArray;
    <ul> futures->React.array </ul>;
  };
};
