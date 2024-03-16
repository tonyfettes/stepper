[@react.component]
let make =
    (
      ~settings: Settings.t,
      ~value: [<
         | `Error(string)
         | `Expr(list((Stepper.Context.t, Stepper.Expr.t)))
         | `Value(Stepper.Value.t)
       ],
      ~onClick: (Stepper.Context.t, Stepper.Expr.t) => unit,
    ) => {
  switch (value) {
  | `Error(value) =>
    <div className="whitespace-pre font-mono text-red-500">
      value->React.string
    </div>
  | `Value(value) =>
    <div className="whitespace-pre font-mono text-green-500">
      {value->Stepper.Value.to_string->React.string}
    </div>
  | `Expr(expr) =>
    expr
    ->Belt.List.mapWithIndex((i, (context, expr)) =>
        <Object settings key={i->Belt.Int.toString} context expr onClick />
      )
    ->Belt.List.toArray
    ->React.array
  };
};
