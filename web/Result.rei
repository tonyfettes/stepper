[@react.component]
let make:
  (
    ~settings: Settings.t,
    ~value: [<
              | `Error(string)
              | `Expr(list((Stepper.Context.t, Stepper.Expr.t)))
              | `Value(Stepper.Value.t)
            ],
    ~onClick: (Stepper.Context.t, Stepper.Expr.t) => unit
  ) =>
  React.element;
