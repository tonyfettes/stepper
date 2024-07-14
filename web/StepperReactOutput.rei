[@react.component]
let make:
  (
    ~settings: StepperReactSettings.t,
    ~value: [<
              | `Waiting
              | `Pending(Stepper.Expr.t)
              | `Error(string)
              | `Expr(list((Stepper.Context.t, Stepper.Expr.t)))
              | `Value(Stepper.Value.t)
            ],
    ~onClick: (Stepper.Context.t, Stepper.Expr.t) => unit
  ) =>
  React.element;
