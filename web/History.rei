[@react.component]
let make:
  (
    ~settings: Settings.t,
    ~history: list((Stepper.Context.t, Stepper.Expr.t))
  ) =>
  React.element;
