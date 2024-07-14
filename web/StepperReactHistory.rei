[@react.component]
let make:
  (
    ~settings: StepperReactSettings.t,
    ~history: list((Stepper.Context.t, Stepper.Expr.t))
  ) =>
  React.element;
