[@react.component]
let make:
  (
    ~settings: StepperReactSettings.t,
    ~context: Stepper.Context.t,
    ~expr: Stepper.Expr.t,
    ~onClick: (Stepper.Context.t, Stepper.Expr.t) => unit
  ) =>
  React.element;
