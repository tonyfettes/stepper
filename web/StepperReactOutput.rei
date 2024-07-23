[@react.component]
let make:
  (
    ~settings: StepperReactSettings.t,
    ~value: StepperResult.t,
    ~onClick: (Stepper.Context.t, Stepper.Expr.t) => unit
  ) =>
  React.element;
