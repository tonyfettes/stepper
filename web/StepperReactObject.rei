[@react.component]
let make:
  (
    ~settings: StepperReactSettings.t,
    ~context: Stepper.Ctx.t,
    ~expr: Stepper.Syntax.Exp.t,
    ~onClick: (Stepper.Ctx.t, Stepper.Syntax.Exp.t) => unit
  ) =>
  React.element;
