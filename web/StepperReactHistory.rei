[@react.component]
let make:
  (
    ~settings: StepperReactSettings.t,
    ~history: list((Stepper.Ctx.t, Stepper.Syntax.Exp.t))
  ) =>
  React.element;
