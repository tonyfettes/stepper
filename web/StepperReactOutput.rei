[@react.component]
let make:
  (
    ~settings: StepperReactSettings.t,
    ~value: StepperResult.t,
    ~onClick: (Stepper.Ctx.t, Stepper.Syntax.Exp.t) => unit
  ) =>
  React.element;
