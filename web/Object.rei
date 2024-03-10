[@react.component]
let make:
  (
    ~ctx: Stepper.Ctx.t,
    ~exp: Syntax.Exp.t,
    ~onClick: (Stepper.Ctx.t, Syntax.Exp.t) => unit
  ) =>
  React.element;
