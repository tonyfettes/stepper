[@react.component]
let make:
  (
    ~settings: Settings.t,
    ~context: Stepper.Context.t,
    ~expr: Stepper.Expr.t,
    ~onClick: (Stepper.Context.t, Stepper.Expr.t) => unit
  ) =>
  React.element;
