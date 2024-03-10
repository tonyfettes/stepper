[@react.component]
let make:
  (
    ~value: [<
              | `Err(string)
              | `Exp(list((Stepper.Ctx.t, Syntax.Exp.t)))
              | `Val(Syntax.Exp.t)
            ],
    ~onClick: (Stepper.Ctx.t, Syntax.Exp.t) => unit
  ) =>
  React.element;
