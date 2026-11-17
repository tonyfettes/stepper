type t =
  | Pending(Stepper.Syntax.Exp.t)
  | Waiting
  | Expr(list((Stepper.Ctx.t, Stepper.Syntax.Exp.t)))
  | Value(Stepper.Syntax.Exp.t)
  | Error(string);

let toString: t => string;
