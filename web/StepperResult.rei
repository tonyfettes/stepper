type t =
  | Pending(Stepper.Expr.t)
  | Waiting
  | Expr(list((Stepper.Context.t, Stepper.Expr.t)))
  | Value(Stepper.Value.t)
  | Error(string);

let toString: t => string;
