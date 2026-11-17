type t =
  | Pending(Stepper.Syntax.Exp.t)
  | Waiting
  | Expr(list((Stepper.Ctx.t, Stepper.Syntax.Exp.t)))
  | Value(Stepper.Syntax.Exp.t)
  | Error(string);

let toString = (result: t) => {
  switch (result) {
  | Pending(expr) =>
    Printf.sprintf("Pending(%s)", Display.exp(expr))
  | Waiting => Printf.sprintf("Waiting")
  | Expr(expr) =>
    let buffer = Buffer.create(42);
    buffer->Buffer.add_string("Expr(");
    expr->Belt.List.forEach(((ctx, expr)) => {
      buffer->Buffer.add_string(
        Printf.sprintf(
          "(%s, %s)",
          ctx->Stepper.Print.ctx,
          expr->Display.exp,
        ),
      )
    });
    buffer->Buffer.add_char(')');
    buffer->Buffer.contents;
  | Value(value) =>
    Printf.sprintf("Value(%s)", value->Display.exp)
  | Error(message) => Printf.sprintf("Error(%s)", message)
  };
};
