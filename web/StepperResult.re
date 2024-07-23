type t =
  | Pending(Stepper.Expr.t)
  | Waiting
  | Expr(list((Stepper.Context.t, Stepper.Expr.t)))
  | Value(Stepper.Value.t)
  | Error(string);

let toString = (result: t) => {
  switch (result) {
  | Pending(expr) =>
    Printf.sprintf("Pending(%s)", Stepper.Expr.to_string(expr))
  | Waiting => Printf.sprintf("Waiting")
  | Expr(expr) =>
    let buffer = Buffer.create(42);
    buffer->Buffer.add_string("Expr(");
    expr->Belt.List.forEach(((ctx, expr)) => {
      buffer->Buffer.add_string(
        Printf.sprintf(
          "(%s, %s)",
          ctx->Stepper.Context.to_string,
          expr->Stepper.Expr.to_string,
        ),
      )
    });
    buffer->Buffer.add_char(')');
    buffer->Buffer.contents;
  | Value(value) =>
    Printf.sprintf("Value(%s)", value->Stepper.Value.to_string)
  | Error(message) => Printf.sprintf("Error(%s)", message)
  };
};
