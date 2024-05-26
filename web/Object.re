[@react.component]
let make =
    (
      ~settings: Settings.t,
      ~context: Stepper.Context.t,
      ~expr: Stepper.Expr.t,
      ~onClick,
    ) => {
  let onClick = _ => onClick(context, expr);
  let expr = {
    let buffer = Buffer.create(42);
    Stepper.Expr.pretty_print(expr)
    |> PPrint.ToBuffer.pretty(1.0, 64, buffer);
    Buffer.contents(buffer);
  };
  let segments = {
    let buffer = Buffer.create(42);
    context
    |> Stepper.Context.pretty_print(~residue=settings.showResidue)
    |> PPrint.ToBuffer.pretty(1.0, 64, buffer);
    buffer |> Buffer.contents |> String.split_on_char('@');
  };
  switch (segments) {
  | [prefix, suffix] =>
    <li className="whitespace-pre font-mono">
      prefix->React.string
      <span className="cursor-pointer text-blue-500" onClick>
        expr->React.string
      </span>
      suffix->React.string
    </li>
  | _ => failwith("Invalid context")
  };
};
