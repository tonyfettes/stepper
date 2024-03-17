[@react.component]
let make =
    (
      ~settings: Settings.t,
      ~context: Stepper.Context.t,
      ~expr: Stepper.Expr.t,
      ~onClick,
    ) => {
  let buffer = Buffer.create(128);
  context
  |> Stepper.Context.pretty_print(~residue=settings.showResidue)
  |> PPrint.ToBuffer.pretty(1.0, 64, buffer);
  let onClick = _ => onClick(context, expr);
  let expr = Stepper.Expr.to_string(~residue=settings.showResidue, expr);
  let segments = buffer |> Buffer.contents |> String.split_on_char('@');
  switch (segments) {
  | [prefix, suffix] =>
    <div className="whitespace-pre font-mono">
      prefix->React.string
      <span className="cursor-pointer text-blue-500" onClick>
        expr->React.string
      </span>
      suffix->React.string
    </div>
  | _ => failwith("Invalid context")
  };
};
