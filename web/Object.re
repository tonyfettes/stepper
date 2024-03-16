[@react.component]
let make =
    (
      ~settings: Settings.t,
      ~context: Stepper.Context.t,
      ~expr: Stepper.Expr.t,
      ~onClick,
    ) => {
  let segments =
    context |> Stepper.Context.to_string(~residue=settings.showResidue) |> String.split_on_char('@');
  let onClick = _ => onClick(context, expr);
  Js.log(settings);
  let expr = Stepper.Expr.to_string(~residue=settings.showResidue, expr);
  Js.log("expr: " ++ expr);
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
