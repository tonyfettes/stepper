[@react.component]
let make =
    (
      ~settings: StepperReactSettings.t,
      ~context: Stepper.Ctx.t,
      ~expr: Stepper.Syntax.Exp.t,
      ~onClick,
    ) => {
  let onClick = _ => onClick(context, expr);
  let (prefix, middle, suffix) = {
    let string =
      context |> Display.context(~residue=settings.showResidue, ~expr);
    switch (string |> Js.String.split(~sep="{")) {
    | [|prefix, string|] =>
      switch (string |> Js.String.split(~sep="}")) {
      | [|middle, suffix|] => (prefix, middle, suffix)
      | _ => failwith(Printf.sprintf("Invalid context: %s", string))
      }
    | _ => failwith(Printf.sprintf("Invalid context: %s", string))
    };
  };
  <li className="whitespace-pre font-mono">
    prefix->React.string
    <span className="cursor-pointer text-blue-500" onClick>
      middle->React.string
    </span>
    suffix->React.string
  </li>;
};
