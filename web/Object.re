[@react.component]
let make =
    (
      ~settings: Settings.t,
      ~context: Stepper.Context.t,
      ~expr: Stepper.Expr.t,
      ~onClick,
    ) => {
  let onClick = _ => onClick(context, expr);
  let (prefix, middle, suffix) = {
    let buffer = Buffer.create(42);
    context
    |> Stepper.Context.pretty_print(~residue=settings.showResidue, ~expr)
    |> Stepper.Printer.to_buffer(buffer);
    let string = buffer |> Buffer.contents;
    Js.log(string);
    switch (string |> String.split_on_char('{')) {
    | [prefix, string] =>
      switch (string |> String.split_on_char('}')) {
      | [middle, suffix] => (prefix, middle, suffix)
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
