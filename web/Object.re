[@react.component]
let make = (~ctx, ~exp, ~onClick) => {
  Stepper.Ctx.to_string(ctx)
  |> String.split_on_char('@')
  |> (
    fun
    | [prefix, suffix] =>
      <div className="whitespace-pre font-mono">
        prefix->React.string
        <span
          className="cursor-pointer text-blue-500"
          onClick={_ => onClick(ctx, exp)}>
          {exp->Syntax.Exp.to_string->React.string}
        </span>
        suffix->React.string
      </div>
    | _ => failwith("Invalid context")
  );
};
