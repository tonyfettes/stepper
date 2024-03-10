[@react.component]
let make =
    (
      ~value: [<
         | `Err(string)
         | `Exp(list((Stepper.Ctx.t, Syntax.Exp.t)))
         | `Val(Syntax.Exp.t)
       ],
      ~onClick: (Stepper.Ctx.t, Syntax.Exp.t) => unit,
    ) => {
  switch (value) {
  | `Err(value) =>
    <div className="whitespace-pre font-mono text-red-500">
      value->React.string
    </div>
  | `Val(value) =>
    <div className="whitespace-pre font-mono text-green-500">
      {value->Syntax.Exp.to_string->React.string}
    </div>
  | `Exp(value) =>
    value
    ->Belt.List.map(((ctx, exp)) => {
        <Object key={Js.Math.random()->string_of_float} ctx exp onClick />
      })
    ->Belt.List.toArray
    ->React.array
  };
};
