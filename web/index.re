module Object = {
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
};

module Result = {
  [@react.component]
  let make = (~value, ~onClick) => {
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
};

let (|->) = (x, f) => {
  f(x);
  x;
};

module CodeMirror = {
  [@react.component] [@mel.module "@uiw/react-codemirror"]
  external make:
    (~value: string, ~onChange: (string, 'a) => unit) => React.element =
    "default";
};

let setLocalStage: string => unit = [%mel.raw
  {|function (value) { window.localStorage.setItem('input', value); }|}
];

let getLocalStage: unit => string = [%mel.raw
  {|function () { return window.localStorage.getItem('input') || ''; }|}
];

module Input = {
  [@react.component]
  let make = (~value, ~onChange: string => unit) => {
    <CodeMirror value onChange={(value, _) => {onChange(value)}} />;
  };
};

module History = {
  [@react.component]
  let make = (~history) => {
    <div>
      {history
       ->Belt.List.mapWithIndex((i, (ctx, exp)) => {
           <Object
             key={i->Belt.Int.toString}
             ctx
             exp
             onClick={(_, _) => ()}
           />
         })
       ->Belt.List.toArray
       ->React.array}
    </div>;
  };
};

module Stepper = {
  // This sample forces an import of Belt.*, so that CI builds can ensure that
  // Melange has been installed correctly for JS bundlers to be able to find it.
  [@react.component]
  let make = () => {
    let (input, setInput) = React.useState(() => "");
    let (history, setHistory) = React.useState(() => []);
    let (result, setResult) = React.useState(() => `Exp([]));

    let updateResult = input => {
      print_endline("source: " ++ input);
      let buf = Lexing.from_string(input);
      switch (Parser.top(Lexer.lex, buf)) {
      | exception (Lexer.Error(message)) =>
        setResult(_ => `Err("Lexing error: " ++ message))
      | exception Parser.Error
      | None => setResult(_ => `Err("Syntax error"))
      | Some(exp) =>
        let result =
          switch (Stepper.step(exp)) {
          | `Err(err, exp) =>
            `Err(
              Printf.sprintf(
                "%s: %s",
                exp->Syntax.Exp.to_string,
                err->Stepper.Err.to_string,
              ),
            )
          | `Val(exp) => `Val(exp)
          | `Exp(exp) => `Exp(exp)
          };
        setResult(_ => result);
      };
    };

    React.useEffect0(() => {
      Dom.Storage.(localStorage |> getItem("stepper-input"))
      ->Belt.Option.forEach(input => {
          setInput(_ => input);
          updateResult(input);
        });
      None;
    });

    let onStep = (ctx, exp) => {
      setHistory(history => [(ctx, exp), ...history]);
      switch (Stepper.transition(exp)) {
      | `Err(err, exp) =>
        let message =
          Printf.sprintf(
            "%s: %s",
            exp->Syntax.Exp.to_string,
            err->Stepper.Err.to_string,
          );
        setResult(_ => `Err(message));
      | `Exp(exp)
      | `Val(exp) =>
        let exp = Stepper.Ctx.compose(ctx, exp);
        let exp = exp |> Syntax.Exp.to_string(~residue=true);
        updateResult(exp);
      };
    };

    <div>
      <Input
        value=input
        onChange={input => {
          Dom.Storage.(localStorage |> setItem("stepper-input", input));
          setHistory(_ => []);
          setInput(_ => input);
          updateResult(input);
        }}
      />
      <hr />
      <History history={history->List.rev} />
      <hr />
      <Result value=result onClick=onStep />
    </div>;
  };
};

module App = {
  [@react.component]
  let make = () => {
    <div className="p-8">
      <h1> "Mini Stepper"->React.string </h1>
      <p>
        <ul className="gap-1">
          <li> <a href="https://hazel.org"> {React.string("Home")} </a> </li>
          <li>
            <a href="https://hazel.org/buil/dev"> "Try"->React.string </a>
          </li>
          <li>
            <a href="https://github.com/hazelgrove/hazel">
              "GitHub"->React.string
            </a>
          </li>
        </ul>
      </p>
      <Stepper />
    </div>;
  };
};

switch (ReactDOM.querySelector("#root")) {
| Some(root) => ReactDOM.Client.(createRoot(root)->render(<App />))
| None =>
  Js.Console.error("Failed to start React: couldn't find the #root element")
};
