module Object = {
  [@react.component]
  let make = (~ctx, ~exp, ~onClick) => {
    <div onClick={(_) => onClick(ctx, exp)}>
      {ctx->Evaluator.Ctx.to_string->React.string}
      {exp->Syntax.Exp.to_string->React.string}
    </div>
  }
}

module Result = {
  [@react.component]
  let make = (~value, ~onClick) => {
    <div>{
      switch (value) {
      | `Err(value) =>
        value->React.string
      | `Ok(value) =>
        value
        ->Belt.List.map(((ctx, exp)) => {
          <Object ctx={ctx} exp={exp} onClick={(_, _) => {
            switch (Evaluator.transition(exp)) {
            | `Err(err, _) => onClick(`Err(Evaluator.Err.to_string(err)))
            | `Exp(exp)
            | `Val(exp) =>
              let exp = Evaluator.Ctx.compose(ctx, exp);
              onClick(`Ok(exp))
            }
          }} />
        })
        ->Belt.List.toArray
        ->React.array
      }
    }</div>
  }
}

module Input = {
  [@react.component]
  let make = (~value, ~onChange: string => unit) => {
    <input
      type_="text"
      value={value}
      onChange={e => {
        let source = e->ReactEvent.Form.target##value;
        onChange(source);
      }}
    />
  }
}

module History = {
  [@react.component]
  let make = (~history) => {
    <div>
      {history->Belt.List.map(exp => {
        <div>
          {exp->Syntax.Exp.to_string->React.string}
        </div>
      })->Belt.List.toArray->React.array}
    </div>
  }
}

module App = {
  // This sample forces an import of Belt.*, so that CI builds can ensure that
  // Melange has been installed correctly for JS bundlers to be able to find it.
  [@react.component]
  let make = () => {
    let (input, setInput) = React.useState(() => "");
    let (history, setHistory): (list(Syntax.Exp.t), _) = React.useState(() => []);
    let (result, setResult) = React.useState(() => `Ok([]));

    let updateResult = input => {
        print_endline("source: " ++ input);
        let buf = Lexing.from_string(input);
        switch (Parser.top(Lexer.lex, buf)) {
        | exception Lexer.Error(message) =>
          setResult(_ => `Err("Lexing error: " ++ message))
        | exception Parser.Error | None =>
          setResult(_ => `Err("Syntax error"))
        | Some(ast) =>
          setResult(_ => `Ok(Evaluator.step(ast)))
        };
      };

    let onStep = exp => {
      switch exp {
      | `Err(_) => ()
      | `Ok(exp) =>
        setHistory(history => [exp, ...history]);
        setInput(_ => {
          let exp = exp->Syntax.Exp.to_string;
          updateResult(exp);
          exp
        });
      }
    };

    <div>
      <Input value=input onChange={input => {
        setInput(_ => input);
        updateResult(input);
      }} />
      <hr />
      <History history={history->List.rev} />
      <hr />
      <Result value={result} onClick={onStep} />
    </div>
  }
};

switch (ReactDOM.querySelector("#root")) {
  | Some(root) => ReactDOM.render(<App />, root)
  | None =>
    Js.Console.error(
      "Failed to start React: couldn't find the #root element",
    )
};
