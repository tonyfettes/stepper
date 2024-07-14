[@react.component]
let make = (~settings: StepperReactSettings.t, ~history) => {
  let history =
    history
    ->Belt.List.mapWithIndex((i, (context, expr)) =>
        <StepperReactObject
          settings
          key={i->Belt.Int.toString}
          context
          expr
          onClick={(_, _) => ()}
        />
      )
    ->Belt.List.toArray;
  <ol> history->React.array </ol>;
};
