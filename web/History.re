[@react.component]
let make = (~settings: Settings.t, ~history) => {
  let history =
    history
    ->Belt.List.mapWithIndex((i, (context, expr)) =>
        <Object
          settings
          key={i->Belt.Int.toString}
          context
          expr
          onClick={(_, _) => ()}
        />
      )
    ->Belt.List.toArray;
  <p> history->React.array </p>;
};
