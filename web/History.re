[@react.component]
let make = (~history) => {
  <p>
    {history
     ->Belt.List.mapWithIndex((i, (ctx, exp)) => {
         <Object key={i->Belt.Int.toString} ctx exp onClick={(_, _) => ()} />
       })
     ->Belt.List.toArray
     ->React.array}
  </p>;
};
