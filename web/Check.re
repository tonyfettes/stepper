[@react.component]
let make = (~value: bool, ~onChange: bool => unit) => {
  <input
    type_="checkbox"
    checked=value
    onChange={e => React.Event.Form.(e->target)##checked->onChange}
  />;
};
