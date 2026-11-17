[@react.component]
let make =
    (
      ~id: string,
      ~name: string,
      ~value: bool,
      ~onChange: bool => unit,
      ~children: React.element,
    ) => {
  <span className="flex gap-2 items-baseline">
    <input
      type_="checkbox"
      id
      name
      checked=value
      onChange={e => React.Event.Form.(e->target)##checked->onChange}
    />
    <label htmlFor=id> children </label>
  </span>;
};
