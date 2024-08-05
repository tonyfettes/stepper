type t = {
  showResidue: bool,
  optimize: bool,
};

[@react.component]
let make: (~value: t, ~onChange: t => unit) => React.element;
