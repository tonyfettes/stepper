type t = {showResidue: bool};

[@react.component]
let make: (~value: t, ~onChange: t => unit) => React.element;
