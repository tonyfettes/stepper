type t = {showResidue: bool};

let default: t;

[@react.component]
let make: (~value: t, ~onChange: t => unit) => React.element;
