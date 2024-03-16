[@react.component]
let make = (~value, ~onChange: string => unit) => {
  <p><CodeMirror value onChange={(value, _) => {onChange(value)}} /></p>;
};
