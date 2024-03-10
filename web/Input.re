[@react.component]
let make = (~value, ~onChange: string => unit) => {
  <CodeMirror value onChange={(value, _) => {onChange(value)}} />;
};
