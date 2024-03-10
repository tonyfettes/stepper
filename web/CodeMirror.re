[@react.component] [@mel.module "@uiw/react-codemirror"]
external make:
  (~value: string, ~onChange: (string, 'a) => unit) => React.element =
  "default";
