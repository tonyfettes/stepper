[@react.component] [@mel.module "@monaco-editor/react"]
external make:
  (
    ~value: string,
    ~height: string,
    ~onChange: (string, 'a) => unit,
    ~options: MonacoOptions.t,
    ~onMount: ('editor, 'monaco) => unit
  ) =>
  React.element =
  "default";
