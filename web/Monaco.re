[@react.component] [@mel.module "@monaco-editor/react"]
external make:
  (
    ~value: string,
    ~onChange: (string, 'a) => unit,
    ~options: Js.t('a),
    ~onMount: ('editor, 'monaco) => unit
  ) =>
  React.element =
  "default";
