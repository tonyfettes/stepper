[@react.component]
let make:
  (
    ~value: string,
    ~height: string,
    ~onChange: (string, 'a) => unit,
    ~options: MonacoOptions.t,
    ~onMount: ('editor, 'monaco) => unit
  ) =>
  React.element;
