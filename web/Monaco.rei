[@react.component]
let make:
  (
    ~value: string,
    ~onChange: (string, 'a) => unit,
    ~options: Js.t('a),
    ~onMount: ('editor, 'monaco) => unit
  ) =>
  React.element;
