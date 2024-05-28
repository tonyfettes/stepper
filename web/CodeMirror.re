[@react.component] [@mel.module "@uiw/react-codemirror"]
external make:
  (
    ~value: string,
    ~onChange: (string, 'a) => unit,
    ~basicSetup: Js.t('a),
    ~extensions: array(Js.t('a))
  ) =>
  React.element =
  "default";
