[@react.component] [@mel.module "./LazyCodeMirror"]
external make:
  (
    ~value: string,
    ~onChange: (string, 'a) => unit,
    ~basicSetup: Js.t('a),
    ~extensions: array(Js.t('a))
  ) =>
  React.element =
  "default";
