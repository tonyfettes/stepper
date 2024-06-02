[@react.component] [@mel.module "./Input"]
external make:
  (
    ~value: string,
    ~onChange: string => unit,
  ) =>
  React.element =
  "default";
