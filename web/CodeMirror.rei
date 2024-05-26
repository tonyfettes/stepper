[@react.component]
let make:
  (~value: string, ~onChange: (string, 'a) => unit, ~basicSetup: Js.t('a), ~extensions: array(Js.t('a))) =>
  React.element;
