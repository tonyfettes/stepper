[@mel.module "./codemirror/stepper"]
external stepper: unit => Js.t('a) = "stepper";

[@react.component]
let make = (~value, ~onChange: string => unit) => {
  <React.Suspense fallback={<p> "Loading..."->React.string </p>}>
    <LazyCodeMirror
      value
      onChange={(value, _) => onChange(value)}
      basicSetup={"lineNumbers": false, "foldGutter": false}
      extensions=[|stepper()|]
    />
  </React.Suspense>;
};
