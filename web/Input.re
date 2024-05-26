[@mel.module "./codemirror/stepper"]
external stepper: unit => Js.t('a) = "stepper";

[@react.component]
let make = (~value, ~onChange: string => unit) => {
  <div>
    <CodeMirror
      value
      onChange={(value, _) => onChange(value)}
      basicSetup={"lineNumbers": false, "foldGutter": false}
      extensions={[|stepper()|]}
    />
  </div>;
};
