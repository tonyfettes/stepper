type t = {
  showResidue: bool,
  optimize: bool,
};

[@react.component]
let make = (~value: t, ~onChange: t => unit): React.element => {
  <p>
    <StepperReactCheckbox
      id="showResidue"
      name="Show Residue (Do Statement)"
      value={value.showResidue}
      onChange={showResidue => onChange({...value, showResidue})}>
      "Show Residue (Do Statement)"->React.string
    </StepperReactCheckbox>
    <StepperReactCheckbox
      id="optimize"
      name="Optimize"
      value={value.optimize}
      onChange={optimize => onChange({...value, optimize})}>
      "Optimize"->React.string
    </StepperReactCheckbox>
  </p>;
};
