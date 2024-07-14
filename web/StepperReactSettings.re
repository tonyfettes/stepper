type t = {showResidue: bool};

[@react.component]
let make = (~value: t, ~onChange: t => unit): React.element => {
  <p>
    <StepperReactCheckbox
      id="showResidue"
      name="Show Residue (Do Statement)"
      value={value.showResidue}
      onChange={showResidue => onChange({showResidue: showResidue})}>
      "Show Residue (Do Statement)"->React.string
    </StepperReactCheckbox>
  </p>;
};
