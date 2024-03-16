type t = {showResidue: bool};

let default = {showResidue: false};

[@react.component]
let make = (~value: t, ~onChange: t => unit): React.element => {
  <p>
    <Check
      id="showResidue"
      name="Show Residue (Do Statement)"
      value={value.showResidue}
      onChange={showResidue => onChange({showResidue: showResidue})}>
      "Show Residue (Do Statement)"->React.string
    </Check>
  </p>;
};
