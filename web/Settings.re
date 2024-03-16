type t = {showResidue: bool};

let default = {showResidue: false};

[@react.component]
let make = (~value: t, ~onChange: t => unit): React.element => {
  <div>
    <label>
      {React.string("Show residue")}
      <Check
        value={value.showResidue}
        onChange={showResidue => onChange({showResidue: showResidue})}
      />
    </label>
  </div>;
};
