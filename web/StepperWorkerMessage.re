type t = {
  id: int,
  expr: Stepper.Syntax.Exp.t,
};

type response = {
  id: int,
  result: StepperResult.t,
};
