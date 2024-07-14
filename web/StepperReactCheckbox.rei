[@react.component]
let make:
  (
    ~id: string,
    ~name: string,
    ~value: bool,
    ~onChange: bool => unit,
    ~children: React.element
  ) =>
  React.element;
