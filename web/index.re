let () =
  switch (ReactDOM.querySelector("#root")) {
  | Some(root) =>
    ReactDOM.Client.createRoot(root)
    ->ReactDOM.Client.render(
        <React.StrictMode>
          <div className="p-8">
            <article className="prose lg:prose-xl">
              <h1> "Hazel Mini Stepper"->React.string </h1>
              <StepperReact />
            </article>
          </div>
        </React.StrictMode>,
      )
  | None =>
    Js.Console.error("Failed to start React: couldn't find the #root element")
  };
