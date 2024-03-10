let () =
  switch (ReactDOM.querySelector("#root")) {
  | Some(root) => ReactDOM.Client.(createRoot(root)->render(<App />))
  | None =>
    Js.Console.error("Failed to start React: couldn't find the #root element")
  };
