let () =
  switch (ReactDOM.querySelector("#root")) {
  | Some(root) => ReactDOM.Client.createRoot(root)->ReactDOM.Client.render(<App />)
  | None =>
    Js.Console.error("Failed to start React: couldn't find the #root element")
  };
