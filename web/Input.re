[@react.component]
let make = (~value, ~onChange: string => unit) => {
  let editorRef = React.useRef(None);
  let (height, setHeight) = React.useState(() => 20);
  <p className={Printf.sprintf("h-[%dpx]", height)}>
    <Monaco
      height={Printf.sprintf("%dpx", height)}
      value
      onChange={(value, _) => {
        switch (editorRef.current) {
        | None => ()
        | Some(editor) =>
          let height = editor##getContentHeight();
          setHeight(_ => height);
        };
        onChange(value);
      }}
      options={
        minimap: {
          enabled: Some(false),
        },
        lineNumbers: Some("off"),
        scrollBeyondLastLine: Some(false),
      }
      onMount={(editor, monaco) => {
        ignore(monaco);
        editorRef.current = Some(editor);
      }}
    />
  </p>;
  // <p> <CodeMirror value onChange={(value, _) => {onChange(value)}} /> </p>;
};
