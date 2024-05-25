[@react.component]
let make = (~value, ~onChange: string => unit) => {
  let editorRef = React.useRef(None);
  let divRef = React.useRef(Js.Nullable.null);
  let updateHeight = () => {
    switch (editorRef.current) {
    | None => ()
    | Some(editor) =>
      let height = editor##getContentHeight();
      let width =
        switch (divRef.current->Js.Nullable.toOption) {
        | None => 300
        | Some(div) => div |> Webapi.Dom.Element.clientWidth
        };
      editor##layout({"width": width, "height": height});
    };
  };
  <div ref={ReactDOM.Ref.domRef(divRef)}>
    <Monaco
      value
      onChange={(value, _) => onChange(value)}
      options={
        "minimap": {
          "enabled": false,
        },
        "lineNumbers": "off",
        "folding": false,
        "scrollBeyondLastLine": false,
        "fontSize": "20px",
        "fontFamily": "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Source Code Pro\", monospace",
        "overviewRulerLanes": 0,
      }
      onMount={(editor, monaco) => {
        ignore(monaco);
        editorRef.current = Some(editor);
        editor##onDidContentSizeChange(updateHeight);
        updateHeight();
      }}
    />
  </div>;
};
