import * as React from 'react';

const LazyCodeMirror = React.lazy(async () => {
  const CodeMirror = await import("@uiw/react-codemirror");
  const { parser } = await import("./stepper.grammar");
  const { styleTags, tags: t } = await import("@lezer/highlight");
  const { continuedIndent, indentNodeProp, LRLanguage, LanguageSupport } = await import("@codemirror/language");
  const language = LRLanguage.define({
    name: "stepper",
    parser: parser.configure({
      props: [
        indentNodeProp.add({
          "If": continuedIndent({ except: /^\s*(else\b)/ }),
          "Function LetStatement LetRecStatement": continuedIndent()
        }),
        styleTags({
          "let rec in fun": t.definitionKeyword,
          "if then else eval hide pause debug filter": t.controlKeyword,
          Identifier: t.variableName,
          Comment: t.blockComment,
          ArithOp: t.arithmeticOperator,
          Integer: t.integer,
          Boolean: t.bool,
          "( )": t.paren,
        })
      ]
    }),
    languageData: {
      commentTokens: { block: { open: "#", close: "#" } },
    }
  });
  const LazyCodeMirror = ({ value, onChange, basicSetup }) => {
    return (
      <CodeMirror.default
        value={value}
        onChange={onChange}
        basicSetup={basicSetup}
        extensions={[new LanguageSupport(language)]}
      />
    );
  };
  return { default: LazyCodeMirror };
});

const Input = ({ value, onChange }) => {
  return (
    <React.Suspense fallback={<p>Loading...</p>}>
      <LazyCodeMirror value={value} onChange={onChange} basicSetup={{ lineNumbers: false, foldGutter: false }} />
    </React.Suspense>
  );
};

export default Input
