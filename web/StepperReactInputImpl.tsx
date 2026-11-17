import * as React from 'react';

const LazyCodeMirror = React.lazy(async () => {
  const CodeMirror = await import("@uiw/react-codemirror");
  const { stepper } = await import("./stepper-language");
  const LazyCodeMirror = ({ value, onChange, basicSetup }) => {
    return (
      <CodeMirror.default
        value={value}
        onChange={onChange}
        basicSetup={basicSetup}
        extensions={[stepper()]}
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
