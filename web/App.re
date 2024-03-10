[@react.component]
let make = () => {
  <div className="p-8">
    <article className="prose lg:prose-xl">
      <h1> "Hazel Mini Stepper"->React.string </h1>
      <ul>
        <li>
          <a href="https://hazel.org">
            "Project Homepage (Hazel)"->React.string
          </a>
        </li>
        <li>
          <a href="https://github.com/hazelgrove/hazel">
            "GitHub"->React.string
          </a>
        </li>
      </ul>
      <Editor />
    </article>
  </div>;
};
