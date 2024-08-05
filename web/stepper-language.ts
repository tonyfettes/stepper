import { parser } from "./stepper.grammar";
import { styleTags, tags as t } from "@lezer/highlight";
import { continuedIndent, indentNodeProp, LRLanguage, LanguageSupport } from "@codemirror/language";

const stepperLanguage = LRLanguage.define({
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

export function stepper() {
  return new LanguageSupport(stepperLanguage);
}
