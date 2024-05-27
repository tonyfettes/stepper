import { parser } from "./parser"
import { LRLanguage, LanguageSupport } from "@codemirror/language"
import { styleTags, tags as t } from "@lezer/highlight"

export const stepperLanguage = LRLanguage.define({
  name: "stepper",
  parser: parser.configure({
    props: [
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
