import { parser } from "./parser"
import { LRLanguage, LanguageSupport } from "@codemirror/language"
import { styleTags, tags as t } from "@lezer/highlight"

export const stepperLanguage = LRLanguage.define({
  name: "stepper",
  parser: parser.configure({
    props: [
      styleTags({
        keyword: t.keyword
      })
    ]
  }),
  languageData: {
  }
});

export function stepper() {
  return new LanguageSupport(stepperLanguage);
}
