/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./web/**/*.re"
  ],
  theme: {
    fontFamily: {
      'mono': "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Cascadia Code\", \"Source Code Pro\", monospace"
    },
    extend: {},
  },
  plugins: [
    require('@tailwindcss/typography'),
  ],
}
