/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./web/**/*.re"
  ],
  theme: {
    extend: {},
  },
  plugins: [
    require('@tailwindcss/typography'),
  ],
}
