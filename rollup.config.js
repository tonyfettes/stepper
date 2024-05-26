import {nodeResolve} from "@rollup/plugin-node-resolve"

export default {
  input: "./web/codemirror/parser.js",
  output: [{
    format: "cjs",
    file: "./web/codemirror/parser.cjs"
  },{
    format: "es",
    file: "./web/codemirror/parser.js"
  }],
  external(id) { return !/^[\.\/]/.test(id) },
  plugins: [
    nodeResolve()
  ]
}
