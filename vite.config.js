import { defineConfig } from "vite";
import melangePlugin from "vite-plugin-melange";
import react from '@vitejs/plugin-react';
import { lezer } from '@lezer/generator/rollup';

export default defineConfig({
  build: {
    minify: false,
  },
  base: './',
  worker: {
    plugins: () => [
      melangePlugin({
        buildTarget: "_output",
        buildCommand: "opam exec -- dune build @melange",
        watchCommand: "opam exec -- dune build --watch @melange",
      }),
    ]
  },
  plugins: [
    lezer({ exportName: 'parser' }),
    melangePlugin({
      buildTarget: "_output",
      buildCommand: "opam exec -- dune build @melange",
      watchCommand: "opam exec -- dune build --watch @melange",
    }),
    react({
      include: /\.(js|jsx|ts|tsx|re|rei|ml|mli|grammar)$/
    }),
  ],
  server: {
    watch: {
      usePolling: true,
      awaitWriteFinish: {
        stabilityThreshold: 500,
        pollInterval: 20,
      },
    },
  },
});
