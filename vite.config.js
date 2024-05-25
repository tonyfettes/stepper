import { defineConfig } from "vite";
import melangePlugin from "vite-plugin-melange";
import react from '@vitejs/plugin-react';

const ReactCompilerConfig = {}

export default defineConfig({
  base: './',
  plugins: [
    {
      enforce: 'pre',
      ...melangePlugin({
        emitDir: "web",
        buildCommand: "dune build @react --release",
        watchCommand: "dune build --watch @react --release",
      })
    },
    react({
      babel: {
        plugins: [
          ["babel-plugin-react-compiler", ReactCompilerConfig],
        ]
      },
      include: /\.(js|jsx|ts|tsx|re|rei|ml|mli)$/
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
