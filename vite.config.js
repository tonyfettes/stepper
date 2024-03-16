import { defineConfig } from "vite";
import melangePlugin from "vite-plugin-melange";
import preact from "@preact/preset-vite";

export default defineConfig({
  base: './',
  plugins: [
    {
      enforce: 'pre',
      ...melangePlugin({
        emitDir: "web",
        buildCommand: "esy -- dune build @react",
        watchCommand: "esy -- dune build --watch @react",
      })
    },
    preact({
      include: /\.(js|jsx|ts|tsx|re|rei|ml|mli)$/
    }),
  ],
  resolve: {
    alias: {
      react: 'preact/compat',
      'react-dom': 'preact/compat',
    },
  },
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
