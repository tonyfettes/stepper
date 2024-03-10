import { defineConfig } from "vite";
import melangePlugin from "vite-plugin-melange";
import react from "@vitejs/plugin-react";

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
    react({
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
