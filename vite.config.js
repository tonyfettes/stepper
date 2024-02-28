import { defineConfig } from "vite";
import melangePlugin from "vite-plugin-melange";

export default defineConfig({
  plugins: [
    melangePlugin({
      emitDir: "web",
      buildCommand: "esy -- dune build @react",
      watchCommand: "esy -- dune build --watch @react",
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
