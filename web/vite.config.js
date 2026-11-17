import { defineConfig } from "vite";
import melangePlugin from "vite-plugin-melange";
import react from '@vitejs/plugin-react';
import { lezer } from '@lezer/generator/rollup';

const workerEntry = (plugin) => ({
  ...plugin,
  resolveId(source, importer, options) {
    if (!importer && /\.(re|ml)$/.test(source)) return source;
    return plugin.resolveId.call(this, source, importer, options);
  },
});

export default defineConfig({
  build: {
    minify: false,
  },
  base: './',
  resolve: {
    dedupe: [
      'react', 'react-dom', '@uiw/react-codemirror',
      '@codemirror/language', '@lezer/highlight', '@lezer/lr',
    ],
  },
  worker: {
    plugins: () => [
      workerEntry(melangePlugin({
        duneDir: "..",
        buildTarget: "_output",
        buildCommand: "opam exec -- dune build --profile melange @melange",
        watchCommand: "opam exec -- dune build --profile melange --watch @melange",
      })),
    ]
  },
  plugins: [
    lezer({ exportName: 'parser' }),
    melangePlugin({
      duneDir: "..",
      buildTarget: "_output",
      buildCommand: "opam exec -- dune build --profile melange @melange",
      watchCommand: "opam exec -- dune build --profile melange --watch @melange",
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
