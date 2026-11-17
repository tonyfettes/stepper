# Hazel Mini Stepper — browser interface

This is the original Melange / Reason React interface, connected to the current
OCaml evaluator in `../lib/`. It retains the CodeMirror editor, live evaluation,
inline redex selection, history, and the Show Residue setting. The obsolete
Optimize switch has been removed because residue management is part of the
current decomposition algorithm.

## Run the prebuilt app

If `dist/` is included, no build tools are needed:

```sh
make serve
```

Open <http://localhost:8001/>. Use `make serve PORT=9001` for another port.
Without Make, run `python3 -m http.server 8001 --bind 127.0.0.1 --directory dist`.
Serve the app over HTTP so its module worker can load.

Enter an expression in the editor; evaluation updates as you type. Click a blue
redex to take that visible step. The selected expression is added to the history,
and silent reductions are taken automatically. Show Residue toggles the display
of internal residues. Editing the program starts a fresh trace and cancels any
pending evaluation. The editor text is saved in the browser's local storage.

For example, enter:

```
debug eval(1 + 2 + 3 + 4) in
debug stop(3 + 3 + 4) in
1 + 2 + 3 + 4
```

The first addition is silent. Click the blue `3 + 3` to continue to `10`.

## Build from source

The build uses OCaml 5.2, Melange 4.0.1-52, Reason React 0.14.1, Vite, and pnpm.
OCaml dependencies are declared in `stepper-web.opam`; JavaScript dependencies
are fixed by `pnpm-lock.yaml`. With a compatible opam switch active:

```sh
make deps
make
make serve
```

To create a separate compatible switch for this directory, run this once before
`make deps`:

```sh
opam switch create . ocaml-base-compiler.5.2.0
eval "$(opam env --switch=. --set-switch)"
```

`make dev` starts the Vite development server. Production output goes into
`dist/`. The same `../lib/` sources compile for both native OCaml and Melange;
there is no second copy of the evaluator.

## Layout

| Path | Purpose |
| --- | --- |
| `index.re`, `index.css`, `index.html` | Original page and styling. |
| `StepperReact*.re` | Editor, settings, candidates, and history. |
| `StepperReactInputImpl.tsx`, `stepper-language.ts`, `stepper.grammar` | Original CodeMirror editor and syntax highlighting. |
| `StepperWorker.re` | Runs the current evaluator in a module worker. |
| `StepperWorkerMessage.re`, `StepperResult.re` | Worker requests and results. |
| `Display.ml`, `vendor/pprint/` | Original display conventions adapted to current expressions and contexts. |
| `dune`, `vite.config.js` | Melange and Vite build configuration. |

The implementation license is in `../LICENSE`; the vendored printer includes
its license. Supplementary copies include notices for bundled third-party code.
