# OCaml reference implementation

A standalone implementation of the stack-based filter mechanism in
`../moonbit/lib/*.mbt`. It follows the current MoonBit implementation; the older
implementation, kept in this repository's history under the `hazel-mini` tag, informed the OCaml module organization.
The parser and evaluator depend only on the OCaml standard library. The
property tests use [QCheck2](https://ocaml.org/p/qcheck-core/0.91/doc/qcheck-core/QCheck2/index.html),
supplied by the test-only `qcheck-core` package.

## Build and run

Requires OCaml 4.13 or later and Dune 3.0 or later, with `ocamllex` and
`ocamlyacc` available (both ship with OCaml). From the repository root:

```sh
opam install . --deps-only --with-test
dune build
dune runtest
dune exec bin/main.exe -- \
  --expr 'debug eval($e) in debug stop(3 + 3 + 4) in 1 + 2 + 3 + 4'
```

The example silently reduces `1 + 2`, then offers `3 + 3` as a visible choice.
After choosing it, evaluation continues silently to `10`.

The executable also accepts a file or standard input. Its modes are:

| Mode | Behavior |
| --- | --- |
| `step` (default) | Runs silent transitions, then prints every visible choice or the final value. |
| `decompose` | Prints all immediate candidates, their actions, action levels, and contexts. |
| `trace` | Prints each transition; chooses the leftmost silent candidate, otherwise the leftmost visible candidate. |

```sh
dune exec bin/main.exe -- \
  --mode trace --expr 'debug eval($e) in debug stop(3 + 3 + 4) in 1 + 2 + 3 + 4'
```

`--limit N` bounds silent transitions in `step` mode and all transitions in
`trace` mode (default 10000). Exhaustion reports the remaining expression and
exits with status 3. Parse and evaluation errors exit with status 2.
The printer shows a context hole as `@` and internal residues as
`eval#1[...]`, `step#2[...]`, etc. Residue notation is diagnostic output,
not source syntax.

Activate your opam switch before building. This workspace has an isolated
switch under `_opam`, containing Dune and QCheck2; activate it from the
repository root with `eval $(opam env --switch=.)`. That directory is
ignored by Git. For a fresh checkout, either use an existing switch or create
one with `opam switch create . ocaml-system --no-switch`, then activate
it and install the dependencies as above.

## Library

The Dune library is `stepper`, with modules under `Stepper`:

| Module | Role |
| --- | --- |
| `Syntax` | Expressions, recursive patterns, filters, residues, actions, gas, stripping. |
| `Matches` | Alpha-aware structural matching and pattern closedness. |
| `Subst` | Substitution in expressions and patterns; expression-to-pattern conversion. |
| `Ctx` | Hole-first evaluation contexts and `compose`. |
| `Step` | Stack lookup, decomposition, instruction transitions, and filtered stepping. |
| `Parse`, `Print` | Source parsing and unambiguous expression/context printing. |

For example:

```ocaml
open Stepper

let choose_first source =
  match Parse.parse source with
  | None -> None
  | Some exp ->
      match Step.step ~limit:10000 exp with
      | Val value -> Some value
      | Exp (obj :: _) -> Some (Step.take obj)
      | Exp [] -> assert false
```

`Step.decompose` returns `act_obj` records containing `ctx`, `exp`, `act`,
and `act_lvl`. `Step.step` runs the leftmost silent candidate repeatedly
and returns `Val exp` or `Exp objs`, where every offered `obj` contains its
own context and redex. `Step.take obj` commits that choice. Call `step`
again on the resulting expression to continue. The library's `step` has no
transition limit unless the caller supplies one.

## Stack mechanism

Decomposition starts with `Pause`, `act_lvl = 0`, and `ins_lvl = 0`. The names
match MoonBit: `act_lvl` is the action level; `ins_lvl` is the residue level
(`ell_r` in the paper). Lexical filter levels start at 1.

Entering a filter pushes it onto an immutable, innermost-first list. Lookup
scans levels strictly above `ins_lvl`. A matching filter changes the selected
action only when its level exceeds `act_lvl`. One-shot matches keep scanning.
The first persistent match inserts one residue, advances `ins_lvl`, and stops
the scan. That residue records its own filter's action, even if a higher-level
one-shot filter has selected a different action.

An existing residue competes for the action using the same strict comparison.
It survives in the candidate context only when it is persistent and its level
exceeds `ins_lvl`; reading it advances the cutoff to the maximum level.
Both operands are explored independently, left to right. Only the chosen
candidate's context is committed. Filters and residues around values are
removed silently using the original context and the entire wrapper as redex.

This replaces the older implementation's separate instrumentation,
optimization, action-selection, and decay passes. There is no implicit default
filter, and visible choices are offered only when no silent candidate remains.

## Compatibility details

- Source keywords follow the paper's keyword table, as MoonBit does:

  | Keyword | Action | Gas |
  | --- | --- | --- |
  | `eval` | `Eval` | `All` |
  | `hide` | `Eval` | `One` |
  | `stop` | `Pause` | `One` |
  | `step` | `Pause` | `All` |

- Expressions include integers, booleans, arithmetic, integer equality,
  functions, application, fixpoints, conditionals, and filters. `let` and
  `let rec` desugar as in MoonBit. Pattern bodies under `fun` and `fix` are
  recursive patterns and may contain `$e` and `$v`.
- Integers use OCaml `int32` so arithmetic has MoonBit `Int` overflow behavior.
  AST integer literals therefore look like `Syntax.Exp.Int 42l`.
- `$v` matches integers, functions, and variables bound by a matched `fun`,
  but not variables bound by `fix`. Like current MoonBit, it does **not** match
  boolean literals, even though booleans are runtime values.
- Substitution stops at shadowing binders and also substitutes filter patterns.
  Like MoonBit, it assumes closed replacements during evaluation; it does not
  rename binders to prevent capture for arbitrary open replacement terms.
- `transition` is the instruction primitive: pass a redex returned by
  `decompose`. As in MoonBit, calling it directly on other expressions is not
  a substitute for the evaluator's call-by-value decomposition.
- The lexer follows MoonBit's signed-literal rule: write `x - 1` for subtraction;
  `x-1` lexes as an identifier followed by a negative integer and is rejected.
- This is an executable reference for the current MoonBit language, with no
  web UI or static typechecker. The Agda proofs remain in `../moonbit/agda/`.

## Validation

`dune runtest` runs 31 regression tests and 14 QCheck2 properties.
The regression tests cover filter priority,
separate action/residue levels, residue cutoff and cleanup, independent
candidates, silent-before-visible stepping, matching under binders, substitution,
errors, parser/print round trips, recursion, overflow, and transition limits.

The properties run **5,000 generated cases each** (70,000 total). They check:

- Recomposition up to residues, value/candidate correspondence, and redex shape.
- Strictly increasing residue levels and at most one insertion per filter.
- Progress for typed closed terms and simulation of unfiltered evaluation.
- Residue ownership and simulation throughout runs of up to 20 transitions.
- Complete evaluation against a separate evaluator using closures, on the
  terminating, fixpoint-free integer subset.
- Parser/print round trips, wrapper-insensitive matching, and patternization
  against an independent alpha-equivalence oracle.

`test/generators.ml` builds recursive expressions, patterns, and typed closed
terms with QCheck2's generator combinators and integrated shrinking. Typed
generators preserve types and scope while shrinking; properties do not discard
ill-typed samples or swallow evaluation exceptions. Counterexamples print in
the stepper's expression syntax, including residue annotations. QCheck prints
the random seed so failures can be reproduced:

```sh
dune exec test/properties.exe -- --seed 20261003 --verbose
# Five times as many cases per property:
dune exec test/properties.exe -- --seed 20261003 --long
```

Runs involving arbitrary fixpoints are bounded; a terminating-subset example
that exceeds its 20,000-transition bound fails the test instead of being skipped.

Validated with OCaml 5.5.0, Dune 3.23.1, and QCheck 0.91.
