# assay

A mutation tester for OCaml.

Mutation testing checks whether your tests would notice if the code were
wrong. It makes a small change to the code (a mutant), runs the tests, and
records whether any of them failed. A mutant that no test catches points at
behaviour that isn't really being tested.

`assay` generates mutants automatically from the parsetree, so there's no
hand-written list to keep in sync with the code.

## How it works

`assay` is a dune instrumentation backend. Enable it on a library with:

```
(library
 (name core)
 (instrumentation
  (backend assay)))
```

A normal build is unaffected. When you build with `--instrument-with assay`,
each mutation point is compiled as a branch guarded by a runtime check, so a
single build contains every mutant and `ASSAY_MUTANT` selects which one is
active. The source is never modified.

```
ASSAY_MUTS_DIR=$PWD/_build/assay dune build --instrument-with assay
```

`ASSAY_MUTS_DIR` is where the ppx writes the list of mutation points, one
`<library>.muts` file per library. It needs to be an absolute path because
dune runs the ppx in a temporary sandbox.

A couple of gotchas with that directory:

- Clear it before each run. Rebuilds only reprocess changed modules, so a
  deleted module's entries would otherwise stick around.
- Build with dune's cache disabled. If dune replays a cached preprocessing
  step, the ppx doesn't run and nothing gets written.

Executables don't have a library name, so their points are recorded under
`exe.<dir>` instead, e.g. `exe.bin` for an executable in `bin/`.

`ASSAY_ONLY` limits instrumentation to a comma-separated list of libraries
(default: all). This is handy when adding a new operator, since compile
errors from one library are much easier to work through than from the whole
tree.

```
ASSAY_ONLY=core,plan ASSAY_MUTS_DIR=$PWD/_build/assay \
  dune build --instrument-with assay
```

## Libraries

- `assay` is the ppx, and the name you use in `(backend assay)`.
- `assay.runtime` contains a single function, `active : int -> bool`.

You don't need to add either to a `libraries` field. The ppx declares the
runtime in `ppx_runtime_libraries`, so dune links it only into instrumented
builds. Normal builds have no dependency on assay at all.

## Skip list

Because every mutant is compiled into the same program, one mutant that
doesn't type-check breaks the build for the whole library.

`ASSAY_SKIP` points to a file of point ids to leave out, one per line,
optionally followed by a tab and a reason. Building it up is iterative:
build, find the point at the error location, add it, and rebuild.

This is slower than it sounds, for two reasons:

- dune doesn't track environment variables as inputs, so changing the skip
  list doesn't trigger a rebuild. Each round needs `dune clean` and
  `DUNE_CACHE=disabled`.
- dune stops at the first error, so you only find one bad point per round.

Match errors to points by their exact start position. An error spanning a
whole lambda covers every point inside it, and guessing the innermost one can
skip a perfectly good mutant. If an error doesn't line up with any single
point, the mutation type-checked locally and the problem showed up further
out (e.g. `x |> invalid_arg` with the stage replaced by `Fun.id`). That's
worth reporting as a bug.

For reference, on one project with 2418 mutants across fourteen libraries, 52
points needed skipping, all of them `|>` drops.

## Operators

| | |
|---|---|
| `extreme` | replace a function body with a default value of its return type |
| `sbr` | statement block removal: a sequence, a unit `if`, a list element, a `\|>` stage, an `iter` call |
| `ror` | relational operator replacement |
| `lcr` | logical connector replacement |
| `aor` | arithmetic operator replacement |
| `uoi` | unary operator insertion (negating an `if` condition) |
| `empty` | replace a set/map operation with one of its arguments |

`sbr` tends to be the most useful: Google report it as 72% of the mutants
their system generates, and the second least likely to survive. `extreme` is
the cheapest, with one mutant per function. If it survives, the function is
run by the tests but nothing checks what it returns.

`extreme` needs the function's return type to be annotated, since the ppx
only has the parsetree to go on.

`empty` only mutates `Set.add` and `Map.add` in files that don't use
Base/Core, since those take their arguments in a different order.

Code inside inline tests (`let%test`, `let%expect_test`, etc.), metaquot
quotations and attribute payloads isn't mutated.

## Running it

The `assay` command reads a config file that says how to build the project,
how to run its tests, and how to tell from test output which part failed.
See `lib/runner/config.mli` for the format.

```
assay -config assay.conf            # all libraries
assay -config assay.conf -only core # just one
assay -no-build                     # skip the build step
assay -j 8                          # run 8 mutants in parallel
```

It will:

1. Build the project with instrumentation.
2. Run every target once with no mutant, as a baseline.
3. Ask dune which targets link each library.
4. Run each mutant against the relevant targets, stopping at the first
   failure.

Each run has a timeout, since some mutants cause infinite loops (removing an
`incr i` from a loop, say). Those are reported as timed out rather than
killed.

If a mutant can't be run at all (for example, its worker process dies), it's
reported under "could not be run" and the rest of the run carries on.

At the end it prints a summary table per operator, what killed the mutants,
any timeouts and errors, and the survivors. It also writes `assay.results` (one line
per mutant) and `assay.records` (a per-file summary you can paste into a
test). Use `-from assay.results` to regenerate the report without rerunning
anything.

assay also notices if a target executable is rebuilt partway through a run
(say, by a plain `dune build` in another shell), which would otherwise make
every remaining mutant look like a survivor. It stops, keeps the results so
far, and reports the rest as "could not be run".

Keep an eye on the baseline timings. A misconfigured target that does
nothing and exits 0 will make every mutant look like a survivor. A target
that normally takes a few seconds finishing instantly is the giveaway.
