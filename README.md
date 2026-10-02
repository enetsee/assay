# assay

A mutation tester for OCaml.

Mutation testing checks whether your tests would notice if the code were
wrong. It makes a small change to the code (a mutant), runs the tests, and
records whether any of them failed. A mutant that no test catches points at
behaviour that isn't really being tested.

`assay` generates mutants automatically from the parsetree, so there's no
hand-written list to keep in sync with the code.

## How it works

`assay` is a [dune instrumentation backend](https://dune.readthedocs.io/en/stable/instrumentation.html). Enable it in a library with:

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

## Skipping points

There are two reasons to leave a point out, and they're handled differently.

### Points that don't type-check

Because every mutant is compiled into the same program, one mutant that
doesn't type-check breaks the build for the whole library. Usually it's a
dropped `|>` stage that changed the type. Nobody decides to skip these; the
compiler does, so assay finds them itself.

When the build fails, the `assay` command looks for errors that start
exactly at a mutation point, adds those points to the skip list (with the
compiler's message as the reason) and builds again, until the build passes.
An error that doesn't start at a point is reported as a build failure. Exact
positions matter: an error spanning a whole lambda covers every point inside
it, and guessing the innermost one could skip a perfectly good mutant.

The skip list is the file named by `skip` in the config: one point id per
line, optionally followed by a tab and a reason. Entries assay added have
reasons starting `doesn't type-check:`. Treat them as a cache. When a point's
code changes, its old entry stops matching anything and assay removes it; if
the new code still doesn't type-check, the next build finds it again.

Pass the file to the ppx with `-skip`, and list it in `deps`, so that dune
knows it's an input:

```
(library
 (name core)
 (instrumentation
  (backend assay -skip assay.skip)
  (deps %{workspace_root}/assay.skip)))
```

Then a change to the list rebuilds only what it affects. The `-skip` path is
relative to the workspace root, and the file has to exist, even if it's
empty. The older `ASSAY_SKIP` environment variable still works, but dune
doesn't track environment variables, so every change to the list needs a
`dune clean`.

### Points you decide against

For a mutant that can't change anything observable, or code whose result
nobody reads, put the decision in the code:

```ocaml
xs |> List.rev [@assay.skip "the order is not observable here"]
let pp ppf t = ... [@@assay.skip "only used for debugging output"]
```

An `[@assay.skip "reason"]` attribute skips every point inside the expression
it's on, plus the point that would drop or replace that expression (above,
the attribute attaches to `List.rev`, and dropping that stage is skipped).
`[@@assay.skip "reason"]` on a binding skips the whole binding. The reason is
required, and shows up in the report. The attribute moves with the code and
reviewers see it where it applies. A plain build ignores it.

For whole families of functions, like loggers and formatters, use the arid
list (`ASSAY_ARID` or `-arid`, one function path per line).

### Point ids

A point's id is a hash of what it is, not where it is: its file, its enclosing
modules and top-level binding, the code being mutated (as printed from the
parsetree, so formatting and comments don't count), and which occurrence of
that code it is within the binding. Editing other code, including adding lines
above a point, leaves its id alone. Editing the point's own code changes the
id, since an old skip entry no longer describes it.

At the start of each run, assay lists skip entries that match no point (they
skip nothing) and entries whose point was built anyway (dune reused
preprocessing from before the list changed).

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

### Records in the tests

Each block in `assay.records` is meant to live in the test that covers its
source file, as a comment, as evidence that the test checks something. Once a
block is there, assay keeps it up to date:

```
assay -update-records test/   # rewrite the blocks under test/ to match this run
assay -check-records test/    # just report the ones that differ; exits 1 if any
```

Both also work with `-from`. A block is found by its first line and matched
to this run's block by the source file on its second. Then:

- A block that differs is replaced, keeping its indentation. A block that
  differs only in its date is left alone.
- A block whose source has no mutants in this run is deleted, with the blank
  line after it. A run limited with `-only` or `ASSAY_ONLY` doesn't delete
  blocks for files it didn't cover.
- A source with no block in any test is listed with a suggestion (the test
  named after the target that killed most of its mutants). Choosing where it
  goes is up to you; once placed, it's kept up to date.

Survivors are named by binding and operator (`survived in Lower.block (sbr
2)`), not line number, so a block stays true until that code changes.

`assay.results` is written as each mutant finishes, so if a run is
interrupted the finished mutants aren't lost. A file from an unfinished run
starts with a `# unfinished run` line, and `-from` warns that it's partial.

assay also notices if a target executable is rebuilt partway through a run
(say, by a plain `dune build` in another shell), which would otherwise make
every remaining mutant look like a survivor. It stops, keeps the results so
far, and reports the rest as "could not be run".

Keep an eye on the baseline timings. A misconfigured target that does
nothing and exits 0 will make every mutant look like a survivor. A target
that normally takes a few seconds finishing instantly is the giveaway.
