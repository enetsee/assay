(** The project config file ([assay.conf] by default).

    Tells the runner how to build the project, how to run its tests, and how
    to work out from test output which part failed.

    One directive per line, starting with its name. Lines starting with [#]
    are comments.

    {[
      build       dune build --instrument-with assay
      muts        _build/assay
      skip        assay.skip
      arid        assay.arid
      timeout     30
      fail-prefix FAIL
      part-field  2
      target      law_lower _build/default/test/laws/law_lower.exe
      target      law_plan  _build/default/test/laws/law_plan.exe
    ]}

    [target] takes the dune executable name, then the command to run it. The
    name is used to look the target up in dune's dependency graph; if dune
    doesn't know it, the target runs for every mutant. The command goes
    through [/bin/sh], so you can set env vars etc. in it.

    [skip] is the skip list. When the build fails at points that don't
    type-check, the runner adds them to it and builds again, so it's created
    if it doesn't exist.

    [arid] is a file of function names to treat as arid (see [Arid] in the
    ppx), e.g. formatters or [failwith]. Only add things once survivors show
    they're noise.

    [covers] takes a target name and then the libraries (separated by commas
    or spaces) to restrict it to, on top of what the dependency graph says.
    Useful for an expensive target that links a lot but only really catches
    mutants in one place. Base this on actual results, since restricting a
    target that does catch things will turn kills into survivors.

    Commands run from the directory assay was started in, not from dune's
    build directory. Tests that load files by relative path will usually need
    a [cd] first. Getting this wrong won't error, the tests will just quietly
    do less and kill fewer mutants.

    Targets run in the order listed and stop at the first failure, so put the
    fast ones first.

    [fail-prefix] marks a line that reports a failure, and [part-field] is the
    (1-based, whitespace-separated) field on that line that names what failed.
    E.g. for [FAIL (c) the count is wrong] those would be [FAIL] and [2],
    giving [(c)]. If no such line is found we still use the exit code, but
    can't say which part failed. *)

type target =
  { name : string
  ; command : string
  }

type t =
  { build : string option
  ; muts : string
  ; skip : string option
  ; arid : string option
  ; timeout : float
  ; fail_prefix : string
  ; part_field : int
  ; targets : target list
  ; covers : (string * string list) list
    (** Keyed by target name. Targets not listed here just use the
            dependency graph. *)
  }

(** Raises [Failure] on an unknown directive (with the line number), or if
    there's no [muts] or [target]. *)
val load : string -> t
