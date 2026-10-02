(** Points to leave out of the build.

    All mutants are compiled into one program, so a single point that doesn't
    type-check breaks the whole library. To work around it: build, map the
    error location back to a point, add it to the skip list, and rebuild.

    The [-skip <path>] ppx flag names the file, or failing that
    [ASSAY_SKIP]. The flag is better: give it in the instrumentation stanza
    along with the file in [deps], and dune rebuilds what a change to the
    list affects. dune doesn't track environment variables, so with
    [ASSAY_SKIP] a change needs [dune clean]. One id per line, optionally followed by a tab
    and the reason. Lines starting with [#] are comments. If there's no file,
    nothing is skipped.

    The rewriter never adds to this list itself. If it silently dropped points
    it couldn't handle, that would be indistinguishable from a bug. Instead
    the runner adds points that the compiler rejected, with the compiler's
    message as the reason (see [Assay_runner.Derive]), so every skip is
    listed and explained. *)

(** Set from the [-skip] flag. *)
val file : string option ref

(** Whether the point with this id is on the skip list. *)
val skipped : int -> bool
