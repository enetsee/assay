(** Points to leave out of the build.

    All mutants are compiled into one program, so a single point that doesn't
    type-check breaks the whole library. To work around it: build, map the
    error location back to a point, add it to the skip list, and rebuild.

    [ASSAY_SKIP] names the file. One id per line, optionally followed by a tab
    and the reason. Lines starting with [#] are comments. If there's no file,
    nothing is skipped.

    The rewriter never adds to this list itself. If it silently dropped points
    it couldn't handle, that would be indistinguishable from a bug, so the
    list (and the reasons) are maintained explicitly. *)

(** Whether the point with this id is on the skip list. *)
val skipped : int -> bool
