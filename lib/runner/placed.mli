(** Records blocks that live in test files.

    A block from {!Records} is meant to sit in the test that covers its
    source file, as a comment. Its first line marks it, and its second names
    the source file, so the blocks can be found again and kept up to date
    instead of being copied in by hand after every run.

    Everything here works on a directory of tests: every [.ml] and [.mli]
    file under it, skipping directories whose names start with [_] or [.]. *)

(** What a run's blocks mean for the blocks already in the tests. Tests and
    sources are paths; tests as found under the directory, sources as the
    blocks name them. *)
type report =
  { stale : (string * string) list
    (** Test and source of each block that differs from this run's. A block
      whose only difference is the date isn't stale. *)
  ; orphaned : (string * string) list
    (** Test and source of each block whose source has no mutants in this run. *)
  ; homeless : string list (** Sources with a block in this run but none in any test. *)
  ; outside : (string * string) list
    (** Test and source of each block left alone because the run didn't
        cover every library and its source wasn't in it. *)
  ; current : int (** How many blocks are up to date. *)
  }

(** [check ~dir ~blocks ~complete] compares the blocks in the tests under
    [dir] with [blocks] (source and block, from {!Records.blocks}).

    [complete] says the run covered every library. If it didn't (with
    [-only] or [ASSAY_ONLY]), a block missing from the run may just be for a
    library that wasn't run, so nothing is reported as orphaned. *)
val check : dir:string -> blocks:(string * string) list -> complete:bool -> report

(** [update ~dir ~blocks ~complete] does what {!check} reports: replaces
    each stale block with this run's, and deletes each orphaned one (with the
    blank line after it). Homeless blocks are left for you to place, since
    which test a file belongs to is a judgement. Returns the same report as
    {!check} would have. Only files that change are written. *)
val update : dir:string -> blocks:(string * string) list -> complete:bool -> report

(** [test_named ~dir name] is the [name.ml] under [dir], if there's one,
    for suggesting where a homeless block could go. *)
val test_named : dir:string -> string -> string option
