(** Generates mutation records: a comment block per source file
    summarising which mutants were killed and by what. These are meant to be
    pasted into the relevant test, as evidence that it actually checks
    something.

    The block doesn't pin exact counts (they change whenever tests are
    added); the point is that every mutant should be killed. Survivors are
    listed by line number.

    We don't know which test a file belongs to, so the block lists the
    targets that killed its mutants and it's up to you where to put it. *)

(** [write ~path ~today outcomes] writes one block per file, with the files
    that have the most survivors first. *)
val write : path:string -> today:string -> (Points.t * Run.outcome) list -> unit
