(** Generates mutation records: a comment block per source file
    summarising which mutants were killed and by what. These are meant to be
    pasted into the relevant test, as evidence that it actually checks
    something.

    The block doesn't pin exact counts (they change whenever tests are
    added); the point is that every mutant should be killed. Survivors are
    listed by binding and operator, e.g. [survived in Lower.block (sbr 2)],
    which stays true until that code changes.

    The block lists the targets that killed its mutants. Where it lives is
    up to you; once it's in a test, {!Placed} keeps it up to date. *)

(** [blocks ~today outcomes] is each source file with its block, the files
    with the most survivors first. *)
val blocks : today:string -> (Points.t * Run.outcome) list -> (string * string) list

(** [write ~path ~today outcomes] writes one block per file, with the files
    that have the most survivors first. *)
val write : path:string -> today:string -> (Points.t * Run.outcome) list -> unit
