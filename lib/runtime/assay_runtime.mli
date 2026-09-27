(** Runtime support for instrumented builds.

    Each mutation point is compiled as a branch guarded by {!active}, so a
    single build contains every mutant and [ASSAY_MUTANT] picks which one
    runs.

    Only generated code calls this. dune links it automatically into
    instrumented libraries. *)

(** [active id] is true if [ASSAY_MUTANT] is set to [id].

    The variable is read once at startup.

    If it's unset, empty or not a number, no mutant is active. Likewise if
    it's an id that doesn't exist, in which case the program runs unmodified
    and the mutant will look like a survivor. *)
val active : int -> bool
