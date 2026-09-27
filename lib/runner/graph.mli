(** Works out which libraries each target links, so a mutant only runs
    against tests that could possibly catch it.

    For each target we run [dune rules] on its executable and collect the
    [.cmxa]/[.cma] archives the link rule depends on. We use archives rather
    than library uids because [dune describe] doesn't include [(tests)]
    stanzas, and [dune describe tests] doesn't list their dependencies.

    Modules of executables are linked directly rather than through an
    archive. Those are matched by the directory of their build objects, as
    [exe.<dir>], which is the name the ppx records them under. *)

type t

(** [build targets] asks dune where each target is built and what it links.
    If dune has several executables with a target's name, the one whose path
    (or else directory) appears in the target's command is used. Targets that
    can't be placed are kept and run for every mutant, rather than assumed to
    reach nothing. *)
val build : Config.target list -> t

(** The targets that link the given library, plus any unplaced targets. *)
val reaches : t -> string -> string list

(** Targets that couldn't be placed, with the reason, in the order given. *)
val unplaced : t -> (string * string) list

(** The built executable for each target dune found, as a path under
    [_build]. Used to notice if something rebuilds them during a run. *)
val executables : t -> (string * string) list
