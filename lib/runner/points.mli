(** Reads the [.muts] files the ppx wrote (one per library).

    Duplicate ids mean a hash collision, and since selecting one would select
    both, {!load} fails rather than running either. *)

type t =
  { id : int
  ; operator : string
  ; file : string
  ; line : int
  ; column : int
  ; edit : string
  ; binding : string
    (** The enclosing modules and top-level binding, e.g. [Lower.block]. *)
  ; library : string (** Taken from the [.muts] file name. *)
  }

(** All live points in [dir] (not the skipped ones), grouped by library, in
    the order they were found.

    Raises [Failure] on a duplicate id. *)
val load : dir:string -> t list

(** An entry from the skip list. Only used for reporting. *)
type skip =
  { point : t
  ; reason : string
  }

(** The points in [dir] marked as skipped, with their reasons taken from the
    skip file ([reasons_file]) if there is one. *)
val skips : dir:string -> reasons_file:string option -> skip list

(** The entries in a skip file, in order: id and reason ([""] if none).
    [[]] if there's no file. *)
val entries : string option -> (int * string) list

(** Skip-list entries that don't do what they claim, and so would silently
    skip nothing. *)
type stale =
  { missing : (int * string) list
    (** Entries (id and reason) that match no point in [dir]. Usually the
      point's code changed or was deleted; or the entry is for a library that
      wasn't instrumented. *)
  ; unapplied : (t * string) list
    (** Entries whose point was built without being skipped. dune doesn't see
      the skip list change, so it reused preprocessing from before. *)
  }

(** Checks the skip file ([reasons_file]) against the points in [dir]. *)
val stale : dir:string -> reasons_file:string option -> stale

(** The first line of a results file written while a run is in progress.
    It's followed by the number of mutants the run had to do. *)
val unfinished_marker : string

(** [Some total] if the results file at [path] is from a run that didn't
    finish, where [total] is how many mutants it was meant to have. *)
val unfinished : string -> int option

(** Marks a results file line naming the libraries a run was limited to. *)
val only_marker : string

(** The libraries the run that wrote the results file at [path] was limited
    to, or [[]] if it covered them all. *)
val only : string -> string list

(** Reads a results file written by {!Table.write_results}, so the report can
    be regenerated without rerunning anything. *)
val results : string -> (t * string * string * string) list
