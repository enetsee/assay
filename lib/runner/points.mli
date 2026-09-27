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

(** Reads a results file written by {!Table.write_results}, so the report can
    be regenerated without rerunning anything. *)
val results : string -> (t * string * string * string) list
