(** Helpers shared by the table and the records. *)

(** The order operators are listed in. *)
val operator_order : string list

(** An operator's position in {!operator_order}; unknown ones sort last. *)
val rank : string -> int

(** Counts each distinct string, most common first (ties alphabetical). *)
val tally : string list -> (string * int) list

val is_killed : Run.outcome -> bool
val is_survived : Run.outcome -> bool
val is_hung : Run.outcome -> bool
val is_errored : Run.outcome -> bool

(** How many of the outcomes satisfy [f]. *)
val count : ('a * Run.outcome) list -> f:(Run.outcome -> bool) -> int
