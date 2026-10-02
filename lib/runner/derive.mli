(** Working out which points don't type-check, from a failed build.

    All mutants are compiled into one program, so a mutant that doesn't
    type-check (usually a dropped [|>] stage that changed the type) breaks the
    build. Nobody decides to skip those; the compiler does. So rather than
    have someone map each error to a point by hand, the runner reads the
    errors, finds the point at each one's exact start position, adds it to
    the skip list and builds again.

    Entries added this way have reasons starting with {!reason_prefix}, so
    they can be told apart from ones someone wrote, and removed when their
    point is gone. *)

(** A compiler error: where it starts and its first line. *)
type error =
  { file : string
  ; line : int
  ; column : int
  ; message : string
  }

(** The errors (not warnings) in a build's output. Positions are where each
    error starts; for one spanning several lines, that's the first. *)
val errors : string -> error list

(** The start of the reason on every entry the runner adds. *)
val reason_prefix : string

(** [reason error] is the reason to record for a point skipped because of
    [error]. *)
val reason : error -> string

(** Whether an entry with this reason was added by the runner. *)
val derived : string -> bool

(** [add ~path entries] appends [(id, reason)] lines to the skip file at
    [path], creating it if needed. *)
val add : path:string -> (int * string) list -> unit

(** [remove ~path ids] rewrites the skip file at [path] without the entries
    for [ids], keeping everything else (comments included) as it was. *)
val remove : path:string -> int list -> unit
