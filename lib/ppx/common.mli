(** Helpers shared by the rest of the ppx. *)

(** A module path as a list, e.g. [Stdlib.Fun.id] gives
    [["Stdlib"; "Fun"; "id"]]. Functor applications give [[]]. *)
val flatten : Ppxlib.longident -> string list

(** The function being applied, as a path. Empty if it isn't an identifier. *)
val callee : Ppxlib.expression -> string list

(** The lines of the file named by [flag] (a ppx command-line flag), or by
    environment variable [var] if the flag wasn't given, without blank lines
    or [#] comments. [[]] if neither is set or the file doesn't exist. *)
val config_lines : flag:string option -> string -> string list
