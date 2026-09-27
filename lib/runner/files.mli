(** Reading files. *)

(** The whole file. *)
val read : string -> string

(** The file's lines, without the trailing newlines. *)
val lines : string -> string list
