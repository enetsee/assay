(** A minimal s-expression parser, just enough for [dune describe] and
    [dune rules] output. Saves pulling in a dependency.

    Handles bare atoms and double-quoted strings. No comments or block
    strings, since dune doesn't output them. *)

type t =
  | Atom of string
  | List of t list

(** Parses exactly one form. Raises [Failure] if the input is malformed or
    doesn't contain exactly one form. *)
val of_string : string -> t

(** Parses any number of forms, e.g. the output of [dune rules]. *)
val many : string -> t list

(** [field name form] finds the first [(name ...)] in [form] and returns
    everything after [name]. *)
val field : string -> t -> t list option

(** Like {!field} but for a single atom, e.g. [(uid abc)] gives ["abc"]. *)
val atom_field : string -> t -> string option
