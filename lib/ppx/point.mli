(** A single mutation point: a location and the edit made there. *)

type t =
  { id : int
  ; operator : Operator.t
  ; file : string
  ; line : int
  ; column : int
  ; edit : string
    (** Human-readable description, e.g. [">= -> >"] or ["drop 2 of 3"].
            Only used for reporting; the actual mutation is built from the
            AST node. *)
  }

(** [make ~operator ~loc ~edit] builds a point, taking file/line/column from
    [loc] and hashing all of them to get the [id].

    Ids need to be unique across the whole program, since a run selects a
    single mutant by id. The ppx only sees one compilation unit at a time, so
    a counter won't work and we use a hash instead. [edit] is part of the hash
    because a single node can have more than one point (e.g. [<=] to [<] and
    [<=] to [>=]).

    If two points do end up with the same id, whoever reads the [.muts] files
    should reject both, since selecting one would also select the other. *)
val make : operator:Operator.t -> loc:Ppxlib.Location.t -> edit:string -> t
