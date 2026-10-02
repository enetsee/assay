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
  ; binding : string
    (** The enclosing modules and top-level binding, e.g. [Lower.block], or
            [pp#2] for the second binding called [pp]. Unlike [line], it
            stays true when code above it moves, so reports name survivors by
            it. *)
  }

(** [make ~operator ~loc ~edit ~binding ~key] builds a point, taking file/line/column
    from [loc]. The [id] is a hash of the file, [key], the operator and
    [edit], not of the position.

    [key] says what the point is rather than where: the enclosing module path
    and top-level binding, the code being mutated as printed from the
    parsetree, and which occurrence of that code it is within the binding
    (see {!Mutate}). So an edit elsewhere in the file leaves the id alone,
    and the skip list keeps pointing at the right point. An edit to the code
    itself changes the id, which is right, since an old skip entry no longer
    describes it.

    Ids need to be unique across the whole program, since a run selects a
    single mutant by id. The ppx only sees one compilation unit at a time, so
    a counter won't work and we use a hash instead. [edit] is part of the hash
    because a single node can have more than one point (e.g. [<=] to [<] and
    [<=] to [>=]).

    If two points do end up with the same id, whoever reads the [.muts] files
    should reject both, since selecting one would also select the other. *)
val make
  :  operator:Operator.t
  -> loc:Ppxlib.Location.t
  -> edit:string
  -> binding:string
  -> key:string
  -> t
