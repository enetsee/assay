(** Arid code: places where a mutant isn't worth generating.

    Some code (logging, formatting, assertions) produces survivors that nobody
    cares about. Google found that filtering these out made mutation results
    much more useful (roughly 20% to 80% of findings being actionable), so we
    do the same.

    The [-arid <path>] ppx flag (or failing that [ASSAY_ARID]) points at a
    file of function paths, one per line, with [#]
    for comments. A call to any of them is arid, as is everything inside the
    call. If the variable is unset or the file is missing, nothing is arid.

    Start with an empty list and only add a name once a survivor shows it's
    noise. Guessing up front tends to be wrong: e.g. trace calls look like
    logging, but a test that counts steps will notice when one is dropped.

    A compound expression is arid if all of its children are. Leaves
    (identifiers, constants) are never arid, otherwise [a + b] and every other
    bit of arithmetic would be too. *)

(** Set from the [-arid] flag. *)
val file : string option ref

(** Reads the names from the file named by [-arid] or [ASSAY_ARID]. *)
val configured : unit -> string list

(** [is_arid ~names e] is true if mutating [e] isn't worthwhile. *)
val is_arid : names:string list -> Ppxlib.expression -> bool
