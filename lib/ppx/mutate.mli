(** The AST walk that finds mutation points and rewrites them.

    Finding and rewriting happen in the same pass so that the id written to
    [<lib>.muts] and the id compiled into the code always match. *)

type t =
  { live : Point.t list
    (** Points that were rewritten. The build contains a branch for each one,
            selected by setting [ASSAY_MUTANT] to its id. *)
  ; skipped : Point.t list
    (** Points left out because they're on the skip list. They're recorded
            (marked as skipped) so the runner can report them. *)
  ; structure : Ppxlib.structure
  }

(** [run ~skip ~arid structure] mutates [structure], leaving out any point
    whose id [skip] returns true for, and anything inside an expression [arid]
    returns true for. See {!Skip} and {!Arid}.

    Skipped points are ones that break the build; arid points are ones that
    aren't worth testing. Neither is in the build. Skipped points are still
    listed in [<lib>.muts] (marked as skipped) so they show up in the report.

    [Set.add] and [Map.add] are only mutated in files that don't use
    Base/Core, since those take their arguments in a different order.

    Everything is decided from the parsetree, without types. Two operators have
    to guess: dropping [|> f] assumes [f] returns the same type it takes, and
    replacing a function body assumes the return annotation is accurate. A bad
    guess fails to type-check, and the fix is to add that point to the skip
    list. Pipeline stages that clearly change the type, like [|> ignore] or
    [|> List.length], aren't offered at all. *)
val run : skip:(int -> bool) -> arid:(Ppxlib.expression -> bool) -> Ppxlib.structure -> t
