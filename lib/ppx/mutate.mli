(** The AST walk that finds mutation points and rewrites them.

    Finding and rewriting happen in the same pass so that the id written to
    [<lib>.muts] and the id compiled into the code always match. *)

type t =
  { live : Point.t list
    (** Points that were rewritten. The build contains a branch for each one,
            selected by setting [ASSAY_MUTANT] to its id. *)
  ; skipped : (Point.t * string option) list
    (** Points left out, either because they're on the skip list ([None]) or
            inside an [assay.skip] attribute (with its reason). They're
            recorded (marked as skipped) so the runner can report them. *)
  ; structure : Ppxlib.structure
  }

(** [run ~skip ~arid structure] mutates [structure], leaving out any point
    whose id [skip] returns true for, and anything inside an expression [arid]
    returns true for. See {!Skip} and {!Arid}.

    An expression or binding with an [[@assay.skip "reason"]] (or
    [[@@assay.skip "reason"]]) attribute has every point inside it skipped,
    with that reason, along with the point that would replace or drop it
    (so [xs |> List.rev [@assay.skip "..."]], where the attribute attaches to
    [List.rev], skips dropping that stage). That's for judgement calls about one site, like a
    mutant that can't change anything observable; the decision then sits
    next to the code and moves with it. A missing or empty reason is an
    error.

    Skipped points are ones that break the build, or that someone decided
    against; arid points are ones that aren't worth testing. Neither is in the build. Skipped points are still
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
