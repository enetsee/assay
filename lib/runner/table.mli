(** The summary table printed at the end of a run.

    There's a row for every operator, even ones with no mutants, so it's
    obvious when an operator isn't generating anything.

    [generated] counts the points found plus the skipped ones, and every one
    of them should have exactly one outcome, i.e.
    [generated = killed + survived + hung + errored + skipped]. If that
    doesn't hold, a mutant went missing (or was counted twice) and the table
    prints a warning.

    Hung mutants get their own column rather than counting as killed, since
    it was the timeout that caught them, not a test. Errored mutants are ones
    that couldn't be run at all, so their outcome is unknown. *)

type row =
  { operator : string
  ; generated : int
  ; killed : int
  ; survived : int
  ; hung : int
  ; errored : int
  ; skipped : int
  }

val rows
  :  points:Points.t list
  -> skips:Points.skip list
  -> outcomes:(Points.t * Run.outcome) list
  -> row list

(** Prints the table, then skip reasons, what did the killing, any hangs, and
    finally the survivors (last, since they're what you care about). *)
val print
  :  points:Points.t list
  -> skips:Points.skip list
  -> outcomes:(Points.t * Run.outcome) list
  -> unit

(** [start_results ~path ~total ~skips] starts a results file for a run of
    [total] mutants, so that if the run is interrupted the mutants that
    finished aren't lost. Its first line is {!Points.unfinished_marker}, so
    [-from] can tell the file is partial; the skipped points follow. *)
val start_results
  :  path:string
  -> total:int
  -> only:string list
  -> skips:Points.skip list
  -> out_channel

(** Adds one mutant's line to a file from {!start_results} and flushes it. *)
val add_result : out_channel -> Points.t -> Run.outcome -> unit

(** Writes one tab-separated line per mutant: id, operator, file, line,
    binding, edit, outcome, target, part. Skipped points are included with the outcome
    [skipped], their library as the target and the reason as the part. Can be
    read back with {!Points.results}.

    Replaces [path] in one go, including a file from {!start_results}.

    If the run only covered some libraries, [only] names them, in a line
    {!Points.only} reads back. *)
val write_results
  :  path:string
  -> only:string list
  -> skips:Points.skip list
  -> (Points.t * Run.outcome) list
  -> unit
