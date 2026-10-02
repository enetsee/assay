(** Running commands and mutants.

    Everything runs with a timeout, since some mutants cause infinite loops
    (e.g. removing the [incr i] from a loop). A timeout counts as its own
    outcome. *)

type finish =
  | Exited of int
  | Timed_out

(** [command cmd ~timeout ~env] runs [cmd] with [/bin/sh], adding [env] to
    the current environment. [ASSAY_MUTANT] is removed from the environment
    unless [env] sets it. Returns how it finished and the combined
    stdout/stderr.

    The command runs in its own process group so that on timeout we can kill
    the shell and everything it started. If assay is interrupted while
    waiting, the command's process group is killed too. *)
val command : string -> timeout:float -> env:(string * string) list -> finish * string

(** [capture cmd] returns the stdout of [cmd]. Raises [Failure] (including
    its stderr) if it fails. *)
val capture : string -> string

type outcome =
  | Killed of
      { target : string
      ; part : string (** Parsed from the failure line, or ["?"]. *)
      }
  | Survived
  | Hung of string (** The target that timed out. *)
  | Errored of string
  (** The mutant couldn't be run (e.g. fork failed, or the worker process
      died), so we don't know the outcome. *)

(** [mutant config point targets] sets [ASSAY_MUTANT] and runs the targets in
    order, stopping at the first failure. We only need to know that something
    killed the mutant, not everything that would have. *)
val mutant : Config.t -> Points.t -> Config.target list -> outcome

(** Runs every target once with no mutant selected, and times them.

    This catches a suite that's already failing, and also a misconfigured
    command that does nothing and exits 0 (which would make every mutant look
    like a survivor). The timings help spot the latter: a target that should
    take seconds finishing instantly is suspicious. *)
val baseline : Config.t -> (Config.target * finish * float) list

(** [parallel config ~jobs ~progress work] runs all the mutants, [jobs] at a
    time, and returns outcomes in the same order as [work].

    Each worker handles one mutant and runs its targets sequentially, so the
    first failure still short-circuits. Running in parallel assumes the
    targets only read from the build directory.

    [progress] is called with the number completed after each mutant.

    [save] is called with each mutant's outcome as soon as it's known, so it
    can be written out before the run ends. If the run is interrupted, the
    mutants still running are never passed to it.

    [check] is called after each mutant finishes, and returns a reason if the
    build has changed (e.g. another [dune build] replaced a target). Then the
    mutant that just finished, any still running, and any not yet started are
    all recorded as [Errored] with that reason, since they may have run
    against the wrong build.

    If a worker fails or dies without writing a result, that mutant is
    recorded as [Errored] and the run carries on, rather than guessing an
    outcome or losing the rest of the run. *)
val parallel
  :  Config.t
  -> jobs:int
  -> progress:(int -> unit)
  -> save:(Points.t -> outcome -> unit)
  -> check:(unit -> string option)
  -> (Points.t * Config.target list) list
  -> (Points.t * outcome) list
