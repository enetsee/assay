(** Writes [<lib>.muts], the list of every mutation point in a library.

    This can't be a normal build artefact: dune runs the ppx in a sandbox that
    gets deleted afterwards. Instead we write to the directory named by
    [ASSAY_MUTS_DIR], one file per library. Library names are unique within a
    dune workspace, so the files can't clash.

    Since dune doesn't know about the file:

    - Units of a library may be preprocessed in parallel, so we hold an
      exclusive lock while reading and rewriting it.
    - A rebuild only reprocesses changed units, so each write replaces just its
      own unit's block and keeps the rest.
    - If dune replays a cached action, the ppx doesn't run and nothing gets
      written. For a complete file, clear the directory and build with the
      cache disabled. If the point count drops unexpectedly, a stale cache is
      the likely cause.

    Format: a header line per unit ([#unit], module name, number of live
    points), followed by one line per point: id, operator, file, line, column,
    edit. Skipped points have an extra [skipped] field at the end. Fields are
    tab-separated with no escaping; none of them can contain a tab.

    Executables don't have a library name, so their points go under
    [exe.<dir>], e.g. [exe.bin] for executables in [bin/]. *)

val write
  :  dir:string
  -> library:string
  -> unit_name:string
  -> live:Point.t list
  -> skipped:Point.t list
  -> unit
