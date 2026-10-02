open StdLabels

(* CLI entry point. Builds the project with instrumentation, loads the
   mutation points, works out which targets cover each library, runs every
   mutant and prints a summary. *)

let absolute (path : string) : string =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
;;

let () =
  let config_path = ref "assay.conf" in
  let only = ref [] in
  let build = ref true in
  let results = ref "assay.results" in
  let records = ref "assay.records" in
  let jobs = ref 1 in
  let from = ref "" in
  let check_records = ref "" in
  let update_records = ref "" in
  Arg.parse
    [ "-config", Arg.Set_string config_path, "<path> the config to read"
    ; ( "-only"
      , Arg.String (fun names -> only := !only @ String.split_on_char ~sep:',' names)
      , "<library,...> only run mutants in these libraries" )
    ; "-no-build", Arg.Clear build, " skip the build step"
    ; "-results", Arg.Set_string results, "<path> where to write one line per mutant"
    ; ( "-from"
      , Arg.Set_string from
      , "<path> regenerate the report from an existing results file" )
    ; ( "-records"
      , Arg.Set_string records
      , "<path> where to write a per-file summary of results" )
    ; ( "-check-records"
      , Arg.Set_string check_records
      , "<dir> report records in the tests under <dir> that this run would change (exits \
         1 if any)" )
    ; ( "-update-records"
      , Arg.Set_string update_records
      , "<dir> rewrite the records in the tests under <dir> to match this run" )
    ; "-j", Arg.Set_int jobs, "<n> how many mutants to run at a time (default 1)"
    ]
    (fun arg -> raise (Arg.Bad (Printf.sprintf "unknown argument %S" arg)))
    "assay [options]";
  let today =
    let t = Unix.localtime (Unix.gettimeofday ()) in
    Printf.sprintf "%04d-%02d-%02d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
  in
  (* [-check-records] and [-update-records]: compare this run's records with
     the blocks in the tests, and rewrite them. [complete] is whether the run
     covered every library; [refuse] is why the run can't be trusted to, if
     it can't. Returns whether -check-records found anything to fix. *)
  let place_records
        ~(complete : bool)
        ~(refuse : string option)
        (outcomes : (Assay_runner.Points.t * Assay_runner.Run.outcome) list)
    : bool
    =
    let dir, update =
      if String.length !update_records > 0
      then !update_records, true
      else !check_records, false
    in
    (* With no results, every block would look orphaned and be deleted. *)
    let refuse =
      match refuse, outcomes with
      | Some reason, _ -> Some reason
      | None, [] -> Some "the run has no results"
      | None, _ :: _ -> None
    in
    if String.length dir = 0
    then false
    else (
      match refuse with
      | Some reason ->
        Printf.printf "\nrecords in %s left alone: %s\n" dir reason;
        not update
      | None ->
        let blocks = Assay_runner.Records.blocks ~today outcomes in
        let report =
          (if update then Assay_runner.Placed.update else Assay_runner.Placed.check)
            ~dir
            ~blocks
            ~complete
        in
        (* Where a homeless block could go: the test named after the target
           that killed most of that file's mutants. *)
        let suggestion (source : string) : string =
          let killers =
            List.filter_map outcomes ~f:(fun ((p : Assay_runner.Points.t), outcome) ->
              match outcome with
              | Assay_runner.Run.Killed { target; _ } when String.equal p.file source ->
                Some target
              | _ -> None)
          in
          match Assay_runner.Summary.tally killers with
          | [] -> "nothing killed its mutants"
          | (target, _) :: _ ->
            (match Assay_runner.Placed.test_named ~dir target with
             | Some test -> Printf.sprintf "%s, most kills by %s" test target
             | None -> Printf.sprintf "most kills by %s" target)
        in
        Printf.printf "\nrecords in %s:\n" dir;
        List.iter report.stale ~f:(fun (test, source) ->
          Printf.printf
            "  %-11s %s in %s\n"
            (if update then "updated" else "out of date")
            source
            test);
        List.iter report.orphaned ~f:(fun (test, source) ->
          Printf.printf
            "  %-11s %s from %s (it has no mutants now)\n"
            (if update then "removed" else "orphaned")
            source
            test);
        List.iter report.homeless ~f:(fun source ->
          Printf.printf "  %-11s %s (%s)\n" "no home" source (suggestion source));
        List.iter report.outside ~f:(fun (test, source) ->
          Printf.printf "  %-11s %s in %s\n" "not in run" source test);
        Printf.printf
          "  %d up to date%s\n"
          report.current
          (if update && report.homeless <> []
           then "; place the ones with no home by hand, and the next run will keep them"
           else "");
        (not update)
        && (report.stale <> [] || report.orphaned <> [] || report.homeless <> []))
  in
  (* [-from]: regenerate the report from an existing results file without
     running anything. *)
  if String.length !from > 0
  then (
    let rows = Assay_runner.Points.results !from in
    (match Assay_runner.Points.unfinished !from with
     | Some total ->
       let done_ =
         List.length
           (List.filter rows ~f:(fun (_, verdict, _, _) ->
              not (String.equal verdict "skipped")))
       in
       Printf.eprintf
         "warning: %s is from a run that didn't finish. It has results for %d of %d \
          mutants.\n"
         !from
         done_
         total
     | None -> ());
    let skips =
      List.filter_map
        rows
        ~f:(fun ((point : Assay_runner.Points.t), verdict, library, reason) ->
          if String.equal verdict "skipped"
          then Some { Assay_runner.Points.point = { point with library }; reason }
          else None)
    in
    let outcomes =
      List.filter_map rows ~f:(fun (point, verdict, target, part) ->
        match verdict with
        | "killed" -> Some (point, Assay_runner.Run.Killed { target; part })
        | "hung" -> Some (point, Assay_runner.Run.Hung target)
        | "error" -> Some (point, Assay_runner.Run.Errored part)
        | "survived" -> Some (point, Assay_runner.Run.Survived)
        | "skipped" -> None
        | other -> failwith (Printf.sprintf "%s: unknown outcome %S" !from other))
    in
    Assay_runner.Table.print ~points:(List.map outcomes ~f:fst) ~skips ~outcomes;
    Assay_runner.Records.write ~path:!records ~today outcomes;
    Printf.printf "\na record per file in %s\n" !records;
    let stale =
      place_records
        ~complete:(Assay_runner.Points.only !from = [])
        ~refuse:
          (Option.map
             (fun _ -> "the results are from a run that didn't finish")
             (Assay_runner.Points.unfinished !from))
        outcomes
    in
    exit (if stale then 1 else 0));
  let config = Assay_runner.Config.load !config_path in
  let muts = absolute config.muts in
  let skip_path = Option.map absolute config.skip in
  let arid_path = Option.map absolute config.arid in
  (* Paths need to be absolute because the ppx runs inside dune's sandbox. *)
  let named (name : string) (path : string option) : (string * string) list =
    match path with
    | Some path -> [ name, path ]
    | None -> []
  in
  let build_env =
    (("ASSAY_MUTS_DIR", muts) :: named "ASSAY_SKIP" skip_path)
    @ named "ASSAY_ARID" arid_path
  in
  (* A mutant that doesn't type-check breaks the build. When an error starts
     exactly at a live point, that's the cause: add the point to the skip
     list and build again, until the build passes or an error isn't at a
     point (a real problem, which we report). *)
  let rec build_until_it_passes (added : int) : unit =
    match config.build with
    | None -> ()
    | Some command ->
      prerr_endline ("building: " ^ command);
      let finish, output =
        Assay_runner.Run.command command ~timeout:3600. ~env:build_env
      in
      let fail (message : string) : 'a =
        prerr_string output;
        prerr_endline message;
        exit 1
      in
      (match finish with
       | Assay_runner.Run.Exited 0 ->
         if added > 0
         then
           Printf.eprintf
             "added %d %s to %s that %s\n"
             added
             (if added = 1 then "point" else "points")
             (Option.value config.skip ~default:"")
             (if added = 1 then "doesn't type-check" else "don't type-check")
       | Assay_runner.Run.Timed_out -> fail "build timed out"
       | Assay_runner.Run.Exited code ->
         let failed = Printf.sprintf "build failed (exit %d)" code in
         let live = Assay_runner.Points.load ~dir:muts in
         let at (error : Assay_runner.Derive.error) =
           List.find_opt live ~f:(fun (p : Assay_runner.Points.t) ->
             String.equal p.file error.file
             && p.line = error.line
             && p.column = error.column)
           |> Option.map (fun (p : Assay_runner.Points.t) -> p, error)
         in
         let found =
           List.sort_uniq
             ~cmp:(fun ((a : Assay_runner.Points.t), _) (b, _) -> compare a.id b.id)
             (List.filter_map (Assay_runner.Derive.errors output) ~f:at)
         in
         let listed = List.map (Assay_runner.Points.entries skip_path) ~f:fst in
         (match skip_path, found with
          | _, [] -> fail failed
          | None, _ :: _ ->
            fail
              (failed
               ^ ". Some errors are at mutation points that don't type-check. Set [skip] \
                  in the config and assay will add them to the skip list itself.")
          | Some path, _ :: _ ->
            (match
               List.filter found ~f:(fun ((p : Assay_runner.Points.t), _) ->
                 List.mem p.id ~set:listed)
             with
             | _ :: _ ->
               fail
                 (failed
                  ^ ". Points already on the skip list were built anyway, because dune \
                     doesn't know the skip list is an input. Pass it to the ppx with \
                     -skip and list it in deps (see the README), or run dune clean and \
                     build again.")
             | [] ->
               List.iter found ~f:(fun ((p : Assay_runner.Points.t), _) ->
                 Printf.eprintf "  skipping %s at %s:%d\n" p.edit p.file p.line);
               Assay_runner.Derive.add
                 ~path
                 (List.map found ~f:(fun ((p : Assay_runner.Points.t), error) ->
                    p.id, Assay_runner.Derive.reason error));
               build_until_it_passes (added + List.length found))))
  in
  if !build then build_until_it_passes 0;
  let points = Assay_runner.Points.load ~dir:muts in
  let skips = Assay_runner.Points.skips ~dir:muts ~reasons_file:skip_path in
  let skips =
    match !only with
    | [] -> skips
    | wanted ->
      List.filter skips ~f:(fun (s : Assay_runner.Points.skip) ->
        List.mem s.point.library ~set:wanted)
  in
  let points =
    match !only with
    | [] -> points
    | wanted ->
      List.filter points ~f:(fun (p : Assay_runner.Points.t) ->
        List.mem p.library ~set:wanted)
  in
  (* An entry whose point's code changed (or was deleted) skips nothing, and
     one dune didn't pick up skips nothing either. Say so, rather than let
     them fail silently. *)
  let stale = Assay_runner.Points.stale ~dir:muts ~reasons_file:skip_path in
  let skip_name = Option.value config.skip ~default:"" in
  let reason_or_none (reason : string) : string =
    if String.length reason = 0 then "(no reason given)" else reason
  in
  (* Entries assay added itself are a cache: if their point has gone, the
     code changed and the build would say if the new code needs one. Only
     after a full build, though: without one, or with ASSAY_ONLY, a point can
     be missing just because it wasn't built. *)
  let derived, written =
    List.partition stale.missing ~f:(fun (_, reason) ->
      Assay_runner.Derive.derived reason)
  in
  let stale =
    match skip_path, derived with
    | Some path, _ :: _ when !build && Option.is_none (Sys.getenv_opt "ASSAY_ONLY") ->
      Assay_runner.Derive.remove ~path (List.map derived ~f:fst);
      Printf.eprintf
        "removed %d %s from %s that no longer %s\n"
        (List.length derived)
        (if List.length derived = 1 then "entry" else "entries")
        skip_name
        (if List.length derived = 1 then "matches a point" else "match a point");
      { stale with missing = written }
    | _, _ -> stale
  in
  if stale.missing <> []
  then (
    Printf.eprintf
      "warning: these entries in %s match no mutation point, so they skip nothing. The \
       code they were for has changed or gone; remove them, or find the points again and \
       update the ids.%s\n"
      skip_name
      (match Sys.getenv_opt "ASSAY_ONLY" with
       | Some _ -> " ASSAY_ONLY is set, so some may be for libraries that weren't built."
       | None -> "");
    List.iter stale.missing ~f:(fun (id, reason) ->
      Printf.eprintf "  %d  %s\n" id (reason_or_none reason)));
  if stale.unapplied <> []
  then (
    Printf.eprintf
      "warning: these entries in %s weren't applied, so their mutants will run. dune \
       reused older preprocessing because it doesn't know the skip list is an input. \
       Pass it to the ppx with -skip and list it in deps (see the README), or run dune \
       clean and build again.\n"
      skip_name;
    List.iter stale.unapplied ~f:(fun ((p : Assay_runner.Points.t), reason) ->
      Printf.eprintf
        "  %d  %s:%d  %s  %s\n"
        p.id
        p.file
        p.line
        p.edit
        (reason_or_none reason)));
  if points = []
  then (
    Printf.eprintf "no mutation points found in %s. Was the build instrumented?\n" muts;
    exit 1);
  prerr_endline "running baseline (no mutant)";
  let red = ref false in
  List.iter
    (Assay_runner.Run.baseline config)
    ~f:(fun ((t : Assay_runner.Config.target), finish, seconds) ->
      let verdict =
        match finish with
        | Assay_runner.Run.Exited 0 -> "ok"
        | Assay_runner.Run.Exited code ->
          red := true;
          Printf.sprintf "exited %d" code
        | Assay_runner.Run.Timed_out ->
          red := true;
          "did not finish"
      in
      Printf.eprintf "  %7.2fs  %-16s %s\n" seconds t.name verdict);
  if !red
  then (
    prerr_endline "baseline failed: fix the failing target(s) before running mutants";
    exit 1);
  prerr_endline "querying dune for target dependencies";
  let graph = Assay_runner.Graph.build config.targets in
  List.iter (Assay_runner.Graph.unplaced graph) ~f:(fun (name, reason) ->
    Printf.eprintf
      "warning: can't tell what target %s links (%s); running it for every mutant\n"
      name
      reason);
  let selected (library : string) : Assay_runner.Config.target list =
    let reached = Assay_runner.Graph.reaches graph library in
    List.filter config.targets ~f:(fun (t : Assay_runner.Config.target) ->
      List.mem t.name ~set:reached
      &&
      match List.assoc_opt t.name config.covers with
      | None -> true
      | Some libraries -> List.mem library ~set:libraries)
  in
  let total = List.length points in
  let started = Unix.gettimeofday () in
  let spoke = ref 0. in
  let work =
    List.map points ~f:(fun (point : Assay_runner.Points.t) ->
      point, selected point.library)
  in
  (* Mutants no target links can't be killed, and would otherwise just show
     up as survivors with nothing to say why. *)
  let unreached = Hashtbl.create 16 in
  List.iter work ~f:(fun ((point : Assay_runner.Points.t), targets) ->
    if List.is_empty targets
    then
      Hashtbl.replace
        unreached
        point.library
        (1 + Option.value (Hashtbl.find_opt unreached point.library) ~default:0));
  List.iter
    (List.sort ~cmp:compare (Hashtbl.fold (fun k v acc -> (k, v) :: acc) unreached []))
    ~f:(fun (library, n) ->
      Printf.eprintf
        "warning: no target links %s, so its %d mutants will be reported as survivors \
         without running\n"
        library
        n);
  (* Remember each target executable so we can tell if something (usually a
     plain [dune build] in another shell) replaces it partway through. That
     would silently turn every remaining mutant into a survivor. *)
  let identity (path : string) : (int * int * int * float) option =
    match Unix.stat path with
    | st -> Some (st.st_dev, st.st_ino, st.st_size, st.st_mtime)
    | exception Unix.Unix_error _ -> None
  in
  let snapshot =
    List.filter_map (Assay_runner.Graph.executables graph) ~f:(fun (name, path) ->
      Option.map (fun id -> name, path, id) (identity path))
  in
  let rebuilt () : (string * string) option =
    List.find_map snapshot ~f:(fun (name, path, id) ->
      if Option.equal ( = ) (identity path) (Some id) then None else Some (name, path))
  in
  let check () : string option =
    Option.map
      (fun (name, _) -> Printf.sprintf "target %s was rebuilt mid-run" name)
      (rebuilt ())
  in
  (* On a terminal, overwrite one progress line; otherwise (piped, redirected)
     print a line per update so it's readable as it goes. *)
  let tty = Unix.isatty Unix.stderr in
  (* Results are written as each mutant finishes, so an interrupted run still
     leaves the ones that finished. *)
  (* The libraries this run was limited to, if it was. *)
  let limited =
    !only
    @
    match Sys.getenv_opt "ASSAY_ONLY" with
    | Some names ->
      List.filter
        (List.map (String.split_on_char ~sep:',' names) ~f:String.trim)
        ~f:(fun name -> String.length name > 0)
    | None -> []
  in
  let partial =
    Assay_runner.Table.start_results ~path:!results ~total ~only:limited ~skips
  in
  let outcomes =
    Assay_runner.Run.parallel
      config
      ~jobs:!jobs
      ~save:(Assay_runner.Table.add_result partial)
        (* Every twenty-five, and every two seconds besides. A run of eight
         mutants would otherwise print nothing at all between the last message
         and the table, and silence for minutes reads as a hang. *)
      ~progress:(fun finished ->
        let now = Unix.gettimeofday () in
        let due = now -. !spoke > 2. in
        if due then spoke := now;
        if finished mod 25 = 0 || finished = total || due
        then
          Printf.eprintf
            "%s%d of %d, %.0fs elapsed%s%!"
            (if tty then "\r" else "")
            finished
            total
            (Unix.gettimeofday () -. started)
            (if tty then "" else "\n"))
      ~check
      work
  in
  close_out partial;
  (* Rewrite the file in order now, before anything else can fail. *)
  Assay_runner.Table.write_results ~path:!results ~only:limited ~skips outcomes;
  if tty then prerr_newline ();
  (match rebuilt () with
   | Some (name, path) ->
     Printf.eprintf
       "error: target %s (%s) was rebuilt during the run, probably by a dune build in \
        another shell. Mutants from that point on weren't run (see \"could not be run\" \
        below). Rebuild with instrumentation and run again.\n"
       name
       path
   | None -> ());
  (* If nothing was killed, the build probably isn't instrumented (e.g. a
     plain [dune build] ran in between). The baseline can't catch this since
     the tests pass either way. *)
  if
    List.for_all outcomes ~f:(fun (_, outcome) ->
      match outcome with
      | Assay_runner.Run.Killed _ -> false
      | Assay_runner.Run.Survived | Assay_runner.Run.Hung _ | Assay_runner.Run.Errored _
        -> true)
  then
    prerr_endline
      "warning: no mutants were killed. The build may not be instrumented (did a plain \
       dune build run in between?).";
  Assay_runner.Table.print ~points ~skips ~outcomes;
  Assay_runner.Records.write ~path:!records ~today outcomes;
  Printf.printf "\none line per mutant in %s, a record per file in %s\n" !results !records;
  let stale =
    place_records
      ~complete:(limited = [])
      ~refuse:
        (Option.map
           (fun (name, _) -> Printf.sprintf "target %s was rebuilt during the run" name)
           (rebuilt ()))
      outcomes
  in
  if stale then exit 1
;;
