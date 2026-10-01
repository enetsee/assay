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
    ; "-j", Arg.Set_int jobs, "<n> how many mutants to run at a time (default 1)"
    ]
    (fun arg -> raise (Arg.Bad (Printf.sprintf "unknown argument %S" arg)))
    "assay [options]";
  let today =
    let t = Unix.localtime (Unix.gettimeofday ()) in
    Printf.sprintf "%04d-%02d-%02d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
  in
  (* [-from]: regenerate the report from an existing results file without
     running anything. *)
  if String.length !from > 0
  then (
    let rows = Assay_runner.Points.results !from in
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
    exit 0);
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
  (match config.build, !build with
   | Some command, true ->
     prerr_endline ("building: " ^ command);
     let finish, output =
       Assay_runner.Run.command command ~timeout:3600. ~env:build_env
     in
     (match finish with
      | Assay_runner.Run.Exited 0 -> ()
      | Assay_runner.Run.Exited code ->
        prerr_string output;
        Printf.eprintf "build failed (exit %d)\n" code;
        exit 1
      | Assay_runner.Run.Timed_out ->
        prerr_endline "build timed out";
        exit 1)
   | Some _, false | None, _ -> ());
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
  let outcomes =
    Assay_runner.Run.parallel
      config
      ~jobs:!jobs
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
  Assay_runner.Table.write_results ~path:!results ~skips outcomes;
  Assay_runner.Records.write ~path:!records ~today outcomes;
  Printf.printf "\none line per mutant in %s, a record per file in %s\n" !results !records
;;
