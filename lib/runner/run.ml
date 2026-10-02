open StdLabels

type finish =
  | Exited of int
  | Timed_out

type outcome =
  | Killed of
      { target : string
      ; part : string
      }
  | Survived
  | Hung of string
  | Errored of string

(* The current environment plus [extra]. [ASSAY_MUTANT] is always dropped
   unless [extra] sets it, so a value left in the user's shell can't leak into
   the baseline or anything else. *)
let environment (extra : (string * string) list) : string array =
  let own = Array.to_list (Unix.environment ()) in
  let names = "ASSAY_MUTANT" :: List.map extra ~f:fst in
  let kept =
    List.filter own ~f:(fun entry ->
      not
        (List.exists names ~f:(fun name ->
           String.length entry > String.length name
           && String.equal (String.sub entry ~pos:0 ~len:(String.length name)) name
           && entry.[String.length name] = '=')))
  in
  Array.of_list (kept @ List.map extra ~f:(fun (name, value) -> name ^ "=" ^ value))
;;

let stop_signals = [ Sys.sigint; Sys.sigterm; Sys.sighup ]

(* Exit code for being killed by a signal, following the shell convention. *)
let exit_code_for (signal : int) : int =
  if signal = Sys.sigint then 130 else if signal = Sys.sighup then 129 else 143
;;

(* Runs [f] with [on_signal] handling SIGINT/SIGTERM/SIGHUP, restoring the
   previous handlers afterwards. *)
let with_stop_handler ~(on_signal : int -> unit) (f : unit -> 'a) : 'a =
  let previous =
    List.map stop_signals ~f:(fun signal ->
      signal, Sys.signal signal (Signal_handle on_signal))
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter previous ~f:(fun (signal, behaviour) -> Sys.set_signal signal behaviour))
    f
;;

let kill_group (pid : int) : unit =
  (try Unix.kill (-pid) Sys.sigkill with
   | Unix.Unix_error _ -> ());
  try Unix.kill pid Sys.sigkill with
  | Unix.Unix_error _ -> ()
;;

(* Poll every 2ms since [waitpid] can't time out. That's small compared to
   process startup, so fast tests don't get slowed down.

   The command runs in its own session, so Ctrl-C doesn't reach it. If we
   get stopped while waiting, kill its process group and run [cleanup] before
   exiting, so it isn't left running (possibly in an infinite loop). *)
let wait_until ~(cleanup : unit -> unit) (pid : int) (deadline : float) : finish =
  let rec loop () : finish =
    match Unix.waitpid [ WNOHANG ] pid with
    | 0, _ ->
      if Unix.gettimeofday () > deadline
      then (
        kill_group pid;
        (try ignore (Unix.waitpid [] pid : int * Unix.process_status) with
         | Unix.Unix_error _ -> ());
        Timed_out)
      else (
        (try ignore (Unix.select [] [] [] 0.002 : Unix.file_descr list * _ * _) with
         | Unix.Unix_error (EINTR, _, _) -> ());
        loop ())
    | _, Unix.WEXITED code -> Exited code
    | _, (Unix.WSIGNALED _ | Unix.WSTOPPED _) -> Exited 1
  in
  with_stop_handler
    ~on_signal:(fun signal ->
      kill_group pid;
      cleanup ();
      Unix._exit (exit_code_for signal))
    loop
;;

(* Runs [cmd] with [/bin/sh], returning how it finished, its stdout, and its
   stderr ([""] if [merge] sent stderr to stdout). *)
let spawn
      (cmd : string)
      ~(timeout : float)
      ~(env : (string * string) list)
      ~(merge : bool)
  : finish * string * string
  =
  let out_path = Filename.temp_file "assay" ".out" in
  let err_path = Filename.temp_file "assay" ".err" in
  let remove_files () =
    List.iter [ out_path; err_path ] ~f:(fun path ->
      try Sys.remove path with
      | Sys_error _ -> ())
  in
  Fun.protect ~finally:remove_files (fun () ->
    let out = Unix.openfile out_path [ O_WRONLY; O_TRUNC ] 0o600 in
    let err = Unix.openfile err_path [ O_WRONLY; O_TRUNC ] 0o600 in
    (* stdin is /dev/null so a test that reads it doesn't hang waiting for
          input. *)
    let null = Unix.openfile "/dev/null" [ O_RDONLY ] 0o400 in
    let close_all () = List.iter [ out; err; null ] ~f:Unix.close in
    (* Fork and [setsid] (instead of [create_process]) so the command gets
          its own process group, which we can kill as a whole on timeout.

          Prefixing the command with [exec] doesn't work: [exec cd x && ./y]
          runs nothing and exits 0, which would make every mutant look like a
          survivor. *)
    match Unix.fork () with
    | exception e ->
      close_all ();
      raise e
    | 0 ->
      (try ignore (Unix.setsid () : int) with
       | Unix.Unix_error _ -> ());
      Unix.dup2 null Unix.stdin;
      Unix.dup2 out Unix.stdout;
      Unix.dup2 (if merge then out else err) Unix.stderr;
      (try Unix.execve "/bin/sh" [| "/bin/sh"; "-c"; cmd |] (environment env) with
       | Unix.Unix_error _ -> ());
      Unix._exit 127
    | pid ->
      close_all ();
      let finish =
        wait_until ~cleanup:remove_files pid (Unix.gettimeofday () +. timeout)
      in
      finish, Files.read out_path, Files.read err_path)
;;

let command (cmd : string) ~(timeout : float) ~(env : (string * string) list)
  : finish * string
  =
  let finish, output, _ = spawn cmd ~timeout ~env ~merge:true in
  finish, output
;;

let capture (cmd : string) : string =
  let finish, output, errors = spawn cmd ~timeout:600. ~env:[] ~merge:false in
  match finish with
  | Exited 0 -> output
  | Exited code -> failwith (Printf.sprintf "%s exited %d:\n%s" cmd code errors)
  | Timed_out -> failwith (cmd ^ " did not finish")
;;

let whitespace_to_space (c : char) : char =
  match c with
  | '\t' | '\r' | '\n' | '\012' | '\011' -> ' '
  | c -> c
;;

(* Pulls the part name out of the first failure line. Returns ["?"] if there
   isn't one; the mutant still counts as killed based on the exit code. *)
let part_of (config : Config.t) (output : string) : string =
  let prefix = config.fail_prefix in
  let named =
    List.find_map (String.split_on_char ~sep:'\n' output) ~f:(fun line ->
      if
        String.length line >= String.length prefix
        && String.equal (String.sub line ~pos:0 ~len:(String.length prefix)) prefix
      then (
        let words =
          List.filter
            (String.split_on_char ~sep:' ' (String.map line ~f:whitespace_to_space))
            ~f:(fun w -> String.length w > 0)
        in
        match List.nth_opt words (config.part_field - 1) with
        | Some word -> Some word
        | None -> None)
      else None)
  in
  match named with
  | Some word -> word
  | None -> "?"
;;

let mutant (config : Config.t) (point : Points.t) (targets : Config.target list) : outcome
  =
  let rec go (remaining : Config.target list) : outcome =
    match remaining with
    | [] -> Survived
    | target :: rest ->
      let finish, output =
        command
          target.command
          ~timeout:config.timeout
          ~env:[ "ASSAY_MUTANT", string_of_int point.id ]
      in
      (match finish with
       | Timed_out -> Hung target.name
       | Exited 0 -> go rest
       | Exited _ -> Killed { target = target.name; part = part_of config output })
  in
  go targets
;;

let baseline (config : Config.t) : (Config.target * finish * float) list =
  List.map config.targets ~f:(fun (target : Config.target) ->
    let started = Unix.gettimeofday () in
    let finish, _ = command target.command ~timeout:config.timeout ~env:[] in
    target, finish, Unix.gettimeofday () -. started)
;;

(* Serialised to a single tab-separated line so the worker process can pass
   it back via a temp file. Fields have any whitespace turned into spaces so
   they can't break the format. *)
let encode (outcome : outcome) : string =
  let clean = String.map ~f:whitespace_to_space in
  match outcome with
  | Killed { target; part } -> Printf.sprintf "k\t%s\t%s" (clean target) (clean part)
  | Survived -> "s"
  | Hung target -> Printf.sprintf "h\t%s" (clean target)
  | Errored message -> Printf.sprintf "e\t%s" (clean message)
;;

let decode (line : string) : outcome option =
  match String.split_on_char ~sep:'\t' line with
  | [ "s" ] -> Some Survived
  | [ "h"; target ] -> Some (Hung target)
  | [ "k"; target; part ] -> Some (Killed { target; part })
  | [ "e"; message ] -> Some (Errored message)
  | _ -> None
;;

(* OCaml's signal numbers are its own negative constants, not the OS ones. *)
let signal_name (signal : int) : string =
  let known =
    [ Sys.sigkill, "SIGKILL"
    ; Sys.sigsegv, "SIGSEGV"
    ; Sys.sigbus, "SIGBUS"
    ; Sys.sigabrt, "SIGABRT"
    ; Sys.sigterm, "SIGTERM"
    ; Sys.sigint, "SIGINT"
    ; Sys.sigfpe, "SIGFPE"
    ; Sys.sigill, "SIGILL"
    ; Sys.sigstop, "SIGSTOP"
    ]
  in
  match List.assoc_opt signal known with
  | Some name -> name
  | None -> Printf.sprintf "signal %d" signal
;;

let parallel
      (config : Config.t)
      ~(jobs : int)
      ~(progress : int -> unit)
      ~(save : Points.t -> outcome -> unit)
      ~(check : unit -> string option)
      (work : (Points.t * Config.target list) list)
  : (Points.t * outcome) list
  =
  let work = Array.of_list work in
  let n = Array.length work in
  let jobs = max jobs 1 in
  let outcomes = Array.make n None in
  let dir = Filename.temp_file "assay-run" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  let path (i : int) : string = Filename.concat dir (string_of_int i) in
  let running = Hashtbl.create jobs in
  let next = ref 0 in
  let finished = ref 0 in
  (* Set once [check] fails. From then on nothing new is started and every
     remaining mutant is recorded as errored. *)
  let invalid = ref None in
  let record (i : int) (outcome : outcome) : unit =
    outcomes.(i) <- Some outcome;
    save (fst work.(i)) outcome;
    incr finished;
    progress !finished
  in
  (* Starts the next mutant. Returns false if fork failed while other workers
     are still running, so we can wait for one to finish and try again. *)
  let start () : bool =
    let i = !next in
    let point, targets = work.(i) in
    match Unix.fork () with
    | exception Unix.Unix_error (err, _, _) ->
      if Hashtbl.length running > 0
      then false
      else (
        incr next;
        record i (Errored ("fork failed: " ^ Unix.error_message err));
        true)
    | 0 ->
      (* The child must never return into the parent's code, whatever
         happens, so everything is caught and it always exits here. It also
         drops the parent's signal handler, which would kill its siblings. *)
      List.iter stop_signals ~f:(fun signal -> Sys.set_signal signal Signal_default);
      let outcome =
        try mutant config point targets with
        | e -> Errored (Printexc.to_string e)
      in
      (try
         let oc = open_out (path i) in
         output_string oc (encode outcome);
         close_out oc
       with
       | _ -> ());
      Unix._exit 0
    | pid ->
      incr next;
      Hashtbl.replace running pid i;
      true
  in
  let reap () : unit =
    let pid, status = Unix.waitpid [] (-1) in
    match Hashtbl.find_opt running pid with
    | None -> ()
    | Some i ->
      Hashtbl.remove running pid;
      let line =
        try String.trim (Files.read (path i)) with
        | Sys_error _ -> ""
      in
      (try Sys.remove (path i) with
       | Sys_error _ -> ());
      let outcome =
        match decode line with
        | Some outcome -> outcome
        | None ->
          let how =
            match status with
            | WEXITED code -> Printf.sprintf "exit %d" code
            | WSIGNALED signal | WSTOPPED signal -> signal_name signal
          in
          Errored (Printf.sprintf "worker exited without writing a result (%s)" how)
      in
      (match !invalid with
       | Some reason -> record i (Errored reason)
       | None ->
         (match check () with
          | None -> record i outcome
          | Some reason ->
            (* We can't tell when the change happened, so this mutant and the
               ones still running may have used the changed build. Stop the
               running ones and give up on the rest. *)
            invalid := Some reason;
            record i (Errored reason);
            Hashtbl.iter
              (fun pid _ ->
                 try Unix.kill pid Sys.sigterm with
                 | Unix.Unix_error _ -> ())
              running;
            while !next < n do
              let j = !next in
              incr next;
              record j (Errored ("not run: " ^ reason))
            done))
  in
  let remove_dir () : unit =
    (try
       Array.iter (Sys.readdir dir) ~f:(fun name ->
         try Sys.remove (Filename.concat dir name) with
         | Sys_error _ -> ())
     with
     | Sys_error _ -> ());
    try Unix.rmdir dir with
    | Unix.Unix_error _ -> ()
  in
  (* If we're stopped, pass the signal on to the workers so each one kills
     its running command, then exit. *)
  let on_signal (signal : int) : unit =
    Hashtbl.iter
      (fun pid _ ->
         try Unix.kill pid Sys.sigterm with
         | Unix.Unix_error _ -> ())
      running;
    (* Wait for the workers to kill their commands and clean up. *)
    Hashtbl.iter
      (fun pid _ ->
         try ignore (Unix.waitpid [] pid : int * Unix.process_status) with
         | Unix.Unix_error _ -> ())
      running;
    remove_dir ();
    Unix._exit (exit_code_for signal)
  in
  Fun.protect ~finally:remove_dir (fun () ->
    with_stop_handler ~on_signal (fun () ->
      while !finished < n do
        let rec fill () : unit =
          if Hashtbl.length running < jobs && !next < n && start () then fill ()
        in
        fill ();
        if Hashtbl.length running > 0 then reap ()
      done));
  List.init ~len:n ~f:(fun i -> fst work.(i), Option.get outcomes.(i))
;;
