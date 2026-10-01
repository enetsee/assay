open StdLabels

type t =
  { linked : (string * string list) list (** target name, the libraries it links *)
  ; executables : (string * string) list (** target name, the file dune builds *)
  ; unplaced : (string * string) list (** target name, why it wasn't placed *)
  }

(* Test executables and their paths, from [dune describe tests]. Plain
   [dune describe] doesn't include tests. *)
(* Asking dune is meant to be cheap: a describe, and one [dune rules] per
   target, each a few hundredths of a second on a tree that is already built. A
   call that takes seconds is dune doing something else — waiting on a build
   lock, or rebuilding because it was asked about a configuration the tree is
   not in — and the run is about to be far slower than it looks. So the slow
   ones say so rather than looking like a hang. *)
let asking (command : string) : string =
  let started = Unix.gettimeofday () in
  let answer = Run.capture command in
  let seconds = Unix.gettimeofday () -. started in
  if seconds > 1. then Printf.eprintf "  %.1fs  %s\n%!" seconds command;
  answer
;;

let test_targets () : (string * string) list =
  match Sexp.of_string (asking "dune describe tests") with
  | Sexp.Atom _ -> []
  | Sexp.List entries ->
    List.filter_map entries ~f:(fun entry ->
      match Sexp.atom_field "name" entry, Sexp.atom_field "target" entry with
      | Some name, Some target -> Some (name, target)
      | _, _ -> None)
;;

(* Non-test executables and their paths. The directory comes from the first
   module's path. *)
let workspace_targets () : (string * string) list =
  match Sexp.of_string (asking "dune describe") with
  | Sexp.Atom _ -> []
  | Sexp.List entries ->
    List.concat_map entries ~f:(fun entry ->
      match entry with
      | Sexp.List [ Sexp.Atom "executables"; body ] ->
        let directory =
          match Sexp.field "modules" body with
          | Some [ Sexp.List (first :: _) ] ->
            (* dune writes this option as [(impl (path))], but accept
               [(impl path)] too. *)
            (match Sexp.field "impl" first with
             | Some [ Sexp.Atom path ] | Some [ Sexp.List [ Sexp.Atom path ] ] ->
               Some (Filename.dirname path)
             | _ -> None)
          | _ -> None
        in
        let names =
          match Sexp.field "names" body with
          | Some [ Sexp.List items ] ->
            List.filter_map items ~f:(fun item ->
              match item with
              | Sexp.Atom name -> Some name
              | Sexp.List _ -> None)
          | _ -> []
        in
        (match directory with
         | None -> []
         | Some directory ->
           List.map names ~f:(fun name -> name, Filename.concat directory (name ^ ".exe")))
      | Sexp.Atom _ | Sexp.List _ -> [])
;;

(* [_build/default/bin/.main.eobjs/native/x.cmx] -> [Some "exe.bin"], matching
   the name the ppx gives executable modules. *)
let executable_of_path (path : string) : string option =
  let parts = String.split_on_char ~sep:'/' path in
  let parts =
    match parts with
    | "_build" :: _context :: rest -> rest
    | _context :: rest -> rest
    | [] -> []
  in
  let rec dir (acc : string list) (parts : string list) : string option =
    match parts with
    | [] -> None
    | part :: rest ->
      if Filename.check_suffix part ".eobjs"
      then (
        match List.rev acc with
        | [] -> Some "exe"
        | segments -> Some ("exe." ^ String.concat ~sep:"." segments))
      else dir (part :: acc) rest
  in
  dir [] parts
;;

(* Library names from the archives the link rule depends on, plus [exe.<dir>]
   for executable modules linked directly. Only looks at paths inside the
   build dir; external libraries can't contain mutants. *)
let libraries_of_rule (rule : Sexp.t) : string list =
  let rec paths (form : Sexp.t) : string list =
    match form with
    | Sexp.Atom _ -> []
    | Sexp.List [ Sexp.Atom "In_build_dir"; Sexp.Atom path ] -> [ path ]
    | Sexp.List items -> List.concat_map items ~f:paths
  in
  let deps =
    match Sexp.field "deps" rule with
    | Some items -> List.concat_map items ~f:paths
    | None -> []
  in
  List.sort_uniq
    ~cmp:String.compare
    (List.filter_map deps ~f:(fun path ->
       if Filename.check_suffix path ".cmxa" || Filename.check_suffix path ".cma"
       then Some (Filename.remove_extension (Filename.basename path))
       else executable_of_path path))
;;

(* The files a rule builds, from [(targets ((files (...)) ...))]. *)
let targets_of_rule (rule : Sexp.t) : string list =
  match Sexp.field "targets" rule with
  | Some [ targets ] ->
    (match Sexp.field "files" targets with
     | Some [ Sexp.List files ] ->
       List.filter_map files ~f:(fun file ->
         match file with
         | Sexp.Atom path -> Some path
         | Sexp.List _ -> None)
     | _ -> [])
  | _ -> []
;;

let contains ~(sub : string) (s : string) : bool =
  let n = String.length sub in
  let rec go (i : int) : bool =
    i + n <= String.length s
    && (String.equal (String.sub s ~pos:i ~len:n) sub || go (i + 1))
  in
  n > 0 && go 0
;;

(* A path relative to the source tree: [_build/default/a/t.exe] -> [a/t.exe]. *)
let source_relative (path : string) : string =
  match String.split_on_char ~sep:'/' path with
  | "_build" :: _context :: rest -> String.concat ~sep:"/" rest
  | _ -> path
;;

(* Picks which of several same-named executables a target means, by looking
   for its path in the target's command, or failing that its directory. *)
let disambiguate (command : string) (candidates : string list) : string option =
  let unique (matches : string list) : string option =
    match matches with
    | [ one ] -> Some one
    | _ -> None
  in
  match
    unique
      (List.filter candidates ~f:(fun path ->
         contains ~sub:(source_relative path) command))
  with
  | Some path -> Some path
  | None ->
    (* Longest matching directory, so [test/a/b] beats [test/a]. *)
    let by_dir =
      List.filter candidates ~f:(fun path ->
        contains ~sub:(Filename.dirname (source_relative path)) command)
    in
    let longest =
      List.fold_left by_dir ~init:0 ~f:(fun acc path ->
        max acc (String.length (Filename.dirname (source_relative path))))
    in
    unique
      (List.filter by_dir ~f:(fun path ->
         String.length (Filename.dirname (source_relative path)) = longest))
;;

let build (targets : Config.target list) : t =
  let where = test_targets () @ workspace_targets () in
  let linked = ref [] in
  let executables = ref [] in
  let unplaced = ref [] in
  List.iter targets ~f:(fun ({ name; command } : Config.target) ->
    let candidates =
      List.filter_map where ~f:(fun (n, path) ->
        if String.equal n name then Some path else None)
    in
    let chosen =
      match candidates with
      | [] -> Error "dune doesn't know it"
      | [ path ] -> Ok path
      | several ->
        (match disambiguate command several with
         | Some path -> Ok path
         | None ->
           Error
             (Printf.sprintf
                "dune has %d executables with that name and the command doesn't say \
                 which (%s)"
                (List.length several)
                (String.concat ~sep:", " (List.map several ~f:source_relative))))
    in
    match chosen with
    | Error reason -> unplaced := (name, reason) :: !unplaced
    | Ok target ->
      let rules = Sexp.many (asking ("dune rules " ^ Filename.quote target)) in
      let libraries =
        List.sort_uniq ~cmp:String.compare (List.concat_map rules ~f:libraries_of_rule)
      in
      linked := (name, libraries) :: !linked;
      (* The rule that links the executable is the one that builds a file
         with the same name as the target path. *)
      List.iter (List.concat_map rules ~f:targets_of_rule) ~f:(fun file ->
        if String.equal (Filename.basename file) (Filename.basename target)
        then executables := (name, file) :: !executables));
  { linked = List.rev !linked
  ; executables = List.rev !executables
  ; unplaced = List.rev !unplaced
  }
;;

let reaches (t : t) (library : string) : string list =
  List.map t.unplaced ~f:fst
  @ List.filter_map t.linked ~f:(fun (name, libraries) ->
    if List.mem library ~set:libraries then Some name else None)
;;

let unplaced (t : t) : (string * string) list = t.unplaced
let executables (t : t) : (string * string) list = t.executables
