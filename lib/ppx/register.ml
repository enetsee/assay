open StdLabels

(* dune passes the library name as a cookie. Executables don't get one; see
   [executable_library]. *)
let library : string option ref = ref None

let () =
  Ppxlib.Driver.Cookies.add_simple_handler
    "library-name"
    Ppxlib.Ast_pattern.(estring __)
    ~f:(fun name -> library := name)
;;

(* Module name from the input path. Strips everything after the first dot,
   since preprocessed files can look like [lexer.pp.ml]. *)
let unit_name (input : string) : string =
  let base = Filename.basename input in
  let stem =
    match String.index_opt base '.' with
    | Some i -> String.sub base ~pos:0 ~len:i
    | None -> base
  in
  String.capitalize_ascii stem
;;

(* Name used in place of a library for executable modules: [exe.] followed by
   the source directory, e.g. [exe.bin] or [exe.tools.gen]. Module names are
   unique within a directory, so units can't clash, and the runner matches
   the directory against the executables' build objects. *)
let executable_library (input : string) : string =
  match Filename.dirname input with
  | "." | "" -> "exe"
  | dir -> "exe." ^ String.concat ~sep:"." (String.split_on_char ~sep:'/' dir)
;;

(* The ppx runs in a dune sandbox that gets deleted afterwards, so we need an
   absolute output directory from the environment. Fail loudly if it's unset
   rather than silently writing nothing. *)
let directory () : string =
  match Sys.getenv_opt "ASSAY_MUTS_DIR" with
  | Some dir when not (Filename.is_relative dir) -> dir
  | Some _ | None ->
    Ppxlib.Location.raise_errorf
      ~loc:Ppxlib.Location.none
      "assay: ASSAY_MUTS_DIR must be set to an absolute directory for the .muts files \
       (the ppx runs in dune's sandbox)."
;;

(* [ASSAY_ONLY] is a comma-separated list of libraries to rewrite. If unset,
   rewrite everything. Useful for narrowing things down when the build breaks. *)
let in_scope (library : string) : bool =
  match Sys.getenv_opt "ASSAY_ONLY" with
  | None -> true
  | Some names ->
    List.exists (String.split_on_char ~sep:',' names) ~f:(fun name ->
      String.equal (String.trim name) library)
;;

(* Only read once per process. *)
let arid_names : string list Lazy.t = lazy (Arid.configured ())

let rewrite (context : Ppxlib.Expansion_context.Base.t) (structure : Ppxlib.structure)
  : Ppxlib.structure
  =
  let input = Ppxlib.Expansion_context.Base.input_name context in
  let unit_name = unit_name input in
  let library =
    match !library with
    | Some name -> name
    | None -> executable_library input
  in
  if not (in_scope library)
  then structure
  else (
    let mutated =
      Mutate.run
        ~skip:Skip.skipped
        ~arid:(Arid.is_arid ~names:(Lazy.force arid_names))
        structure
    in
    Muts.write
      ~dir:(directory ())
      ~library
      ~unit_name
      ~live:mutated.live
      ~skipped:mutated.skipped;
    mutated.structure)
;;

(* Given in the instrumentation stanza, e.g. [(backend assay -skip
   %{workspace_root}/assay.skip)], with the same file in its [deps] so dune
   reprocesses when it changes. *)
let () =
  Ppxlib.Driver.add_arg
    "-skip"
    (String (fun path -> Skip.file := Some path))
    ~doc:"<path> the skip list (instead of ASSAY_SKIP)";
  Ppxlib.Driver.add_arg
    "-arid"
    (String (fun path -> Arid.file := Some path))
    ~doc:"<path> the arid list (instead of ASSAY_ARID)"
;;

(* Run [Before] other rewriters so we mutate the code as written, not
   generated code. *)
let () =
  Ppxlib.Driver.V2.register_transformation
    "assay"
    ~instrument:(Ppxlib.Driver.Instrument.V2.make rewrite ~position:Before)
;;
