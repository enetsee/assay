open StdLabels

type t =
  { id : int
  ; operator : string
  ; file : string
  ; line : int
  ; column : int
  ; edit : string
  ; binding : string
  ; library : string
  }

type skip =
  { point : t
  ; reason : string
  }

let fields (line : string) : string list = String.split_on_char ~sep:'\t' line

let library_of_path (path : string) : string =
  Filename.remove_extension (Filename.basename path)
;;

(* Whether a point in a [.muts] file was built, or skipped: by the skip list
   ([None]), or by an [assay.skip] attribute with its reason. *)
type state =
  | Live
  | Skipped of string option

(* Every point in the [.muts] files under [dir], with its state. *)
let read_all ~(dir : string) : (t * state) list =
  let files =
    if not (Sys.file_exists dir)
    then []
    else
      List.filter
        (List.map (Array.to_list (Sys.readdir dir)) ~f:(Filename.concat dir))
        ~f:(fun path -> Filename.check_suffix path ".muts")
  in
  List.concat_map (List.sort ~cmp:String.compare files) ~f:(fun path ->
    let library = library_of_path path in
    List.filter_map (Files.lines path) ~f:(fun line ->
      if String.length line = 0 || line.[0] = '#'
      then None
      else (
        match fields line with
        | id :: operator :: file :: line_no :: column :: binding :: edit :: marker ->
          (match
             ( int_of_string_opt id
             , int_of_string_opt line_no
             , int_of_string_opt column
             , marker )
           with
           | Some id, Some line_no, Some column, ([] | [ "skipped" ] | [ "skipped"; _ ])
             ->
             let state =
               match marker with
               | [ _; reason ] -> Skipped (Some reason)
               | [ _ ] -> Skipped None
               | _ -> Live
             in
             Some
               ( { id; operator; file; line = line_no; column; edit; binding; library }
               , state )
           | _ -> None)
        | _ -> None)))
;;

let load ~(dir : string) : t list =
  let seen = Hashtbl.create 4096 in
  List.filter_map (read_all ~dir) ~f:(fun ((point : t), state) ->
    match state with
    | Skipped _ -> None
    | Live ->
      (match Hashtbl.find_opt seen point.id with
       | Some (other : t) ->
         failwith
           (Printf.sprintf
              "hash collision: id %d is shared by %s at %s:%d and %s at %s:%d"
              point.id
              other.edit
              other.file
              other.line
              point.edit
              point.file
              point.line)
       | None ->
         Hashtbl.replace seen point.id point;
         Some point))
;;

(* Entries in the skip file, in file order: one id per line, optionally
   followed by a tab and the reason. *)
let entries (path : string option) : (int * string) list =
  match path with
  | Some path when Sys.file_exists path ->
    List.filter_map (Files.lines path) ~f:(fun line ->
      if String.length line > 0 && line.[0] <> '#'
      then (
        let id, reason =
          match String.index_opt line '\t' with
          | Some i ->
            ( String.sub line ~pos:0 ~len:i
            , String.sub line ~pos:(i + 1) ~len:(String.length line - i - 1) )
          | None -> line, ""
        in
        Option.map (fun id -> id, String.trim reason) (int_of_string_opt (String.trim id)))
      else None)
  | Some _ | None -> []
;;

let reasons (path : string option) : (int, string) Hashtbl.t =
  let table = Hashtbl.create 64 in
  List.iter (entries path) ~f:(fun (id, reason) -> Hashtbl.replace table id reason);
  table
;;

type stale =
  { missing : (int * string) list
  ; unapplied : (t * string) list
  }

let stale ~(dir : string) ~(reasons_file : string option) : stale =
  let found = Hashtbl.create 4096 in
  List.iter (read_all ~dir) ~f:(fun ((point : t), state) ->
    Hashtbl.replace found point.id (point, state));
  let missing, unapplied =
    List.fold_left
      (entries reasons_file)
      ~init:([], [])
      ~f:(fun (missing, unapplied) (id, reason) ->
        match Hashtbl.find_opt found id with
        | None -> (id, reason) :: missing, unapplied
        | Some (_, Skipped _) -> missing, unapplied
        | Some (point, Live) -> missing, (point, reason) :: unapplied)
  in
  { missing = List.rev missing; unapplied = List.rev unapplied }
;;

let skips ~(dir : string) ~(reasons_file : string option) : skip list =
  let reasons = reasons reasons_file in
  List.filter_map (read_all ~dir) ~f:(fun ((point : t), state) ->
    match state with
    | Live -> None
    | Skipped (Some reason) -> Some { point; reason }
    | Skipped None ->
      let reason =
        match Hashtbl.find_opt reasons point.id with
        | Some reason when String.length reason > 0 -> reason
        | Some _ | None -> "no reason given"
      in
      Some { point; reason })
;;

let unfinished_marker = "# unfinished run:"

let unfinished (path : string) : int option =
  match Files.lines path with
  | first :: _
    when String.length first > String.length unfinished_marker
         && String.equal
              (String.sub first ~pos:0 ~len:(String.length unfinished_marker))
              unfinished_marker ->
    let rest =
      String.sub
        first
        ~pos:(String.length unfinished_marker)
        ~len:(String.length first - String.length unfinished_marker)
    in
    Scanf.sscanf_opt rest " %d" Fun.id
  | _ -> None
;;

let only_marker = "# only:"

let only (path : string) : string list =
  List.concat_map (Files.lines path) ~f:(fun line ->
    if
      String.length line > String.length only_marker
      && String.equal
           (String.sub line ~pos:0 ~len:(String.length only_marker))
           only_marker
    then
      List.filter_map
        (String.split_on_char
           ~sep:','
           (String.sub
              line
              ~pos:(String.length only_marker)
              ~len:(String.length line - String.length only_marker)))
        ~f:(fun name ->
          let name = String.trim name in
          if String.length name = 0 then None else Some name)
    else [])
;;

let results (path : string) : (t * string * string * string) list =
  List.filter_map (Files.lines path) ~f:(fun line ->
    (* Files from before bindings were recorded have no binding field. *)
    let fields =
      match fields line with
      | [ id; operator; file; line_no; edit; verdict; target; part ] ->
        Some (id, operator, file, line_no, "", edit, verdict, target, part)
      | [ id; operator; file; line_no; binding; edit; verdict; target; part ] ->
        Some (id, operator, file, line_no, binding, edit, verdict, target, part)
      | _ -> None
    in
    match fields with
    | Some (id, operator, file, line_no, binding, edit, verdict, target, part) ->
      (match int_of_string_opt id, int_of_string_opt line_no with
       | Some id, Some line_no ->
         Some
           ( { id
             ; operator
             ; file
             ; line = line_no
             ; column = 0
             ; edit
             ; binding
             ; library = ""
             }
           , verdict
           , target
           , part )
       | _, _ -> None)
    | None -> None)
;;
