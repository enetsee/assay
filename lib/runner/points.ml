open StdLabels

type t =
  { id : int
  ; operator : string
  ; file : string
  ; line : int
  ; column : int
  ; edit : string
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

(* Every point in the [.muts] files under [dir], paired with whether it's
   marked as skipped. *)
let read_all ~(dir : string) : (t * bool) list =
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
        | id :: operator :: file :: line_no :: column :: edit :: marker ->
          (match
             ( int_of_string_opt id
             , int_of_string_opt line_no
             , int_of_string_opt column
             , marker )
           with
           | Some id, Some line_no, Some column, ([] | [ "skipped" ]) ->
             Some
               ( { id; operator; file; line = line_no; column; edit; library }
               , not (List.is_empty marker) )
           | _ -> None)
        | _ -> None)))
;;

let load ~(dir : string) : t list =
  let seen = Hashtbl.create 4096 in
  List.filter_map (read_all ~dir) ~f:(fun ((point : t), skipped) ->
    if skipped
    then None
    else (
      match Hashtbl.find_opt seen point.id with
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

(* Reasons from the skip file: one id per line, optionally followed by a tab
   and the reason. *)
let reasons (path : string option) : (int, string) Hashtbl.t =
  let table = Hashtbl.create 64 in
  (match path with
   | Some path when Sys.file_exists path ->
     List.iter (Files.lines path) ~f:(fun line ->
       if String.length line > 0 && line.[0] <> '#'
       then (
         let id, reason =
           match String.index_opt line '\t' with
           | Some i ->
             ( String.sub line ~pos:0 ~len:i
             , String.sub line ~pos:(i + 1) ~len:(String.length line - i - 1) )
           | None -> line, ""
         in
         match int_of_string_opt (String.trim id) with
         | Some id -> Hashtbl.replace table id (String.trim reason)
         | None -> ()))
   | Some _ | None -> ());
  table
;;

let skips ~(dir : string) ~(reasons_file : string option) : skip list =
  let reasons = reasons reasons_file in
  List.filter_map (read_all ~dir) ~f:(fun ((point : t), skipped) ->
    if not skipped
    then None
    else (
      let reason =
        match Hashtbl.find_opt reasons point.id with
        | Some reason when String.length reason > 0 -> reason
        | Some _ | None -> "no reason given"
      in
      Some { point; reason }))
;;

let results (path : string) : (t * string * string * string) list =
  List.filter_map (Files.lines path) ~f:(fun line ->
    match fields line with
    | [ id; operator; file; line_no; edit; verdict; target; part ] ->
      (match int_of_string_opt id, int_of_string_opt line_no with
       | Some id, Some line_no ->
         Some
           ( { id; operator; file; line = line_no; column = 0; edit; library = "" }
           , verdict
           , target
           , part )
       | _, _ -> None)
    | _ -> None)
;;
