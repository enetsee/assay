open StdLabels

(* Loaded once when the module is initialised. *)
let ids : (int, unit) Hashtbl.t =
  let table = Hashtbl.create 64 in
  List.iter (Common.config_lines "ASSAY_SKIP") ~f:(fun line ->
    let id =
      match String.index_opt line '\t' with
      | Some i -> String.sub line ~pos:0 ~len:i
      | None -> line
    in
    match int_of_string_opt (String.trim id) with
    | Some id -> Hashtbl.replace table id ()
    | None -> ());
  table
;;

let skipped (id : int) : bool = Hashtbl.mem ids id
