open StdLabels

let file : string option ref = ref None

(* Loaded on first use, since that's after the ppx flags have been read. *)
let ids : (int, unit) Hashtbl.t Lazy.t =
  lazy
    (let table = Hashtbl.create 64 in
     List.iter (Common.config_lines ~flag:!file "ASSAY_SKIP") ~f:(fun line ->
       let id =
         match String.index_opt line '\t' with
         | Some i -> String.sub line ~pos:0 ~len:i
         | None -> line
       in
       match int_of_string_opt (String.trim id) with
       | Some id -> Hashtbl.replace table id ()
       | None -> ());
     table)
;;

let skipped (id : int) : bool = Hashtbl.mem (Lazy.force ids) id
