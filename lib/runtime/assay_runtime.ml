(* Read once at startup, since [active] can be called a lot (e.g. in loops). *)
let selected : int option =
  match Sys.getenv_opt "ASSAY_MUTANT" with
  | None -> None
  | Some s -> int_of_string_opt (String.trim s)
;;

let active (id : int) : bool =
  match selected with
  | None -> false
  | Some m -> m = id
;;
