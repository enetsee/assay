open StdLabels

type t =
  { id : int
  ; operator : Operator.t
  ; file : string
  ; line : int
  ; column : int
  ; edit : string
  ; binding : string
  }

(* FNV-1a. Not using [Hashtbl.hash] because it isn't guaranteed to be stable
   across compiler versions, and ids should stay the same between runs. *)
let hash (s : string) : int =
  let h = ref 0x2545f4914f6cdd1d in
  String.iter s ~f:(fun c -> h := !h lxor Char.code c * 0x100000001b3 land max_int);
  !h
;;

let make
      ~(operator : Operator.t)
      ~(loc : Ppxlib.Location.t)
      ~(edit : string)
      ~(binding : string)
      ~(key : string)
  : t
  =
  let file = loc.loc_start.pos_fname in
  let line = loc.loc_start.pos_lnum in
  let column = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let id =
    hash (Printf.sprintf "%s:%s:%s:%s" file key (Operator.to_string operator) edit)
  in
  { id; operator; file; line; column; edit; binding }
;;
