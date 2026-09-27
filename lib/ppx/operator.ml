type t =
  | Extreme
  | Sbr
  | Ror
  | Lcr
  | Aor
  | Uoi
  | Empty

let to_string (t : t) : string =
  match t with
  | Extreme -> "extreme"
  | Sbr -> "sbr"
  | Ror -> "ror"
  | Lcr -> "lcr"
  | Aor -> "aor"
  | Uoi -> "uoi"
  | Empty -> "empty"
;;
