open StdLabels

let operator_order = [ "extreme"; "sbr"; "ror"; "lcr"; "aor"; "uoi"; "empty" ]

let rank (operator : string) : int =
  let rec go (i : int) (names : string list) : int =
    match names with
    | [] -> i
    | name :: rest -> if String.equal name operator then i else go (i + 1) rest
  in
  go 0 operator_order
;;

let tally (keys : string list) : (string * int) list =
  let table = Hashtbl.create 16 in
  List.iter keys ~f:(fun key ->
    Hashtbl.replace table key (1 + Option.value (Hashtbl.find_opt table key) ~default:0));
  List.sort
    ~cmp:(fun (a, x) (b, y) -> if x = y then String.compare a b else compare y x)
    (Hashtbl.fold (fun key n acc -> (key, n) :: acc) table [])
;;

let is_killed (o : Run.outcome) : bool =
  match o with
  | Killed _ -> true
  | Survived | Hung _ | Errored _ -> false
;;

let is_survived (o : Run.outcome) : bool =
  match o with
  | Survived -> true
  | Killed _ | Hung _ | Errored _ -> false
;;

let is_hung (o : Run.outcome) : bool =
  match o with
  | Hung _ -> true
  | Killed _ | Survived | Errored _ -> false
;;

let is_errored (o : Run.outcome) : bool =
  match o with
  | Errored _ -> true
  | Killed _ | Survived | Hung _ -> false
;;

let count (rows : ('a * Run.outcome) list) ~(f : Run.outcome -> bool) : int =
  List.length (List.filter rows ~f:(fun (_, outcome) -> f outcome))
;;
