open StdLabels

type row =
  { operator : string
  ; generated : int
  ; killed : int
  ; survived : int
  ; hung : int
  ; errored : int
  ; skipped : int
  }

let operators (points : Points.t list) (skips : Points.skip list) : string list =
  let extra =
    List.filter
      (List.sort_uniq
         ~cmp:String.compare
         (List.map points ~f:(fun (p : Points.t) -> p.operator)
          @ List.map skips ~f:(fun (s : Points.skip) -> s.point.operator)))
      ~f:(fun name -> not (List.mem name ~set:Summary.operator_order))
  in
  Summary.operator_order @ extra
;;

let rows
      ~(points : Points.t list)
      ~(skips : Points.skip list)
      ~(outcomes : (Points.t * Run.outcome) list)
  : row list
  =
  List.map (operators points skips) ~f:(fun operator ->
    let mine =
      List.filter outcomes ~f:(fun ((p : Points.t), _) ->
        String.equal p.operator operator)
    in
    let skipped =
      List.length
        (List.filter skips ~f:(fun (s : Points.skip) ->
           String.equal s.point.operator operator))
    in
    let generated =
      List.length
        (List.filter points ~f:(fun (p : Points.t) -> String.equal p.operator operator))
      + skipped
    in
    let count f = Summary.count mine ~f in
    let killed = count Summary.is_killed in
    let survived = count Summary.is_survived in
    let hung = count Summary.is_hung in
    let errored = count Summary.is_errored in
    { operator; generated; killed; survived; hung; errored; skipped })
;;

let print
      ~(points : Points.t list)
      ~(skips : Points.skip list)
      ~(outcomes : (Points.t * Run.outcome) list)
  : unit
  =
  let rows = rows ~points ~skips ~outcomes in
  Printf.printf
    "%-10s %10s %8s %9s %6s %6s %8s\n"
    "operator"
    "generated"
    "killed"
    "survived"
    "hung"
    "error"
    "skipped";
  List.iter rows ~f:(fun row ->
    Printf.printf
      "%-10s %10d %8d %9d %6d %6d %8d\n"
      row.operator
      row.generated
      row.killed
      row.survived
      row.hung
      row.errored
      row.skipped);
  let total f = List.fold_left rows ~init:0 ~f:(fun acc row -> acc + f row) in
  let generated = total (fun r -> r.generated) in
  Printf.printf
    "%-10s %10d %8d %9d %6d %6d %8d\n"
    "total"
    generated
    (total (fun r -> r.killed))
    (total (fun r -> r.survived))
    (total (fun r -> r.hung))
    (total (fun r -> r.errored))
    (total (fun r -> r.skipped));
  let accounted =
    total (fun r -> r.killed)
    + total (fun r -> r.survived)
    + total (fun r -> r.hung)
    + total (fun r -> r.errored)
    + total (fun r -> r.skipped)
  in
  if accounted <> generated
  then
    Printf.printf
      "\nwarning: totals don't add up (%d generated, %d accounted for)\n"
      generated
      accounted;
  let reasons = Summary.tally (List.map skips ~f:(fun (s : Points.skip) -> s.reason)) in
  if reasons <> []
  then (
    Printf.printf "\nskipped, by reason\n";
    List.iter reasons ~f:(fun (reason, n) -> Printf.printf "  %5d  %s\n" n reason));
  let killers =
    Summary.tally
      (List.filter_map outcomes ~f:(fun (_, outcome) ->
         match outcome with
         | Run.Killed { target; part } -> Some (target ^ " " ^ part)
         | Run.Survived | Run.Hung _ | Run.Errored _ -> None))
  in
  if killers <> []
  then (
    Printf.printf "\nkilled by\n";
    List.iter killers ~f:(fun (who, n) -> Printf.printf "  %5d  %s\n" n who));
  let hangs =
    List.filter_map outcomes ~f:(fun ((p : Points.t), outcome) ->
      match outcome with
      | Run.Hung target -> Some (p, target)
      | Run.Killed _ | Run.Survived | Run.Errored _ -> None)
  in
  if hangs <> []
  then (
    Printf.printf "\ntimed out\n";
    List.iter hangs ~f:(fun ((p : Points.t), target) ->
      Printf.printf "  %-34s %s:%d  under %s\n" p.edit p.file p.line target));
  let errors =
    List.filter_map outcomes ~f:(fun ((p : Points.t), outcome) ->
      match outcome with
      | Run.Errored message -> Some (p, message)
      | Run.Killed _ | Run.Survived | Run.Hung _ -> None)
  in
  if errors <> []
  then (
    Printf.printf "\ncould not be run (outcome unknown)\n";
    List.iter errors ~f:(fun ((p : Points.t), message) ->
      Printf.printf "  %-34s %s:%d  %s\n" p.edit p.file p.line message));
  let survivors =
    List.filter_map outcomes ~f:(fun ((p : Points.t), outcome) ->
      match outcome with
      | Run.Survived -> Some p
      | Run.Killed _ | Run.Hung _ | Run.Errored _ -> None)
  in
  Printf.printf "\nsurvived\n";
  if survivors = []
  then Printf.printf "  none\n"
  else
    List.iter
      (List.sort
         ~cmp:(fun (a : Points.t) (b : Points.t) ->
           if String.equal a.file b.file
           then compare a.line b.line
           else String.compare a.file b.file)
         survivors)
      ~f:(fun (p : Points.t) -> Printf.printf "  %-34s %s:%d\n" p.edit p.file p.line)
;;

let write_results
      ~(path : string)
      ~(skips : Points.skip list)
      (outcomes : (Points.t * Run.outcome) list)
  : unit
  =
  let oc = open_out path in
  (* Tabs or newlines inside a field would break the format. *)
  let clean =
    String.map ~f:(fun c ->
      match c with
      | '\t' | '\n' | '\r' -> ' '
      | c -> c)
  in
  let line (p : Points.t) (verdict : string) (target : string) (part : string) : unit =
    let p = { p with edit = clean p.edit; file = clean p.file } in
    let target = clean target
    and part = clean part in
    Printf.fprintf
      oc
      "%d\t%s\t%s\t%d\t%s\t%s\t%s\t%s\n"
      p.id
      p.operator
      p.file
      p.line
      p.edit
      verdict
      target
      part
  in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
       List.iter outcomes ~f:(fun ((p : Points.t), outcome) ->
         match outcome with
         | Run.Killed { target; part } -> line p "killed" target part
         | Run.Survived -> line p "survived" "" ""
         | Run.Hung target -> line p "hung" target ""
         | Run.Errored message -> line p "error" "" message);
       (* Skipped points carry their library and reason, so [-from] can report
          them too. *)
       List.iter skips ~f:(fun (s : Points.skip) ->
         line s.point "skipped" s.point.library s.reason))
;;
