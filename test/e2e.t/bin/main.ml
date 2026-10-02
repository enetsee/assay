let double (x : int) : int = x * 2

let () =
  if Calc.clamp 5 0 3 <> 3 || double 2 <> 4
  then (
    print_endline "FAIL (main) wrong answer";
    exit 1)

(* Dropping [List.map string_of_int] doesn't type-check, so assay finds it
   and skips it. *)
let shown (xs : int list) : string list = xs |> List.rev |> List.map string_of_int

let () =
  if shown [ 1; 2 ] <> [ "2"; "1" ]
  then (
    print_endline "FAIL (main) wrong order";
    exit 1)
