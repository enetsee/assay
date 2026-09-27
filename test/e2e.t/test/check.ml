let () =
  if Calc.clamp (-1) 0 3 <> 0
  then (
    print_endline "FAIL (low) clamp";
    exit 1);
  if List.length Calc.labels <> 2
  then (
    print_endline "FAIL (labels) count";
    exit 1)
