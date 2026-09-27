let double (x : int) : int = x * 2

let () =
  if Calc.clamp 5 0 3 <> 3 || double 2 <> 4
  then (
    print_endline "FAIL (main) wrong answer";
    exit 1)
