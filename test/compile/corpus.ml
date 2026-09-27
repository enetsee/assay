(* Test input for test/compile. Unlike test/corpus, this is rewritten and then
   actually compiled and run, so every mutant here has to type-check. *)

module Kind = struct
  module Set = Set.Make (Int)
end

module Name = struct
  module Map = Map.Make (String)
end

let bounded (x : int) (lo : int) (hi : int) : bool = x >= lo && x <= hi

(* The left operand's location starts at the paren, same as the whole
   expression's. Both connectors must still get different ids. *)
let nested (a : bool) (b : bool) (c : bool) : bool = (a && b) && c

(* Long enough that copying operands into both branches would produce an
   enormous file. *)
let all_set (a : bool array) : bool =
  a.(0)
  && a.(1)
  && a.(2)
  && a.(3)
  && a.(4)
  && a.(5)
  && a.(6)
  && a.(7)
  && a.(8)
  && a.(9)
  && a.(10)
  && a.(11)
  && a.(12)
  && a.(13)
  && a.(14)
  && a.(15)
  && a.(16)
  && a.(17)
  && a.(18)
  && a.(19)
;;

let widen (a : Kind.Set.t) (b : Kind.Set.t) : Kind.Set.t =
  Kind.Set.union a b |> Kind.Set.add 3
;;

let index (k : string) (v : int) (m : int Name.Map.t) : int Name.Map.t =
  Name.Map.add k v m
;;

let names : string list = [ "one"; "two"; "three" ]

let () =
  assert (bounded 2 1 3);
  assert (not (nested true false true));
  assert (all_set (Array.make 20 true));
  assert (Kind.Set.cardinal (widen (Kind.Set.singleton 1) (Kind.Set.singleton 2)) = 3);
  assert (Name.Map.find "k" (index "k" 1 Name.Map.empty) = 1);
  assert (List.length names = 3)
;;
