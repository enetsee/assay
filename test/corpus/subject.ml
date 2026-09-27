(* Test input for test/dump_points.ml, with an example of each mutation
   operator. Only parsed, never compiled, so it doesn't need to type-check.
   Line numbers matter: editing above a case changes points.expected. *)

let bounded (x : int) (lo : int) (hi : int) : bool = x >= lo && x <= hi
let scale (x : float) (k : float) : float = (x *. k) +. 1.0
let pick (flag : bool) (a : int) (b : int) : int = if flag then a else b
let names : string list = [ "one"; "two"; "three" ]

let shout (xs : string list) : unit =
  List.iter (fun x -> print_string x) xs;
  print_newline ()
;;

let warn (xs : string list) : unit = if xs <> [] then print_string "some"

let widen (a : Kind.Set.t) (b : Kind.Set.t) : Kind.Set.t =
  Kind.Set.union a b |> Kind.Set.add Kind.dummy
;;

let index (k : string) (v : int) (m : int Name.Map.t) : int Name.Map.t = Name.Map.add k v m
let unannotated x = x + 1

(* A unit [if] as the first statement: one sbr point, not two. *)
let log (b : bool) : unit =
  if b then print_string "x";
  print_newline ()
;;

(* The body of a [try] still counts when the handler is arid. *)
let guarded (x : int) : int = try x + 1 with _ -> Log.info "failed"

(* Inline tests, quotations and attribute payloads are left alone. *)
let%expect_test "t" = print_int (1 + 2)
let quoted = [%expr a + b]
let annotated (x : int) : int = (x + 1 [@default 1 + 2])

(* Pipe stages that change the type get no point; [|> Fun.id]-able ones do. *)
let pipes (xs : int list) : unit =
  xs |> List.length |> string_of_int |> print_endline;
  xs |> List.rev |> ignore
;;
