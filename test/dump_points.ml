open StdLabels

(* Treats [Log.info] as arid, so the corpus can test the arid rules.

   Prints the points found in a file.

   Ids aren't printed since they're a hash of the position, so any edit to
   the corpus would change them all. *)
let () =
  let path = Sys.argv.(1) in
  let source =
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf path;
  let mutated =
    Assay_ppx.Mutate.run
      ~skip:(fun _ -> false)
      ~arid:(Assay_ppx.Arid.is_arid ~names:[ "Log.info" ])
      (Ppxlib.Parse.implementation lexbuf)
  in
  let show (points : Assay_ppx.Point.t list) : unit =
    List.iter points ~f:(fun (p : Assay_ppx.Point.t) ->
      Printf.printf
        "%-8s %3d:%-3d %s\n"
        (Assay_ppx.Operator.to_string p.operator)
        p.line
        p.column
        p.edit)
  in
  show mutated.live
;;
