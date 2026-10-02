open StdLabels

(* Treats [Log.info] as arid, so the corpus can test the arid rules.

   Prints the points found in a file, live then skipped (with the reason from
   an [assay.skip] attribute).

   With [-ids], prints each point's id and edit instead, for checking that ids
   don't depend on position. Otherwise ids aren't printed, so the expected
   output doesn't change when the hash does. [-shift] parses the file with
   lines and a new binding added at the top, as if edited above every point,
   and keeps its name, which is part of the id. *)
let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let ids = List.mem "-ids" ~set:args in
  let shift = List.mem "-shift" ~set:args in
  let path = List.find args ~f:(fun a -> String.length a > 0 && a.[0] <> '-') in
  let source =
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let source =
    if shift
    then "(* Added above everything. *)\n\nlet unrelated = 1 + 2\n\n" ^ source
    else source
  in
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf path;
  let mutated =
    Assay_ppx.Mutate.run
      ~skip:(fun _ -> false)
      ~arid:(Assay_ppx.Arid.is_arid ~names:[ "Log.info" ])
      (Ppxlib.Parse.implementation lexbuf)
  in
  let show (suffix : string) (p : Assay_ppx.Point.t) : unit =
    if ids
    then Printf.printf "%d %s%s\n" p.id p.edit suffix
    else
      Printf.printf
        "%-8s %3d:%-3d %s%s\n"
        (Assay_ppx.Operator.to_string p.operator)
        p.line
        p.column
        p.edit
        suffix
  in
  List.iter mutated.live ~f:(fun (p : Assay_ppx.Point.t) ->
    if not (shift && p.line <= 4) then show "" p);
  List.iter mutated.skipped ~f:(fun (p, reason) ->
    show
      (match reason with
       | Some reason -> Printf.sprintf " (skipped: %s)" reason
       | None -> " (skipped)")
      p)
;;
