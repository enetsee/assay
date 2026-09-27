open StdLabels

(* Rewrites a file with every mutation point applied and prints the result,
   so the output can be compiled. Fails if two points share an id, since the
   runner would refuse to run them. *)
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
      ~arid:(fun _ -> false)
      (Ppxlib.Parse.implementation lexbuf)
  in
  let seen = Hashtbl.create 64 in
  List.iter mutated.live ~f:(fun (p : Assay_ppx.Point.t) ->
    (match Hashtbl.find_opt seen p.id with
     | Some (other : Assay_ppx.Point.t) ->
       Printf.eprintf
         "duplicate id %d: %s at %d:%d and %s at %d:%d\n"
         p.id
         other.edit
         other.line
         other.column
         p.edit
         p.line
         p.column;
       exit 1
     | None -> ());
    Hashtbl.replace seen p.id p);
  print_string (Ppxlib.Pprintast.string_of_structure mutated.structure)
;;
