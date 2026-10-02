(* Prints the errors Derive finds in a build's output, and the reason each
   would get in the skip list. *)
let () =
  let output = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  List.iter
    (fun (e : Assay_runner.Derive.error) ->
       Printf.printf
         "%s:%d:%d\n  %s\n"
         e.file
         e.line
         e.column
         (Assay_runner.Derive.reason e))
    (Assay_runner.Derive.errors output)
;;
