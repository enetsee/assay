open StdLabels

let header = "#unit"

let block
      ~(unit_name : string)
      ~(live : Point.t list)
      ~(skipped : (Point.t * string option) list)
  : string
  =
  let buf = Buffer.create 1024 in
  Buffer.add_string
    buf
    (Printf.sprintf "%s\t%s\t%d\n" header unit_name (List.length live));
  (* Tabs and newlines in a binding (a printed pattern) or an attribute's
     reason would break the format. *)
  let clean =
    String.map ~f:(fun c ->
      match c with
      | '\t' | '\n' | '\r' -> ' '
      | c -> c)
  in
  let add (suffix : string) (p : Point.t) : unit =
    Buffer.add_string
      buf
      (Printf.sprintf
         "%d\t%s\t%s\t%d\t%d\t%s\t%s%s\n"
         p.id
         (Operator.to_string p.operator)
         p.file
         p.line
         p.column
         (clean p.binding)
         p.edit
         suffix)
  in
  List.iter live ~f:(add "");
  List.iter skipped ~f:(fun (p, reason) ->
    match reason with
    | None -> add "\tskipped" p
    | Some reason -> add ("\tskipped\t" ^ clean reason) p);
  Buffer.contents buf
;;

(* The file contents minus this unit's block (from its header up to the next
   header). *)
let without ~(unit_name : string) (contents : string) : string =
  let mine = Printf.sprintf "%s\t%s\t" header unit_name in
  let buf = Buffer.create (String.length contents) in
  let dropping = ref false in
  List.iter (String.split_on_char ~sep:'\n' contents) ~f:(fun line ->
    if String.length line > 0
    then (
      let starts_block =
        String.length line >= String.length header
        && String.equal (String.sub line ~pos:0 ~len:(String.length header)) header
      in
      if starts_block
      then
        dropping
        := String.length line >= String.length mine
           && String.equal (String.sub line ~pos:0 ~len:(String.length mine)) mine;
      if not !dropping
      then (
        Buffer.add_string buf line;
        Buffer.add_char buf '\n')));
  Buffer.contents buf
;;

let read_all (fd : Unix.file_descr) : string =
  let buf = Buffer.create 4096 in
  let chunk = Bytes.create 65536 in
  let rec loop () : unit =
    let n = Unix.read fd chunk 0 (Bytes.length chunk) in
    if n > 0
    then (
      Buffer.add_subbytes buf chunk 0 n;
      loop ())
  in
  loop ();
  Buffer.contents buf
;;

let write_all (fd : Unix.file_descr) (s : string) : unit =
  let bytes = Bytes.of_string s in
  let rec loop (pos : int) : unit =
    if pos < Bytes.length bytes
    then loop (pos + Unix.write fd bytes pos (Bytes.length bytes - pos))
  in
  loop 0
;;

let rec mkdir_p (dir : string) : unit =
  if not (Sys.file_exists dir)
  then (
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with
    | Unix.Unix_error (EEXIST, _, _) -> ())
;;

let write
      ~(dir : string)
      ~(library : string)
      ~(unit_name : string)
      ~(live : Point.t list)
      ~(skipped : (Point.t * string option) list)
  : unit
  =
  mkdir_p dir;
  let path = Filename.concat dir (library ^ ".muts") in
  let fd = Unix.openfile path [ O_RDWR; O_CREAT ] 0o644 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
       Unix.lockf fd F_LOCK 0;
       let kept = without ~unit_name (read_all fd) in
       ignore (Unix.lseek fd 0 SEEK_SET : int);
       Unix.ftruncate fd 0;
       write_all fd (kept ^ block ~unit_name ~live ~skipped))
;;
