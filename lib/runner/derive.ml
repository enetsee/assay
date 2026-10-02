open StdLabels

type error =
  { file : string
  ; line : int
  ; column : int
  ; message : string
  }

let starts_with ~(prefix : string) (s : string) : bool =
  String.length s >= String.length prefix
  && String.equal (String.sub s ~pos:0 ~len:(String.length prefix)) prefix
;;

(* [File "lib/x.ml", line 12, characters 4-9:] or [..., lines 12-15,
   characters 4-9:]. *)
let location (line : string) : (string * int * int) option =
  let single () =
    Scanf.sscanf line "File %S, line %d, characters %d-%_d:" (fun f l c -> f, l, c)
  in
  let multi () =
    Scanf.sscanf line "File %S, lines %d-%_d, characters %d-%_d:" (fun f l c -> f, l, c)
  in
  match single () with
  | found -> Some found
  | exception (Scanf.Scan_failure _ | Failure _ | End_of_file) ->
    (match multi () with
     | found -> Some found
     | exception (Scanf.Scan_failure _ | Failure _ | End_of_file) -> None)
;;

(* A location is followed by the source excerpt and then the message. It's
   an error if the message starts with [Error], which includes warnings
   turned into errors ([Error (warning 26 ...)]). *)
let errors (output : string) : error list =
  let rec go (lines : string list) (found : error list) : error list =
    match lines with
    | [] -> List.rev found
    | line :: rest ->
      (match location line with
       | None -> go rest found
       | Some (file, line, column) ->
         let rec message (lines : string list) : string option * string list =
           match lines with
           | [] -> None, []
           | l :: rest when Option.is_some (location l) -> None, l :: rest
           | l :: rest when starts_with ~prefix:"Error" l ->
             (* The compiler wraps a long message onto indented lines, e.g. a
                type too long for the first one. Join them up. *)
             let rec more (lines : string list) (acc : string list) =
               match lines with
               | l :: rest
                 when String.length l > 0
                      && (l.[0] = ' ' || l.[0] = '\t')
                      && String.length (String.trim l) > 0 ->
                 more rest (String.trim l :: acc)
               | lines -> String.concat ~sep:" " (List.rev acc), lines
             in
             let message, rest = more rest [ String.trim l ] in
             Some message, rest
           | l :: rest when starts_with ~prefix:"Warning" l -> None, rest
           | _ :: rest -> message rest
         in
         (match message rest with
          | Some message, rest -> go rest ({ file; line; column; message } :: found)
          | None, rest -> go rest found))
  in
  go (String.split_on_char ~sep:'\n' output) []
;;

let reason_prefix = "doesn't type-check:"

(* The index of [sub] in [s], if it's there. *)
let find (s : string) (sub : string) : int option =
  let n = String.length s
  and k = String.length sub in
  let rec go (i : int) : int option =
    if i + k > n
    then None
    else if String.equal (String.sub s ~pos:i ~len:k) sub
    then Some i
    else go (i + 1)
  in
  go 0
;;

let reason (error : error) : string =
  let message =
    if starts_with ~prefix:"Error: " error.message
    then String.sub error.message ~pos:7 ~len:(String.length error.message - 7)
    else error.message
  in
  (* The expected type is the one the stage takes, which the first half
     already shows; what matters is what the stage turned it into. *)
  let message =
    match find message " but an expression was expected" with
    | Some i -> String.sub message ~pos:0 ~len:i
    | None -> message
  in
  (* No position: it would go stale with the next edit above the point. *)
  Printf.sprintf "%s %s" reason_prefix message
;;

let derived (reason : string) : bool = starts_with ~prefix:reason_prefix reason

let clean : string -> string =
  String.map ~f:(fun c ->
    match c with
    | '\t' | '\n' | '\r' -> ' '
    | c -> c)
;;

let add ~(path : string) (entries : (int * string) list) : unit =
  (* Don't glue the first entry onto a last line with no newline. *)
  let needs_newline =
    Sys.file_exists path
    &&
    let contents = Files.read path in
    String.length contents > 0 && contents.[String.length contents - 1] <> '\n'
  in
  let oc = open_out_gen [ Open_append; Open_creat; Open_text ] 0o644 path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
       if needs_newline then output_char oc '\n';
       List.iter entries ~f:(fun (id, reason) ->
         Printf.fprintf oc "%d\t%s\n" id (clean reason)))
;;

let remove ~(path : string) (ids : int list) : unit =
  let id_of (line : string) : int option =
    let field =
      match String.index_opt line '\t' with
      | Some i -> String.sub line ~pos:0 ~len:i
      | None -> line
    in
    int_of_string_opt (String.trim field)
  in
  let kept =
    List.filter (Files.lines path) ~f:(fun line ->
      match id_of line with
      | Some id -> not (List.mem id ~set:ids)
      | None -> true)
  in
  let temp = path ^ ".tmp" in
  let oc = open_out temp in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
       List.iter kept ~f:(fun line ->
         output_string oc line;
         output_char oc '\n');
       close_out oc);
  Sys.rename temp path
;;
