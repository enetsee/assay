open StdLabels

type t =
  | Atom of string
  | List of t list

let many (source : string) : t list =
  let n = String.length source in
  let pos = ref 0 in
  let fail : type a. string -> a =
    fun what -> failwith (Printf.sprintf "sexp: %s at %d" what !pos)
  in
  let rec skip_space () : unit =
    if !pos < n
    then (
      match source.[!pos] with
      | ' ' | '\t' | '\n' | '\r' ->
        incr pos;
        skip_space ()
      | _ -> ())
  in
  let quoted () : string =
    let buf = Buffer.create 32 in
    incr pos;
    let rec go () : unit =
      if !pos >= n
      then fail "unterminated string"
      else (
        match source.[!pos] with
        | '"' -> incr pos
        | '\\' ->
          if !pos + 1 >= n then fail "unterminated escape";
          (match source.[!pos + 1] with
           | 'n' -> Buffer.add_char buf '\n'
           | 't' -> Buffer.add_char buf '\t'
           | 'r' -> Buffer.add_char buf '\r'
           | c -> Buffer.add_char buf c);
          pos := !pos + 2;
          go ()
        | c ->
          Buffer.add_char buf c;
          incr pos;
          go ())
    in
    go ();
    Buffer.contents buf
  in
  let bare () : string =
    let start = !pos in
    let rec go () : unit =
      if !pos < n
      then (
        match source.[!pos] with
        | ' ' | '\t' | '\n' | '\r' | '(' | ')' -> ()
        | _ ->
          incr pos;
          go ())
    in
    go ();
    if !pos = start then fail "empty atom";
    String.sub source ~pos:start ~len:(!pos - start)
  in
  let rec form () : t =
    skip_space ();
    if !pos >= n
    then fail "end of input"
    else (
      match source.[!pos] with
      | '(' ->
        incr pos;
        let items = ref [] in
        let rec go () : unit =
          skip_space ();
          if !pos >= n
          then fail "unterminated list"
          else if source.[!pos] = ')'
          then incr pos
          else (
            items := form () :: !items;
            go ())
        in
        go ();
        List (List.rev !items)
      | ')' -> fail "unexpected )"
      | '"' -> Atom (quoted ())
      | _ -> Atom (bare ()))
  in
  let forms = ref [] in
  skip_space ();
  while !pos < n do
    forms := form () :: !forms;
    skip_space ()
  done;
  List.rev !forms
;;

let of_string (source : string) : t =
  match many source with
  | [ form ] -> form
  | [] -> failwith "sexp: empty input"
  | _ :: _ :: _ -> failwith "sexp: expected one form, got several"
;;

let field (name : string) (form : t) : t list option =
  match form with
  | Atom _ -> None
  | List items ->
    List.find_map items ~f:(fun item ->
      match item with
      | List (Atom key :: rest) when String.equal key name -> Some rest
      | Atom _ | List _ -> None)
;;

let atom_field (name : string) (form : t) : string option =
  match field name form with
  | Some [ Atom value ] -> Some value
  | Some _ | None -> None
;;
