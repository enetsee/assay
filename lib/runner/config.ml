open StdLabels

type target =
  { name : string
  ; command : string
  }

type t =
  { build : string option
  ; muts : string
  ; skip : string option
  ; arid : string option
  ; timeout : float
  ; fail_prefix : string
  ; part_field : int
  ; targets : target list
  ; covers : (string * string list) list
  }

(* Splits a line into its first word and the rest. The rest is only trimmed
   at the ends, since it may be a shell command. *)
let split_directive (line : string) : (string * string) option =
  let trimmed = String.trim line in
  if String.length trimmed = 0 || trimmed.[0] = '#'
  then None
  else (
    match String.index_opt trimmed ' ' with
    | None -> Some (trimmed, "")
    | Some i ->
      Some
        ( String.sub trimmed ~pos:0 ~len:i
        , String.trim (String.sub trimmed ~pos:i ~len:(String.length trimmed - i)) ))
;;

let load (path : string) : t =
  let build = ref None
  and muts = ref None
  and skip = ref None
  and arid = ref None
  and timeout = ref 30.
  and fail_prefix = ref "FAIL"
  and part_field = ref 2
  and targets = ref []
  and covers = ref [] in
  List.iteri (Files.lines path) ~f:(fun i line ->
    let where () = Printf.sprintf "%s line %d" path (i + 1) in
    match split_directive line with
    | None -> ()
    | Some (key, rest) ->
      (match key with
       | "build" -> build := Some rest
       | "muts" -> muts := Some rest
       | "skip" -> skip := Some rest
       | "arid" -> arid := Some rest
       | "timeout" ->
         (match float_of_string_opt rest with
          | Some seconds when seconds > 0. -> timeout := seconds
          | Some _ | None -> failwith (where () ^ ": timeout wants a positive number"))
       | "fail-prefix" -> fail_prefix := rest
       | "part-field" ->
         (match int_of_string_opt rest with
          | Some field when field >= 1 -> part_field := field
          | Some _ | None -> failwith (where () ^ ": part-field wants a field number"))
       | "covers" ->
         (match String.index_opt rest ' ' with
          | None -> failwith (where () ^ ": covers wants a target and libraries")
          | Some j ->
            let name = String.sub rest ~pos:0 ~len:j in
            (* Libraries can be separated by commas, spaces or both. *)
            let libraries =
              String.sub rest ~pos:j ~len:(String.length rest - j)
              |> String.map ~f:(fun c ->
                match c with
                | ',' | '\t' -> ' '
                | c -> c)
              |> String.split_on_char ~sep:' '
              |> List.filter ~f:(fun library -> not (String.equal library ""))
            in
            covers := (name, libraries) :: !covers)
       | "target" ->
         (match String.index_opt rest ' ' with
          | None -> failwith (where () ^ ": target wants a name and a command")
          | Some j ->
            let name = String.sub rest ~pos:0 ~len:j in
            let command =
              String.trim (String.sub rest ~pos:j ~len:(String.length rest - j))
            in
            if String.length command = 0
            then failwith (where () ^ ": target wants a command");
            targets := { name; command } :: !targets)
       | other -> failwith (Printf.sprintf "%s: unknown directive %S" (where ()) other)));
  let muts =
    match !muts with
    | Some dir -> dir
    | None -> failwith (path ^ ": no muts directory")
  in
  let targets = List.rev !targets in
  if List.length targets = 0 then failwith (path ^ ": no targets");
  { build = !build
  ; muts
  ; skip = !skip
  ; arid = !arid
  ; timeout = !timeout
  ; fail_prefix = !fail_prefix
  ; part_field = !part_field
  ; targets
  ; covers = List.rev !covers
  }
;;
