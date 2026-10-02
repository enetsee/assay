open StdLabels

let rec flatten (path : Ppxlib.longident) : string list =
  match path with
  | Lident name -> [ name ]
  | Ldot (prefix, name) -> flatten prefix @ [ name ]
  | Lapply _ -> []
;;

let callee (f : Ppxlib.expression) : string list =
  match f.pexp_desc with
  | Pexp_ident { txt; _ } -> flatten txt
  | _ -> []
;;

let config_lines ~(flag : string option) (var : string) : string list =
  match
    match flag with
    | Some path -> Some path
    | None -> Sys.getenv_opt var
  with
  | Some path when Sys.file_exists path ->
    In_channel.with_open_text path In_channel.input_lines
    |> List.filter ~f:(fun line ->
      let line = String.trim line in
      String.length line > 0 && line.[0] <> '#')
  | Some _ | None -> []
;;
