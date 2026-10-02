open StdLabels

let file : string option ref = ref None

let configured () : string list =
  List.map (Common.config_lines ~flag:!file "ASSAY_ARID") ~f:String.trim
;;

(* Matches either the full path or a suffix of it, so [Format.printf] in the
   list also matches [Stdlib.Format.printf]. *)
let named (names : string list) (path : string list) : bool =
  if path = []
  then false
  else (
    let whole = String.concat ~sep:"." path in
    List.exists names ~f:(fun name ->
      String.equal name whole
      || (String.length whole > String.length name
          && String.equal
               (String.sub
                  whole
                  ~pos:(String.length whole - String.length name)
                  ~len:(String.length name))
               name
          && whole.[String.length whole - String.length name - 1] = '.')))
;;

(* Direct child expressions. Anything not listed here is treated as a leaf. *)
let children (e : Ppxlib.expression) : Ppxlib.expression list =
  match e.pexp_desc with
  | Pexp_apply (f, args) -> f :: List.map args ~f:snd
  | Pexp_sequence (a, b) -> [ a; b ]
  | Pexp_ifthenelse (c, t, Some else_) -> [ c; t; else_ ]
  | Pexp_ifthenelse (c, t, None) -> [ c; t ]
  | Pexp_let (_, bindings, body) ->
    body :: List.map bindings ~f:(fun (vb : Ppxlib.value_binding) -> vb.pvb_expr)
  (* Just the arms, not the scrutinee. Otherwise a pretty-printer that matches
     on [p] and calls [Format.fprintf] in every arm would never count as arid,
     because [p] is a leaf. *)
  | Pexp_match (_, cases) -> List.map cases ~f:(fun (c : Ppxlib.case) -> c.pc_rhs)
  (* Unlike a match scrutinee, the body of a [try] is the real work, so it
     counts along with the handlers. *)
  | Pexp_try (body, cases) ->
    body :: List.map cases ~f:(fun (c : Ppxlib.case) -> c.pc_rhs)
  | Pexp_tuple items | Pexp_array items -> items
  | Pexp_construct (_, Some inner)
  | Pexp_constraint (inner, _)
  | Pexp_field (inner, _)
  | Pexp_lazy inner
  | Pexp_assert inner
  | Pexp_open (_, inner) -> [ inner ]
  | _ -> []
;;

let rec is_arid ~(names : string list) (e : Ppxlib.expression) : bool =
  match e.pexp_desc with
  (* A call to a listed function, including all its arguments (e.g. the
     values passed to a printf). *)
  | Pexp_apply (f, _) when named names (Common.callee f) -> true
  (* Mutating inside an assert either trips the assert or does nothing, so
     there's nothing to learn. *)
  | Pexp_assert _ -> true
  | _ ->
    (match children e with
     | [] -> false
     | children -> List.for_all children ~f:(is_arid ~names))
;;
