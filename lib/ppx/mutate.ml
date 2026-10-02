open StdLabels

type t =
  { live : Point.t list
  ; skipped : (Point.t * string option) list
  ; structure : Ppxlib.structure
  }

(* The only assay function that generated code calls. *)
let switch = "Assay_runtime.active"

(* One replacement per operator rather than every possible one (Mothra would
   generate five mutants for a single [<=]). We pick the most useful edit:
   shift the boundary for comparisons, negate equality, swap [&&]/[||], and
   invert arithmetic. Could add more later if it turns out to be worth it. *)
let replacement (op : string) : (Operator.t * string) option =
  match op with
  | "<" -> Some (Operator.Ror, "<=")
  | "<=" -> Some (Operator.Ror, "<")
  | ">" -> Some (Operator.Ror, ">=")
  | ">=" -> Some (Operator.Ror, ">")
  | "=" -> Some (Operator.Ror, "<>")
  | "<>" -> Some (Operator.Ror, "=")
  | "==" -> Some (Operator.Ror, "!=")
  | "!=" -> Some (Operator.Ror, "==")
  | "&&" -> Some (Operator.Lcr, "||")
  | "||" -> Some (Operator.Lcr, "&&")
  | "+" -> Some (Operator.Aor, "-")
  | "-" -> Some (Operator.Aor, "+")
  | "*" -> Some (Operator.Aor, "/")
  | "/" -> Some (Operator.Aor, "*")
  | "+." -> Some (Operator.Aor, "-.")
  | "-." -> Some (Operator.Aor, "+.")
  | "*." -> Some (Operator.Aor, "/.")
  | "/." -> Some (Operator.Aor, "*.")
  | _ -> None
;;

let flatten = Common.flatten
let callee = Common.callee

let unlabelled (args : (Ppxlib.arg_label * Ppxlib.expression) list) : bool =
  List.for_all args ~f:(fun ((label : Ppxlib.arg_label), _) ->
    match label with
    | Nolabel -> true
    | Labelled _ | Optional _ -> false)
;;

(* The elements of a list literal, i.e. a chain of [::] ending in [[]].
   Returns [None] if the tail is anything else, like [x :: rest]. *)
let rec elements (e : Ppxlib.expression) : Ppxlib.expression list option =
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> Some []
  | Pexp_construct
      ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple [ hd; tl ]; _ }) ->
    (match elements tl with
     | Some rest -> Some (hd :: rest)
     | None -> None)
  | _ -> None
;;

(* Which mutation applies to a node. This has to be worked out before the
   children are rewritten, because afterwards the node may look different
   (e.g. a mutated [iter] call has become an [if]). *)
type offer =
  | Nothing
  | Sequence
  | Unit_if
  | Iter
  | Pipe
  | Head of
      { operator : Operator.t
      ; edit : string
      ; replacement : string list
      }
  (** Swap the function being applied and keep the arguments. Since
          [a <= b] is [( <= ) a b], this avoids duplicating the arguments.
          [replacement] is a list of paths nested to the right, e.g.
          [Stdlib.Fun.const] or [Stdlib.Fun.const (Stdlib.Fun.id)]. *)
  | Connector of
      { edit : string
      ; dual : string
      }
  (** [&&] and [||]. We can't just swap the head like with [Head], because
          then both sides would always be evaluated (they short-circuit
          normally), even when the mutant isn't selected. Instead both
          operands are wrapped in thunks and called from each branch. *)

(* Calls like [List.iter f xs] return unit and can be dropped entirely. A bare
   [iter] is probably a local function, so we leave it alone. *)
let drops_to_unit (e : Ppxlib.expression) : bool =
  match e.pexp_desc with
  | Pexp_apply (f, args) ->
    (match List.rev (callee f), List.length args with
     | ("iter" | "iteri") :: _ :: _, 2 | "iter2" :: _ :: _, 3 -> true
     | _ -> false)
  | _ -> false
;;

(* Pipeline stages that obviously return a different type from the one they
   take, e.g. [|> ignore] or [|> List.length]. Replacing one of these with
   [Fun.id] would never type-check, so there's no point offering it. *)
let changes_type (stage : Ppxlib.expression) : bool =
  let head =
    match stage.pexp_desc with
    | Pexp_apply (f, _) -> callee f
    | _ -> callee stage
  in
  match List.rev head with
  | [] -> false
  | name :: _ ->
    List.mem
      name
      ~set:
        [ "ignore"
        ; "raise"
        ; "failwith"
        ; "invalid_arg"
        ; "iter"
        ; "iteri"
        ; "iter2"
        ; "iteri2"
        ; "length"
        ; "to_string"
        ; "print_string"
        ; "print_endline"
        ; "prerr_string"
        ; "prerr_endline"
        ]
    || String.starts_with ~prefix:"string_of_" name
;;

let is_unit_if (e : Ppxlib.expression) : bool =
  match e.pexp_desc with
  | Pexp_ifthenelse (_, _, None) -> true
  | _ -> false
;;

(* [stdlib_containers] is false in files that use Base/Core, whose [Set.add]
   and [Map.add] take their arguments in a different order (and labelled). *)
let offer_of ~(stdlib_containers : bool) (e : Ppxlib.expression) : offer =
  if drops_to_unit e
  then Iter
  else (
    match e.pexp_desc with
    (* If the first statement is an [iter] or a unit [if], its own rule
       already covers dropping it, so don't generate a duplicate. *)
    | Pexp_sequence (first, _) when not (drops_to_unit first || is_unit_if first) ->
      Sequence
    | Pexp_ifthenelse (_, _, None) -> Unit_if
    | Pexp_apply
        ({ pexp_desc = Pexp_ident { txt = Lident "|>"; _ }; _ }, [ _; (_, stage) ])
      when not (changes_type stage) -> Pipe
    | Pexp_apply (f, args) ->
      let arity = List.length args in
      (match callee f, arity, unlabelled args with
       | [ ("&&" | "||") ], 2, true ->
         let op =
           match callee f with
           | [ op ] -> op
           | _ -> "&&"
         in
         let dual = if String.equal op "&&" then "||" else "&&" in
         Connector { edit = op ^ " -> " ^ dual; dual }
       | [ op ], 2, true ->
         (match replacement op with
          | Some (operator, replaced_by) ->
            Head
              { operator
              ; edit = op ^ " -> " ^ replaced_by
              ; replacement = [ replaced_by ]
              }
          | None -> Nothing)
       | _ ->
         (* Replace a set/map operation with one of its arguments.
            [Fun.const] returns the first of two arguments, and applying it to
            [Fun.id] skips ahead one. These have the same type as the original
            function as long as it really is the stdlib Set/Map one. *)
         (match List.rev (callee f), arity with
          | _ when not (unlabelled args) -> Nothing
          | "union" :: "Set" :: _, 2 ->
            Head
              { operator = Operator.Empty
              ; edit = "Set.union a b -> a"
              ; replacement = [ "Stdlib.Fun.const" ]
              }
          | "add" :: "Set" :: _, 2 when stdlib_containers ->
            Head
              { operator = Operator.Empty
              ; edit = "Set.add x s -> s"
              ; replacement = [ "Stdlib.Fun.const"; "Stdlib.Fun.id" ]
              }
          | "add" :: "Map" :: _, 3 when stdlib_containers ->
            Head
              { operator = Operator.Empty
              ; edit = "Map.add k v m -> m"
              ; replacement = [ "Stdlib.Fun.const"; "Stdlib.Fun.const"; "Stdlib.Fun.id" ]
              }
          | _ -> Nothing))
    | _ -> Nothing)
;;

(* Nests the paths to the right, e.g. [["Stdlib.Fun.const"; "Stdlib.Fun.const";
   "Stdlib.Fun.id"]] becomes [Stdlib.Fun.const (Stdlib.Fun.const Stdlib.Fun.id)],
   which returns its third argument. *)
let path_expression ~(loc : Ppxlib.Location.t) (paths : string list) : Ppxlib.expression =
  match List.rev_map paths ~f:(Ppxlib.Ast_builder.Default.evar ~loc) with
  | [] -> Ppxlib.Ast_builder.Default.eunit ~loc
  | last :: outer ->
    List.fold_left outer ~init:last ~f:(fun acc f ->
      Ppxlib.Ast_builder.Default.eapply ~loc f [ acc ])
;;

(* A default value for a return type, used by extreme mutation. We only have
   the parsetree, so this relies on the return type being annotated. If it
   isn't, or there's no obvious default, we skip the function. *)
let rec default ~(loc : Ppxlib.Location.t) (ty : Ppxlib.core_type)
  : Ppxlib.expression option
  =
  match ty.ptyp_desc with
  | Ptyp_poly (_, inner) -> default ~loc inner
  | Ptyp_constr ({ txt; _ }, args) ->
    (match flatten txt, args with
     | [ "unit" ], [] -> Some (Ppxlib.Ast_builder.Default.eunit ~loc)
     | [ "bool" ], [] -> Some (Ppxlib.Ast_builder.Default.ebool ~loc false)
     | [ "int" ], [] -> Some (Ppxlib.Ast_builder.Default.eint ~loc 0)
     | [ "float" ], [] -> Some (Ppxlib.Ast_builder.Default.efloat ~loc "0.")
     | [ "string" ], [] -> Some (Ppxlib.Ast_builder.Default.estring ~loc "")
     | [ "list" ], [ _ ] -> Some (Ppxlib.Ast_builder.Default.elist ~loc [])
     | [ "array" ], [ _ ] -> Some (Ppxlib.Ast_builder.Default.pexp_array ~loc [])
     | [ "option" ], [ _ ] ->
       Some
         (Ppxlib.Ast_builder.Default.pexp_construct
            ~loc
            (Ppxlib.Ast_builder.Default.Located.lident ~loc "None")
            None)
     | parts, _ ->
       (* For [Foo.Set.t] or [Foo.Map.t], use [Foo.Set.empty] / [Foo.Map.empty]. *)
       (match List.rev parts with
        | "t" :: (("Set" | "Map") :: _ as container) ->
          Some
            (Ppxlib.Ast_builder.Default.evar
               ~loc
               (String.concat ~sep:"." (List.rev ("empty" :: container))))
        | _ -> None))
  | _ -> None
;;

let bound_name (p : Ppxlib.pattern) : string option =
  match p.ppat_desc with
  | Ppat_var { txt; _ } -> Some txt
  | Ppat_constraint ({ ppat_desc = Ppat_var { txt; _ }; _ }, _) -> Some txt
  | _ -> None
;;

(* [if Assay_runtime.active id then replaced_by else original]. Every mutant
   is compiled into the same build and picked at runtime. *)
let guarded ~(id : int) ~(replaced_by : Ppxlib.expression) (original : Ppxlib.expression)
  : Ppxlib.expression
  =
  let loc = original.pexp_loc in
  Ppxlib.Ast_builder.Default.pexp_ifthenelse
    ~loc
    (Ppxlib.Ast_builder.Default.eapply
       ~loc
       (Ppxlib.Ast_builder.Default.evar ~loc switch)
       [ Ppxlib.Ast_builder.Default.eint ~loc id ])
    replaced_by
    (Some original)
;;

(* Whether a file uses Base/Core, going by a top-level [open] or any path
   starting with one of them. *)
let uses_base_or_core (structure : Ppxlib.structure) : bool =
  let is_base_or_core (path : string list) : bool =
    match path with
    | ("Base" | "Core" | "Core_kernel") :: _ -> true
    | _ -> false
  in
  let found = ref false in
  let finder =
    object
      inherit Ppxlib.Ast_traverse.iter as super

      method! longident (path : Ppxlib.longident) =
        if is_base_or_core (flatten path) then found := true else super#longident path
    end
  in
  finder#structure structure;
  !found
;;

(* Extensions whose payload isn't ordinary program code: inline tests and
   benchmarks, and metaquot quotations (whose contents end up in other
   libraries' code). Anything else, like [let%bind], is walked as normal. *)
let leave_alone (name : string) : bool =
  match name with
  | "test"
  | "test_unit"
  | "test_module"
  | "expect_test"
  | "expect"
  | "bench"
  | "bench_fun"
  | "bench_module"
  | "expr"
  | "pat"
  | "str"
  | "stri"
  | "sig"
  | "sigi"
  | "type" -> true
  | name ->
    String.length name > 9 && String.equal (String.sub name ~pos:0 ~len:9) "metaquot."
;;

(* The reason given by an [[@assay.skip "reason"]] or
   [[@@assay.skip "reason"]] attribute, if there is one. *)
let skip_attribute (attributes : Ppxlib.attributes) : string option =
  List.find_map attributes ~f:(fun (a : Ppxlib.attribute) ->
    if not (String.equal a.attr_name.txt "assay.skip")
    then None
    else (
      match a.attr_payload with
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ({ pexp_desc = Pexp_constant (Pconst_string (reason, _, _)); _ }, _)
            ; _
            }
          ]
        when String.length (String.trim reason) > 0 -> Some (String.trim reason)
      | _ ->
        Ppxlib.Location.raise_errorf
          ~loc:a.attr_loc
          "assay.skip needs a reason, e.g. [@assay.skip \"the order isn't observable \
           here\"]"))
;;

(* [e] with everything below [depth] levels of expressions replaced by a
   placeholder, for printing into a point's key. Printing whole subjects
   would be quadratic in long chains like [a + b + ... + z], where each
   level's subject contains all the levels below it. Points that look alike
   at this depth are still told apart by their occurrence. *)
let truncated ~(depth : int) (e : Ppxlib.expression) : Ppxlib.expression =
  let level = ref 0 in
  let cut =
    object (self)
      inherit Ppxlib.Ast_traverse.map as super

      method! expression (e : Ppxlib.expression) =
        if !level >= depth
        then Ppxlib.Ast_builder.Default.evar ~loc:e.pexp_loc "__"
        else (
          incr level;
          Fun.protect
            ~finally:(fun () -> decr level)
            (fun () ->
               match e.pexp_desc with
               (* Never cut a constructor's argument tuple itself, only its
                  elements: Pprintast recurses forever on [x :: __] where
                  [__] replaced the pair. *)
               | Pexp_construct (name, Some ({ pexp_desc = Pexp_tuple items; _ } as arg))
                 ->
                 { e with
                   pexp_desc =
                     Pexp_construct
                       ( name
                       , Some
                           { arg with
                             pexp_desc = Pexp_tuple (List.map items ~f:self#expression)
                           } )
                 }
               | _ -> super#expression e))
    end
  in
  cut#expression e
;;

(* How a top-level binding is named in a point's key: the bound name, or the
   printed pattern for anything else ([()], [_], a tuple). *)
let binding_name (p : Ppxlib.pattern) : string =
  match bound_name p with
  | Some name -> name
  | None -> Format.asprintf "%a" Ppxlib.Pprintast.pattern p
;;

let run
      ~(skip : int -> bool)
      ~(arid : Ppxlib.expression -> bool)
      (structure : Ppxlib.structure)
  : t
  =
  let live = ref [] in
  let skipped = ref [] in
  let stdlib_containers = not (uses_base_or_core structure) in
  (* Where we are, innermost first: module names and the top-level binding,
     each with its occurrence so two bindings of the same name differ. Part
     of every point's key, so ids don't depend on line numbers. [names] is
     the same path for people to read, e.g. [Lower.block] or [pp#2] for the
     second [pp]. *)
  let scope = ref [] in
  let names = ref [] in
  let seen = Hashtbl.create 256 in
  let occurrence (key : string) : int =
    let n = Option.value (Hashtbl.find_opt seen key) ~default:0 in
    Hashtbl.replace seen key (n + 1);
    n
  in
  let within : 'a. display:string -> string -> (unit -> 'a) -> 'a =
    fun ~display name f ->
    let path = String.concat ~sep:"/" (List.rev (name :: !scope)) in
    let saved = !scope
    and saved_names = !names in
    let n = occurrence path in
    scope := Printf.sprintf "%s#%d" name n :: saved;
    names
    := (if n = 0 then display else Printf.sprintf "%s#%d" display (n + 1)) :: saved_names;
    Fun.protect
      ~finally:(fun () ->
        scope := saved;
        names := saved_names)
      f
  in
  (* The reason from the innermost enclosing [assay.skip] attribute. *)
  let skipping = ref None in
  let skip_under : 'a. string option -> (unit -> 'a) -> 'a =
    fun reason f ->
    match reason with
    | None -> f ()
    | Some _ ->
      let saved = !skipping in
      skipping := reason;
      Fun.protect ~finally:(fun () -> skipping := saved) f
  in
  (* Records the point and returns the guarded expression, or [None] if the
     point is skipped, so the caller can leave the code as it was.

     [subject] is the code being mutated, as written (before any of its
     children were rewritten). Its printed form goes into the point's key,
     along with the scope and which occurrence of the same code, operator
     and edit this is within that scope. *)
  let choose
        ~(operator : Operator.t)
        ~(loc : Ppxlib.Location.t)
        ~(edit : string)
        ~(subject : Ppxlib.expression)
        ~(replaced_by : Ppxlib.expression)
        (original : Ppxlib.expression)
    : Ppxlib.expression option
    =
    let base =
      String.concat ~sep:"/" (List.rev !scope)
      ^ "\n"
      ^ Ppxlib.Pprintast.string_of_expression (truncated ~depth:4 subject)
    in
    let n =
      occurrence (Printf.sprintf "%s\n%s\n%s" base (Operator.to_string operator) edit)
    in
    let binding =
      match !names with
      | [] -> "(top level)"
      | names -> String.concat ~sep:"." (List.rev names)
    in
    let point =
      Point.make ~operator ~loc ~edit ~binding ~key:(Printf.sprintf "%s\n#%d" base n)
    in
    (* An attribute on the code being mutated counts as well as one around
       it: [xs |> List.rev [@assay.skip "..."]] attaches to [List.rev], and
       it's that stage the point drops. *)
    let attribute =
      match !skipping with
      | Some reason -> Some reason
      | None ->
        (match skip_attribute subject.pexp_attributes with
         | Some reason -> Some reason
         | None -> skip_attribute original.pexp_attributes)
    in
    match attribute with
    | Some reason ->
      skipped := (point, Some reason) :: !skipped;
      None
    | None when skip point.id ->
      skipped := (point, None) :: !skipped;
      None
    | None ->
      live := point :: !live;
      Some (guarded ~id:point.id ~replaced_by original)
  in
  (* Like [choose], but returns [original] unchanged for a skipped point. *)
  let mutate ~operator ~loc ~edit ~subject ~replaced_by (original : Ppxlib.expression)
    : Ppxlib.expression
    =
    Option.value
      (choose ~operator ~loc ~edit ~subject ~replaced_by original)
      ~default:original
  in
  let walk =
    object (self)
      inherit Ppxlib.Ast_traverse.map as super

      method! extension ((name, _) as ext : Ppxlib.extension) =
        if leave_alone name.txt then ext else super#extension ext

      (* Attribute payloads aren't code that runs. *)
      method! attribute (a : Ppxlib.attribute) = a

      method! module_binding (mb : Ppxlib.module_binding) =
        let name = Option.value mb.pmb_name.txt ~default:"_" in
        within ~display:name ("module " ^ name) (fun () -> super#module_binding mb)

      (* Each top-level binding is a scope for point keys. Nested [let]s
         aren't, so the key doesn't depend on how a function is split up
         inside. *)
      method! structure_item (item : Ppxlib.structure_item) =
        match item.pstr_desc with
        | Pstr_value (flag, bindings) ->
          let bindings =
            List.map bindings ~f:(fun (vb : Ppxlib.value_binding) ->
              let name = binding_name vb.pvb_pat in
              (* [let () = ...] reads better in a report as [let ()]. *)
              let display =
                match bound_name vb.pvb_pat with
                | Some name -> name
                | None -> "let " ^ name
              in
              within ~display name (fun () -> self#value_binding vb))
          in
          { item with pstr_desc = Pstr_value (flag, bindings) }
        | _ -> super#structure_item item

      method! value_binding (vb : Ppxlib.value_binding) =
        skip_under (skip_attribute vb.pvb_attributes) (fun () -> self#binding vb)

      method private binding (vb : Ppxlib.value_binding) =
        let subject = vb.pvb_expr in
        let vb = super#value_binding vb in
        match bound_name vb.pvb_pat, vb.pvb_expr.pexp_desc with
        | ( Some name
          , Pexp_function
              ( (_ :: _ as params)
              , (Some (Pconstraint ty) as annotation)
              , Pfunction_body body ) ) ->
          (* Don't replace the body of a function that's arid anyway, like a
             pretty-printer. *)
          (match if arid body then None else default ~loc:body.pexp_loc ty with
           | None -> vb
           | Some replaced_by ->
             let edit =
               name ^ " -> " ^ Ppxlib.Pprintast.string_of_expression replaced_by
             in
             let body =
               mutate ~operator:Extreme ~loc:vb.pvb_loc ~edit ~subject ~replaced_by body
             in
             { vb with
               pvb_expr =
                 { vb.pvb_expr with
                   pexp_desc = Pexp_function (params, annotation, Pfunction_body body)
                 }
             })
        | _, _ -> vb

      (* Drop one element of [a; b; c]. Wrapping the whole list for each
         mutant would copy it every time, which blows up quickly with nested
         lists. Instead we rewrite it as [[a] @ ([b] @ [c])] and each mutant
         replaces one of those with [[]]. If every element is skipped, the
         literal is left as a literal. *)
      method
        private literal
        (e : Ppxlib.expression)
        (els : Ppxlib.expression list)
        : Ppxlib.expression =
        let open Ppxlib.Ast_builder.Default in
        let loc = e.pexp_loc in
        let n = List.length els in
        let pieces =
          List.mapi els ~f:(fun i (el : Ppxlib.expression) ->
            let point_loc = el.pexp_loc in
            let subject = el in
            let el = self#expression el in
            let loc = el.pexp_loc in
            ( el
            , choose
                ~operator:Sbr
                ~loc:point_loc
                ~edit:(Printf.sprintf "drop %d of %d" (i + 1) n)
                ~subject
                ~replaced_by:(elist ~loc [])
                (elist ~loc [ el ]) ))
        in
        if List.for_all pieces ~f:(fun (_, taken) -> Option.is_none taken)
        then { e with pexp_desc = (elist ~loc (List.map pieces ~f:fst)).pexp_desc }
        else (
          let append = pexp_ident ~loc { txt = Ldot (Lident "Stdlib", "@"); loc } in
          let pieces =
            List.map pieces ~f:(fun ((el : Ppxlib.expression), taken) ->
              match taken with
              | Some piece -> piece
              | None -> elist ~loc:el.pexp_loc [ el ])
          in
          match List.rev pieces with
          | [] -> e
          | last :: earlier ->
            List.fold_left earlier ~init:last ~f:(fun acc piece ->
              eapply ~loc append [ piece; acc ]))

      method! expression (e : Ppxlib.expression) =
        skip_under (skip_attribute e.pexp_attributes) (fun () -> self#mutated e)

      method private mutated (e : Ppxlib.expression) =
        (* Don't recurse into arid expressions; everything inside is arid too. *)
        if arid e
        then e
        else (
          match elements e with
          | Some (_ :: _ as els) -> self#literal e els
          | _ ->
            let offer = offer_of ~stdlib_containers e in
            let subject = e in
            let e = super#expression e in
            (* Negate the condition by applying either [not] or [Fun.id] to it,
             so the condition itself isn't duplicated. *)
            let e =
              match e.pexp_desc, subject.pexp_desc with
              | Pexp_ifthenelse (cond, then_, else_), Pexp_ifthenelse (written, _, _) ->
                let loc = cond.pexp_loc in
                (match
                   choose
                     ~operator:Uoi
                     ~loc
                     ~edit:"c -> not c"
                     ~subject:written
                     ~replaced_by:(Ppxlib.Ast_builder.Default.evar ~loc "Stdlib.not")
                     (Ppxlib.Ast_builder.Default.evar ~loc "Stdlib.Fun.id")
                 with
                 | None -> e
                 | Some chooser ->
                   let cond = Ppxlib.Ast_builder.Default.eapply ~loc chooser [ cond ] in
                   { e with pexp_desc = Pexp_ifthenelse (cond, then_, else_) })
              | _, _ -> e
            in
            let loc = e.pexp_loc in
            let unit = Ppxlib.Ast_builder.Default.eunit ~loc in
            (match offer, e.pexp_desc with
             (* Drop the first statement by replacing it with [()]. *)
             | Sequence, Pexp_sequence (first, rest) ->
               let loc = first.pexp_loc in
               let subject =
                 match subject.pexp_desc with
                 | Pexp_sequence (written, _) -> written
                 | _ -> subject
               in
               { e with
                 pexp_desc =
                   Pexp_sequence
                     ( mutate
                         ~operator:Sbr
                         ~loc
                         ~edit:"e1; e2 -> e2"
                         ~subject
                         ~replaced_by:(Ppxlib.Ast_builder.Default.eunit ~loc)
                         first
                     , rest )
               }
             | Unit_if, _ ->
               mutate
                 ~operator:Sbr
                 ~loc
                 ~edit:"if c then e -> ()"
                 ~subject
                 ~replaced_by:unit
                 e
             | Iter, _ ->
               mutate ~operator:Sbr ~loc ~edit:"iter -> ()" ~subject ~replaced_by:unit e
             (* Replace the stage [f] in [x |> f] with [Fun.id]. This only
              type-checks if [f] returns the same type it takes. *)
             | Pipe, Pexp_apply (op, [ left; (Nolabel, f) ]) ->
               (* Use [f]'s location for the point, since that's where the type
                error will be reported if the mutant doesn't compile. *)
               let loc = f.pexp_loc in
               let identity = Ppxlib.Ast_builder.Default.evar ~loc "Stdlib.Fun.id" in
               let f =
                 mutate
                   ~operator:Sbr
                   ~loc
                   ~edit:"e |> f -> e"
                   ~subject
                   ~replaced_by:identity
                   f
               in
               { e with pexp_desc = Pexp_apply (op, [ left; Nolabel, f ]) }
             (* Only the function is wrapped, so the arguments aren't copied. *)
             | Head { operator; edit; replacement }, Pexp_apply (f, args) ->
               let loc = f.pexp_loc in
               let f =
                 mutate
                   ~operator
                   ~loc
                   ~edit
                   ~subject
                   ~replaced_by:(path_expression ~loc replacement)
                   f
               in
               { e with pexp_desc = Pexp_apply (f, args) }
             (* [let l () = a and r () = b in if active id then l () || r ()
              else l () && r ()]. The operands go in thunks so they're only
              written once (copying them into both branches blows up
              exponentially on long chains) and still short-circuit. The
              point uses the operator's location: [e]'s can be shared with a
              parenthesised left operand, giving two points the same id. *)
             | Connector { edit; dual }, Pexp_apply (f, [ (_, lhs); (_, rhs) ]) ->
               let loc = f.pexp_loc in
               let open Ppxlib.Ast_builder.Default in
               let force name = eapply ~loc (evar ~loc name) [ eunit ~loc ] in
               let call head =
                 eapply ~loc head [ force "assay__lhs"; force "assay__rhs" ]
               in
               (match
                  choose
                    ~operator:Lcr
                    ~loc
                    ~edit
                    ~subject
                    ~replaced_by:(call (evar ~loc dual))
                    (call f)
                with
                | None -> e
                | Some guarded ->
                  let thunk name body =
                    value_binding
                      ~loc
                      ~pat:(pvar ~loc name)
                      ~expr:(pexp_fun ~loc Nolabel None (punit ~loc) body)
                  in
                  { (pexp_let
                       ~loc:e.pexp_loc
                       Nonrecursive
                       [ thunk "assay__lhs" lhs; thunk "assay__rhs" rhs ]
                       guarded)
                    with
                    pexp_attributes = e.pexp_attributes
                  })
             | (Nothing | Sequence | Pipe | Head _ | Connector _), _ -> e))
    end
  in
  let structure = walk#structure structure in
  { live = List.rev !live; skipped = List.rev !skipped; structure }
;;
