(** The mutation operators.

    Sbr, Ror, Lcr, Aor and Uoi are the classic Mothra operators. [Extreme] and
    [Empty] are additions, described below. *)

type t =
  | Extreme
  (** Replace a function body with a default value of its return type. If
      this survives, nothing tests what the function computes. *)
  | Sbr (** statement block removal *)
  | Ror (** relational operator replacement *)
  | Lcr (** logical connector replacement *)
  | Aor (** arithmetic operator replacement *)
  | Uoi (** unary operator insertion *)
  | Empty (** Replace a set/map operation with one of its arguments. *)

(** Lowercase name used in [.muts] files and reports. *)
val to_string : t -> string
