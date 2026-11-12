open! Core

(** What the user set about themselves, given to the model in every session
    ([get_user_info]'s and [set_user_info]'s reply and the
    [user_info_changed] event): a form of optional fields, and the block the
    model is given. *)

module Units : sig
  type t =
    | Metric
    | Imperial
    | Us_customary
  [@@deriving sexp_of, equal, enumerate]

  (** [metric], [imperial], [us_customary] *)
  val to_wire : t -> string

  val of_wire : string -> t option

  (** e.g. [Metric (°C, km, kg)] *)
  val short : t -> string (** a button's *)
  val label : t -> string
end

module Date_format : sig
  type t =
    | Dmy
    | Mdy
    | Iso
  [@@deriving sexp_of, equal, enumerate]

  (** [dmy], [mdy], [iso] *)
  val to_wire : t -> string

  val of_wire : string -> t option

  (** e.g. [D/M/Y (25/12/2026)] *)
  val short : t -> string (** a button's *)
  val label : t -> string
end

module Form : sig
  type t =
    { name : string option
    ; country : string option
    ; time_zone : string option (** IANA *)
    ; language : string option
    ; units : Units.t option
    ; date_format : Date_format.t option
    ; occupation : string option
    ; background : string option
    ; anything_else : string option
    }
  [@@deriving sexp_of, equal]

  val empty : t

  (** The fields' wire names (what [/user-info FIELD VALUE] takes) and what
      each holds, in the form's order. *)
  val fields : (string * string) list

  val to_json : t -> Json.t
  val of_json : Json.t -> t Or_error.t
end

type t =
  { form : Form.t
  ; text : string (** the block the model is given; empty: none *)
  ; max_short : int (** the longest one-line field, in bytes *)
  ; max_background : int
  ; max_anything_else : int
  ; max_total : int (** all fields together *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** [set_user_info] with the whole form; an empty one removes it. *)
val set_params : Form.t -> (string * Json.t) list

(** [set_user_info] of one field (by its wire name); an empty value clears
    it. *)
val set_field_params : field:string -> string -> (string * Json.t) list
