open! Core
open! Import

(** What the user told prigh about themselves (their name, where they are,
    how they write and measure, what they know): a form of optional fields
    in their settings, rendered into a block that, unlike {!Memory}'s notes,
    which the model looks up when it needs them, is in the context of every
    session ([System_prompt.build] for a new one, a note on the next user
    message for one that knows another text: {!Told_environment}). Only the
    user changes it.

    [~/.prigh/about_you.json] under the world's home, so one per namespace
    with [-tokens]: [{"version": 1, <field>: <value>, ...}] with only the
    filled fields, read on every {!get}, so that an edit by hand counts.
    The free text of earlier versions ([~/.prigh/user_info.md]) becomes
    {!Form.anything_else} on the first {!get} after it, and is renamed
    [user_info.md.migrated]. Make one per world. *)

module Units : sig
  type t =
    | Metric
    | Imperial
    | Us_customary
  [@@deriving sexp_of, equal, enumerate]

  (** [metric], [imperial], [us_customary] *)
  val to_wire : t -> string

  val of_wire : string -> t option
end

module Date_format : sig
  type t =
    | Dmy (** 25/12/2026 *)
    | Mdy (** 12/25/2026 *)
    | Iso (** 2026-12-25 *)
  [@@deriving sexp_of, equal, enumerate]

  (** [dmy], [mdy], [iso] *)
  val to_wire : t -> string

  val of_wire : string -> t option
end

module Form : sig
  type t =
    { name : string option (** what to call them *)
    ; country : string option (** country or region *)
    ; time_zone : string option (** an IANA name, e.g. [Australia/Sydney] *)
    ; language : string option (** e.g. [English, Australian spelling] *)
    ; units : Units.t option (** temperature follows *)
    ; date_format : Date_format.t option
    ; occupation : string option
    ; background : string option (** expertise, short *)
    ; anything_else : string option
    }
  [@@deriving sexp_of, equal]

  val empty : t

  (** Fields' wire names, in the form's order: [name], [country],
      [time_zone], [language], [units], [date_format], [occupation],
      [background], [anything_else]. *)
  val fields : string list

  (** Only the filled fields, as [{"name": ..., "units": "metric", ...}]. *)
  val to_json : t -> Json.t

  (** Reads {!to_json}'s fields (unknown ones ignored); text fields are
      stripped, empty ones absent. A field of the wrong type, or a unit or
      date format not among the known ones, is an error naming it. *)
  val of_json : Json.t -> t Or_error.t

  (** Sets one field (by its wire name) from text: units and date formats
      by their wire name or label ([metric], [us], [D/M/Y], [ISO], ...);
      empty clears it. *)
  val set_field : t -> string -> string -> t Or_error.t

  (** The block's lines, one per filled field in the form's order, e.g.
      [- Name: Lloyd]; a multi-line value continues on lines indented by two
      spaces. [None] when no field is filled. *)
  val render : t -> string option
end

type t

(** The longest value of a text field, in bytes, by its wire name. *)
val max_field : string -> int

(** The most bytes of all the fields' values together. *)
val max_total : int

(** [~/.prigh/about_you.json] under [home]. *)
val path : home:string -> string

(** [~/.prigh/user_info.md] under [home]: earlier versions' free text. *)
val legacy_path : home:string -> string

val create : home:string -> t

(** [f] runs after every change made through this value. *)
val subscribe : t -> f:(unit -> unit) -> unit

(** The form ({!Form.empty} without a file), migrating the free text of
    earlier versions first. An error when the file is not a valid form. *)
val get : t -> Form.t Or_error.t

(** {!Form.render} of {!get}; [None] when empty or invalid. *)
val text : t -> string option

(** Replaces the form (fields stripped), atomically; an empty one removes
    the file. Refused when a field is longer than {!max_field} or all are
    longer than {!max_total}. *)
val set : t -> Form.t -> unit Or_error.t
