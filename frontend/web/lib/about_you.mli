open! Core
open! Import

(** The settings page's About you section: the form of what the user set
    about themselves for the model ({!User_info}), as [get_user_info] and
    [user_info_changed] said it, and the form being edited. It belongs to
    the user (switching starts over). *)

module Status : sig
  type t =
    | Unknown (** not asked *)
    | Loading
    | Loaded
    | Failed of string
  [@@deriving sexp_of, equal]
end

module Field : sig
  type t =
    | Name
    | Country
    | Time_zone
    | Language
    | Units
    | Date_format
    | Occupation
    | Background
    | Anything_else
  [@@deriving sexp_of, equal, enumerate]

  (** e.g. [time_zone], for element ids *)
  val wire : t -> string

  val label : t -> string
  val placeholder : t -> string

  (** A choice ([Units], [Date_format]): buttons for these
      [(wire value, short label, label)]. *)
  val choices : t -> (string * string * string) list option

  (** [Background] and [Anything_else]: a text area. *)
  val multiline : t -> bool
end

type t =
  { status : Status.t
  ; info : User_info.t (** as the backend has it *)
  ; draft : User_info.Form.t option
    (** edited and not saved; [None]: as saved *)
  ; saving : bool (** sent, no reply yet *)
  ; saved : bool (** just saved: said until the next edit *)
  ; elsewhere : bool
    (** it changed on the backend (another browser, the file) under the draft *)
  ; error : string option (** why it was not saved, and what to do *)
  }
[@@deriving sexp_of, equal]

val empty : t
val loading : t -> t
val loaded : t -> (User_info.t, string) Result.t -> t

(** [user_info_changed]: shown at once unless a draft differs from it. *)
val changed : t -> User_info.t -> t

(** The form as shown: the draft, or as saved. *)
val form : t -> User_info.Form.t

(** A field's value as shown ([""]: empty; a choice's wire value). *)
val value : t -> Field.t -> string

(** The longest value of a field, in bytes; [0] before it is known. *)
val max : t -> Field.t -> int

val dirty : t -> bool

(** Sets a field from what was typed or chosen ([""]: clears it). *)
val set : t -> Field.t -> string -> t

(** Drops the draft for what the backend has. *)
val revert : t -> t

(** [Set_user_info] of the draft, or why not (said under the form). *)
val save : t -> t * Request.Method.t option

val was_saved : t -> (User_info.t, string) Result.t -> t
