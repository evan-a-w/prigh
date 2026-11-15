open! Core
open! Import

(** The control centre (Ctrl+K, the sidebar's Search): one filter over the
    settings ({!Control_setting}), their values, the sessions and the slash
    commands. With nothing typed it lists the session's settings, the recent
    sessions and a few commands; typing ranks each group by the filter. This
    is its root level; a setting's values (its own level, a picker) are
    [Dialog.Control]'s. *)

module Group : sig
  type t =
    | This_session
    | Recent
    | Settings
    | Sessions
    | Commands
  [@@deriving sexp_of, equal]

  val title : t -> string
end

module Target : sig
  type t =
    | Setting of Control_setting.t (** its values, or where it is set *)
    | Value of
        { setting : Control_setting.t
        ; id : string (** the item's of its values' picker *)
        } (** applied in one step *)
    | Session of string (** a session's path: switches to it *)
    | Command of string (** a slash command's name *)
  [@@deriving sexp_of, equal]
end

module Row : sig
  type t =
    { target : Target.t
    ; group : Group.t
    ; label : string
    ; matched : (int * int) list
      (** [(start, length)] in bytes of [label]: what the filter matched *)
    ; detail : string (** on the right: its current value, or where *)
    ; keys : string option (** its shortcut *)
    ; marked : bool (** the current value *)
    ; snippet : Session_hits.Snippet.t option
      (** a session's passage that the backend's search found *)
    }
  [@@deriving sexp_of, equal]
end

type t =
  { query : string
  ; selected : int (** of the rows *)
  }
[@@deriving sexp_of, equal]

val empty : t

(** Typed: the highlight back on the first row. *)
val set_query : t -> string -> t

(** The highlight moved by [delta] over [count] rows, clamped. *)
val move : t -> int -> count:int -> t

(** A setting as the root lists it: its [value] now, or [None] when it is not
    on offer (e.g. no tool host to switch to). *)
module Setting_entry : sig
  type t =
    { setting : Control_setting.t
    ; value : string
    ; values : Picker.Item.t list
      (** for {!Control_setting.direct} ones: the values, each an entry *)
    }
end

(** The rows for [t]'s query, in their groups' order. [sessions] are the
    user's, most recent first; [content] the backend's search of their
    messages for the query; [describe] says when and where a session works;
    [current] is the session's path, left out of the recent ones. *)
val rows
  :  t
  -> settings:Setting_entry.t list
  -> sessions:Session_summary.t list
  -> ?content:Session_hits.t
  -> current:string option
  -> describe:(Session_summary.t -> string)
  -> commands:Slash.Spec.t list
  -> unit
  -> Row.t list

(** The commands listed with nothing typed. *)
val common_commands : string list

(** [(start, length)] ranges of sorted byte offsets. *)
val ranges : int list -> (int * int) list
