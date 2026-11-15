open! Core
open! Import

(** The mode new sessions start in, as one choice: an approval mode, or chat
    over the configured approval mode (the config's [default_chat_mode]),
    which leaving chat returns to. *)

type t =
  | Approval of P.Approval_mode.t
  | Chat
[@@deriving sexp_of, equal]

(** ask, auto-edit, smart, auto, chat. *)
val all : t list

val of_config : P.Config.t -> t

(** Chat keeps the approval mode underneath; an approval mode leaves chat. *)
val apply : t -> P.Config.t -> P.Config.t

(** The config fields [apply] sets, to send alone: chat keeps the approval
    mode, which another client may have changed. *)
val fields : t -> string list

(** [fields] of [config], as [set_config] takes them. *)
val json_fields : t -> P.Config.t -> (string * P.Json.t) list

(** ["ask"], ["auto-edit"], ["smart"], ["auto"], ["chat"]: also what
    [/confirm] takes. *)
val to_string : t -> string

(** Also [on] (ask) and [off] (auto). *)
val of_string : string -> t option

(** The session mode a session starts in with [config]'s default. *)
val session_mode : P.Config.t -> Session_mode.t

(** ["chat (leaving it: ask)"], ["ask"]: the default mode in a few words. *)
val summary : P.Config.t -> string

(** What the mode does, in a line; chat's says what leaving it gives. *)
val describe : t -> string

(** What [/confirm] takes, with what each does: on, off, then {!all}. *)
val arguments : (string * string) list
