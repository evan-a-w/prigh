open! Core
open! Import

(** The modal dialog that owns the keyboard (tool confirmations are separate:
    they queue up and show above it). *)

module Picker_kind : sig
  type t =
    | Models
    | Thinking
    | Login (** item ids are [provider method] *)
    | Logout
  [@@deriving sexp_of, equal]
end

type t =
  | Picker of
      { kind : Picker_kind.t
      ; picker : Picker.t
      }
  | Help
  | Rename of string
  | Delete of
      { path : string
      ; title : string
      }
  | Login of Login_flow.t
  | Auth of Auth_status.t list
  | Agents (** the session's background subagents and jobs *)
[@@deriving sexp_of, equal]

(** Belongs to the session (closed when switching). *)
val per_session : t -> bool
