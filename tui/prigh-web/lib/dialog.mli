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
    | Verbosity
    | Confirm_tools
    | Fork of Entry.t list (** item ids are entry ids *)
    | Rewind of Entry.t list
    | Tree
    | Hosts (** item ids are host ids *)
    | Users (** act as one ([/setusr]) *)
    | Accounts (** the account menu: see [App] for the item ids *)
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
  | Hotkeys
  | Scoped_models of
      { picker : Picker.t
      ; checked : String.Set.t (** model keys *)
      }
  | Prompt of Prompt.t
  | Rewind_confirm of
      { id : string
      ; text : string
      }
  | Session of Session_stats.t
  | Text of
      { title : string
      ; text : string
      }
[@@deriving sexp_of, equal]

(** Belongs to the session (closed when switching). *)
val per_session : t -> bool
