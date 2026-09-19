open! Core
open Prigh_protocol

(** Which surface owns the keyboard. Dialogs are never stacked. *)

module Picker_kind : sig
  type t =
    | Models
    | Thinking
    | Login
    | Logout
    | Sessions
    | Commands
    | Auth_select of string (** login prompt id *)
  [@@deriving sexp_of, equal]
end

module Confirm_action : sig
  type t = Logout of string [@@deriving sexp_of, equal]
end

type t =
  | Editing
  | Picker of
      { kind : Picker_kind.t
      ; picker : Picker.t
      }
  | Login_prompt of
      { id : string
      ; prompt : Auth_event.Prompt.t
      }
  | Confirm of
      { question : string
      ; action : Confirm_action.t
      }
[@@deriving sexp_of]

val is_dialog : t -> bool
val name : t -> string
