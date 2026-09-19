open! Core
open Prigh_protocol

module Picker_kind = struct
  type t =
    | Models
    | Thinking
    | Verbosity
    | Login
    | Logout
    | Sessions
    | Commands
    | Auth_select of string
  [@@deriving sexp_of, equal]
end

module Confirm_action = struct
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

let is_dialog = function
  | Editing -> false
  | Picker _ | Login_prompt _ | Confirm _ -> true
;;

let name = function
  | Editing -> "editing"
  | Picker _ -> "picker"
  | Login_prompt _ -> "login"
  | Confirm _ -> "confirm"
;;
