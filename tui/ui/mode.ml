open! Core
open Prigh_protocol

module Picker_kind = struct
  type t =
    | Models
    | Thinking
    | Verbosity
    | Login
    | Logout
    | Sessions of
        { named_only : bool
        ; sessions : Session_summary.t list
        }
    | Fork of Entry.t list
    | Rewind of Entry.t list
    | Tree of Entry.t list
    | Agents
    | Auth_select of string
  [@@deriving sexp_of, equal]
end

module Text_prompt_action = struct
  type t =
    | Name
    | Cd
    | Export_path
    | Import_path
  [@@deriving sexp_of, equal]
end

module Confirm_action = struct
  type t =
    | Logout of string
    | Rewind of string
    | Delete_session of string
  [@@deriving sexp_of, equal]
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
  | Text_prompt of
      { question : string
      ; action : Text_prompt_action.t
      }
  | Confirm of
      { question : string
      ; action : Confirm_action.t
      }
[@@deriving sexp_of]

let is_dialog = function
  | Editing -> false
  | Picker _ | Login_prompt _ | Text_prompt _ | Confirm _ -> true
;;

let name = function
  | Editing -> "editing"
  | Picker _ -> "picker"
  | Login_prompt _ -> "login"
  | Text_prompt _ -> "text_prompt"
  | Confirm _ -> "confirm"
;;
