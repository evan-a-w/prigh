open! Core
open! Import

module Picker_kind = struct
  type t =
    | Models
    | Thinking
    | Login
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
[@@deriving sexp_of, equal]

let per_session = function
  | Rename _ -> true
  | Picker _ | Help | Delete _ | Login _ | Auth _ -> false
;;
