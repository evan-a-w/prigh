open! Core
open! Import

module Picker_kind = struct
  type t =
    | Models
    | Thinking
    | Login
    | Logout
    | Verbosity
    | Confirm_tools
    | Fork of Entry.t list
    | Rewind of Entry.t list
    | Tree
    | Hosts
    | Users
    | Accounts
  [@@deriving sexp_of, equal]
end

module Jobs = struct
  type t =
    { jobs : Job_info.t list
    ; selected : int
    ; output : (string * string) option
    }
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
  | Agents
  | Hotkeys
  | Scoped_models of
      { picker : Picker.t
      ; checked : String.Set.t
      }
  | Prompt of Prompt.t
  | Rewind_confirm of
      { id : string
      ; text : string
      }
  | Session of Session_stats.t
  | Jobs of Jobs.t
  | Text of
      { title : string
      ; text : string
      }
[@@deriving sexp_of, equal]

let per_session = function
  | Rename _
  | Agents
  | Prompt _
  | Rewind_confirm _
  | Session _
  | Jobs _
  | Text _
  | Picker { kind = Fork _ | Rewind _ | Tree | Hosts; _ } -> true
  | Picker
      { kind =
          ( Models
          | Thinking
          | Login
          | Logout
          | Verbosity
          | Confirm_tools
          | Users
          | Accounts )
      ; _
      }
  | Help
  | Hotkeys
  | Scoped_models _
  | Delete _
  | Login _
  | Auth _ -> false
;;
