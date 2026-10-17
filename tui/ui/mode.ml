open! Core
open Prigh_protocol

module Picker_kind = struct
  type t =
    | Models of { logged_in_only : bool }
    | Scoped_models
    | Thinking
    | Verbosity
    | Login
    | Logout
    | Sessions of Session_selection.t
    | Fork of Entry.t list
    | Rewind of Entry.t list
    | Tree of Entry.t list
    | Agents
    | Jobs
    | Queue
    | Modes (** [/mode]: approval modes and plan mode *)
    | Hosts
    | Skills
    | Snippets
    | Reminders
    | Mcp of Mcp_list.t
    | Auth_select of string
  [@@deriving sexp_of, equal]
end

module Text_prompt_action = struct
  type t =
    | Name
    | Cd
    | Mkdir (** [/mkdir]: created, then changed to *)
    | Export_path
    | Import_path
    | Deny_reason of
        { confirm : Pending.Confirm.t
        ; draft : string (** the editor's text, restored afterwards *)
        } (** the reason for denying a tool call, which the model sees *)
    | Host_cwd of string (** host id; the text is the directory there *)
  [@@deriving sexp_of, equal]
end

module Open_picker = struct
  type t =
    { kind : Picker_kind.t
    ; picker : Picker.t
    }
  [@@deriving sexp_of, equal]
end

module Confirm_action = struct
  type t =
    | Logout of string
    | Rewind of string
    | Delete_sessions of
        { paths : string list
        ; back : Open_picker.t option
        }
    | Tool_confirm of Pending.Confirm.t
    | Create_dir of
        { target : Dir_change.Target.t
        ; path : string (** absolute, on the target's host *)
        } (** a missing directory given to [/cd] or [/host] *)
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
  | Question of
      { call_id : string
      ; form : Question_form.t
      } (** the model's [ask_user] *)
  | Search of
      { query : string
      ; matches : int list
      ; current : int
      }
[@@deriving sexp_of]

let is_dialog = function
  | Editing | Search _ -> false
  | Picker _ | Login_prompt _ | Text_prompt _ | Confirm _ | Question _ -> true
;;

let name = function
  | Editing -> "editing"
  | Picker _ -> "picker"
  | Login_prompt _ -> "login"
  | Text_prompt _ -> "text_prompt"
  | Confirm _ -> "confirm"
  | Question _ -> "question"
  | Search _ -> "search"
;;
