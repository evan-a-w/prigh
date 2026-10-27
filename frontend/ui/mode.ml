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
    | Trim of Trim.Point.t list
    | Trim_after of Message_point.t list
    | Delete_message of Message_point.t list
    | Tree of Entry.t list
    | Agents
    | Jobs
    | Queue
    | Modes (** [/mode]: approval modes and plan mode *)
    | Hosts
    | Skills
    | Snippets
    | Reminders
    | Worktrees of Worktree.Listing.t
    | Mcp of Mcp_list.t
    | Pull_requests (** [/pr]: Enter shows one *)
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
    | Connect (** [/connect]'s address: [HOST:PORT] or [local] *)
    | Forge_host of
        { kind : string
        ; method_ : string option
        } (** the git host to sign in to *)
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
    | Forge_logout of string (** the host *)
    | Rewind of string
    | Trim of string
    | Trim_after of string
    | Delete_sessions of
        { paths : string list
        ; back : Open_picker.t option
        }
    | Tool_confirm of Pending.Confirm.t
    | Create_dir of
        { target : Dir_change.Target.t
        ; path : string (** absolute, on the target's host *)
        } (** a missing directory given to [/cd] or [/host] *)
    | Cd_to_checkout of string
    (** the worktree [/pr checkout] made: change the session's directory *)
    | Remove_worktree of
        { worktree : Worktree.t
        ; force : bool
        ; back : Open_picker.t option (** where Esc returns *)
        }
    | Nix_gc of Nix_generations.t
    (** [/nix-gc run DAYS|all]: deleting profile generations *)
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
  | Sign_in of Sign_in_form.t (** [/signin], or a server refusing us *)
  | Pr_form of Pr_form.t (** [/pr new] *)
  | Search of
      { query : string
      ; matches : int list
      ; current : int
      }
  | Output of Output_box.t
[@@deriving sexp_of]

let is_dialog = function
  | Editing | Search _ | Output _ -> false
  | Picker _
  | Login_prompt _
  | Text_prompt _
  | Confirm _
  | Question _
  | Sign_in _
  | Pr_form _ -> true
;;

let name = function
  | Editing -> "editing"
  | Picker _ -> "picker"
  | Login_prompt _ -> "login"
  | Text_prompt _ -> "text_prompt"
  | Confirm _ -> "confirm"
  | Question _ -> "question"
  | Sign_in _ -> "sign_in"
  | Pr_form _ -> "pr_form"
  | Search _ -> "search"
  | Output _ -> "output"
;;
