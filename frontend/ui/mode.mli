open! Core
open Prigh_protocol

(** Which surface owns the keyboard. Dialogs are never stacked. *)

module Picker_kind : sig
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
    | Queue (** [/queue]: the queued steers and follow-ups *)
    | Modes (** [/mode]: approval modes and plan mode *)
    | Hosts
    | Skills
    | Snippets (** [/snippet]: Enter inserts the text *)
    | Reminders (** [/remind]: the session's; Ctrl+D removes one *)
    | Mcp of Mcp_list.t
    | Pull_requests (** [/pr]: Enter shows one *)
    | Auth_select of string (** login prompt id *)
  [@@deriving sexp_of, equal]
end

module Text_prompt_action : sig
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

(** A picker to go back to. *)
module Open_picker : sig
  type t =
    { kind : Picker_kind.t
    ; picker : Picker.t
    }
  [@@deriving sexp_of, equal]
end

module Confirm_action : sig
  type t =
    | Logout of string
    | Forge_logout of string (** the host *)
    | Rewind of string
    | Delete_sessions of
        { paths : string list
        ; back : Open_picker.t option (** where Esc and Enter return *)
        }
    | Tool_confirm of Pending.Confirm.t
    | Create_dir of
        { target : Dir_change.Target.t
        ; path : string (** absolute, on the target's host *)
        } (** a missing directory given to [/cd] or [/host] *)
    | Cd_to_checkout of string
    (** the worktree [/pr checkout] made: change the session's directory *)
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
      ; matches : int list (** wrapped transcript line indexes *)
      ; current : int (** index into [matches] *)
      }
  | Output of Output_box.t
  (** long command output over the transcript; not a dialog: a confirmation
      or a picker replaces it *)
[@@deriving sexp_of]

val is_dialog : t -> bool
val name : t -> string
