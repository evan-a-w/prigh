open! Core
open! Import

(** prigh-web as a pure state machine: [update] performs no I/O, it returns
    [Command.t]s for the page to execute, whose results come back as
    [Action.Reply]. *)

module Auth_purpose : sig
  (** Why we asked for [auth_status]. *)
  type t =
    | Refresh
    | Login_picker
    | Logout_picker
    | Show
  [@@deriving sexp_of, equal]
end

module Entries_purpose : sig
  (** Why we asked for [get_entries]. *)
  type t =
    | Fork
    | Rewind
    | Tree
  [@@deriving sexp_of, equal]
end

module Users_purpose : sig
  type t =
    | Probe (** at startup: only superusers may list them *)
    | Picker
  [@@deriving sexp_of, equal]
end

module Reply_tag : sig
  type t =
    | Ignore
    | Show_error (** only failures are shown *)
    | State
    | Messages
    | Sessions
    | Models
    | Reload_state (** the client's session changed: fetch its state *)
    | Subagent of string
    (** [get_subagent] for this [subagent] call or agent id *)
    | Subagents (** [list_subagents], for the agents panel *)
    | Jobs (** [list_jobs] *)
    | Job_output of string (** the job's id *)
    | Job_started (** [!&command]: the job's id *)
    | Auth_status of Auth_purpose.t
    | Paths of string (** the completion prefix listed *)
    | Restored (** [abort]: queued prompts back to the editor *)
    | Dequeued
    | Deleted of string (** the session's title *)
    | Refresh_sessions (** on success, list the sessions again *)
    | Login_started
    | Notice of string (** shown on success *)
    | Reconnect of int (** generation; stale replies are ignored *)
    | Config (** [get_config] *)
    | Config_saved of string (** the new config; the notice is shown *)
    | Default_saved
    | Session_stats
    | Entries of Entries_purpose.t
    | Reload_messages (** the head moved: fetch state and messages *)
    | Exported
    | Imported
    | Prompt_done of string
    (** closes the prompt dialog with this notice; failures show in it *)
    | Prompt_paths of string (** the prompt's input listed *)
    | Btw of string (** the side question's id *)
    | Users of Users_purpose.t
    | User_switched (** [set_user]'s reply, like [hello]'s *)
  [@@deriving sexp_of, equal]
end

module Command : sig
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * Json.t) list
        ; tag : Reply_tag.t
        }
    | Reconnect of
        { generation : int
        ; delay_ms : int
        ; session : string option
        }
    (** After [delay_ms], connect again and send [hello] (rejoining [session]);
        answered by [Reply (Reconnect generation, hello reply)]. *)
    | Set_url_session of string
    (** the page's [?session=], so a reload rejoins *)
    | Expire_toast of
        { id : int
        ; after_ms : int
        } (** [Dismiss_toast id] after [after_ms] *)
    | Focus of string (** the element with this id, once rendered *)
    | Save_history of string list (** newest first *)
    | Sign_out (** forget this account's saved login and reload *)
    | Reveal of string list
    (** scroll the main chat to a subagent's card, opening the transcripts
        it is in: the [subagent] call ids from the top-level one down *)
    | Copy of string (** to the clipboard *)
    | Switch_account of Accounts.Account.t (** reload signed in as it *)
    | Add_account (** the sign-in form, keeping the saved accounts *)
    | Scroll_chat of int (** by pages *)
    | Jump_to_user_message of int
    (** the previous ([-1]) or next ([1]) user message in view *)
  [@@deriving sexp_of, equal]
end

module Connection : sig
  type t =
    | Connected
    | Reconnecting of
        { attempt : int
        ; generation : int
        }
  [@@deriving sexp_of, equal]

  (** 250ms doubling, capped at 10s. *)
  val delay_ms : attempt:int -> int
end

module Toast : sig
  type t =
    { id : int
    ; text : string
    ; error : bool
    }
  [@@deriving sexp_of, equal]

  (** How long non-error toasts stay. *)
  val lifetime_ms : int
end

module Confirm : sig
  type t =
    { call_id : string
    ; name : string
    ; summary : string
    }
  [@@deriving sexp_of, equal]
end

module Action : sig
  type t =
    | Start (** the initial requests, after [hello] *)
    | Hello of Hello_reply.t
    | Saved_login (** we signed in with a saved user name or token *)
    | Event of Event.t
    | Protocol_error of string
    | Backend_closed
    | Reply of Reply_tag.t * (Json.t, string) Result.t
    | Tick of Time_ns.t (** the clock, for ages; also refreshes the sessions *)
    | Set_narrow of bool (** a phone-sized window: the sidebar is a drawer *)
    | Load_history of string list
    | Set_draft of string (** the caret at the end *)
    | Edit of
        { text : string
        ; cursor : int
        }
    | Send (** prompt, or steer while running; slash commands run *)
    | Send_follow_up (** after the run, or now when idle *)
    | Abort
    | History_older
    | History_newer
    | Complete_move of int
    | Complete_accept of { run : bool }
    (** [run]: a command without arguments, or an argument, runs at once *)
    | Complete_choose of int (** clicked *)
    | Complete_close
    | New_session
    | Switch_session of string (** path *)
    | Ask_delete of string (** path *)
    | Set_session_query of string
    | Open_sessions (** the sidebar, with its search focused *)
    | Set_model of string (** [provider/id] *)
    | Set_thinking of string
    | Open_model_picker
    | Open_thinking_picker
    | Open_help
    | Open_rename
    | Open_subagents of string option
    (** the agents panel: its list, or the agent or job with this number
        (in the list), id or call id *)
    | Toggle_subagents
    | Select_item of Agents.Item.t (** shown in full in the panel *)
    | Agents_back (** from an agent to the list; from the list, closed *)
    | Focus_agent of int (** the [n]th listed, from 1 *)
    | Cycle_agent of int (** the next (1) or previous (-1) listed *)
    | Show_in_chat of string (** a subagent's card in the main chat *)
    | Clock of Time_ns.t
    (** every second while [Model.ticking]: elapsed times, polling jobs *)
    | Picker_query of string
    | Picker_move of int
    | Picker_accept
    | Picker_choose of string (** an item's id, clicked *)
    | Dialog_input of string (** rename, login answers *)
    | Dialog_move of int (** login select options *)
    | Dialog_accept
    | Close_dialog (** no side effects, except cancelling a login *)
    | Login_choose of int (** a login select option, clicked *)
    | Start_login of string (** provider, with its default method *)
    | Logout of string (** provider *)
    | Cancel_subagent of string (** agent id *)
    | Kill_job of string (** job id *)
    | Dequeue (** the last queued message back into the editor *)
    | Toggle_sidebar
    | Respond_confirm of
        { call_id : string
        ; allow : bool
        }
    | Add_image of Image.t
    | Remove_image of int
    | Show_toast of
        { text : string
        ; error : bool
        }
    | Dismiss_toast of int
    | Sign_out
    | Set_accounts of
        { accounts : Accounts.Account.t list
        ; current : Accounts.Account.t option
        } (** the saved sign-ins, from the page *)
    | Open_accounts (** the account menu *)
    | Cycle_verbosity
    | Cycle_model of int (** through the scoped models, forwards or back *)
    | Cycle_thinking
    | Copy_last (** the last reply, to the clipboard *)
    | Close_btw
    | Dialog_toggle (** check or uncheck the highlighted scoped model *)
    | Toggle_scoped of string (** a model key, clicked *)
    | Dialog_complete (** a prompt's highlighted completion *)
    | Choose_suggestion of int (** a prompt's completion, clicked *)
    | Retry_connection
    | Scroll_chat of int
    | Jump_to_user_message of int
    | Run of string (** a slash command, e.g. from a button *)
  [@@deriving sexp_of]
end

module Model : sig
  type t =
    { connection : Connection.t
    ; generation : int
    ; hello : Hello_reply.t option
    ; saved_login : bool (** so signing out means something *)
    ; state : State.t option
    ; chat : Chat.t
    ; sessions : Session_summary.t list
    ; models : Llm.t list
    ; auth : Auth_status.t list
    ; now : Time_ns.t option
    ; narrow : bool
    ; draft : string
    ; cursor : int (** the caret's byte offset in [draft] *)
    ; completion : Completion.t option
    ; history : History.t
    ; images : Image.t list (** attached to the next prompt *)
    ; queue : int * int (** steer, follow-up *)
    ; confirms : Confirm.t list
    ; dialog : Dialog.t option
    ; cancelled_login : string option
      (** the provider whose login we cancelled: its failure is expected *)
    ; toasts : Toast.t list
    ; next_toast : int
    ; sidebar_open : bool
    ; session_query : string
    ; agents : Agents.t (** the agents panel, per session *)
    ; verbosity : Prigh_ui.Verbosity.t
    ; config : Config.t option
    ; btw : Btw.t option
    ; btw_seq : int
    ; accounts : Accounts.Account.t list (** saved in this browser *)
    ; account : Accounts.Account.t option (** the one signed in *)
    ; users : string list option
      (** the users we may act as: [Some] for superusers *)
    }
  [@@deriving sexp_of]

  val running : t -> bool

  (** The agents panel is open with something running: the page sends
      [Clock] every second. *)
  val ticking : t -> bool

  (** The completion popup when it has something to show. *)
  val popup : t -> Completion.t option
end

val thinking_levels : string list
val init : Model.t
val update : Model.t -> Action.t -> Model.t * Command.t list
