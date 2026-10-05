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

module Reply_tag : sig
  type t =
    | Ignore
    | Show_error (** only failures are shown *)
    | State
    | Messages
    | Sessions
    | Models
    | Auth_status of Auth_purpose.t
    | Paths of string (** the completion prefix listed *)
    | Restored (** [abort]: queued prompts back to the editor *)
    | Dequeued
    | Deleted of string (** the session's title *)
    | Refresh_sessions (** on success, list the sessions again *)
    | Login_started
    | Notice of string (** shown on success *)
    | Reconnect of int (** generation; stale replies are ignored *)
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
    | Sign_out (** forget the saved login and reload *)
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
    | Open_agents
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
    | Cancel_subagent of string
    | Kill_job of string
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
  [@@deriving sexp_of]
end

module Model : sig
  type t =
    { connection : Connection.t
    ; generation : int
    ; hello : Hello_reply.t option
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
    ; toasts : Toast.t list
    ; next_toast : int
    ; sidebar_open : bool
    ; session_query : string
    }
  [@@deriving sexp_of]

  val running : t -> bool

  (** The completion popup when it has something to show. *)
  val popup : t -> Completion.t option
end

val thinking_levels : string list
val init : Model.t
val update : Model.t -> Action.t -> Model.t * Command.t list
