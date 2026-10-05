open! Core
open! Import

(** prigh-web as a pure state machine: [update] performs no I/O, it returns
    [Command.t]s for the page to execute, whose results come back as
    [Action.Reply]. *)

module Reply_tag : sig
  type t =
    | Ignore
    | Show_error (** only failures are shown *)
    | State
    | Messages
    | Sessions
    | Models
    | Reload_state (** the client's session changed: fetch its state *)
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
    | Set_draft of string
    | Send (** prompt, or steer while running *)
    | Send_follow_up (** after the run, or now when idle *)
    | Abort
    | New_session
    | Switch_session of string (** path *)
    | Set_model of string (** [provider/id] *)
    | Set_thinking of string
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
    ; draft : string
    ; images : Image.t list (** attached to the next prompt *)
    ; queue : int * int (** steer, follow-up *)
    ; confirms : Confirm.t list
    ; toasts : Toast.t list
    ; next_toast : int
    ; sidebar_open : bool
    }
  [@@deriving sexp_of]

  val running : t -> bool
end

val thinking_levels : string list
val init : Model.t
val update : Model.t -> Action.t -> Model.t * Command.t list
