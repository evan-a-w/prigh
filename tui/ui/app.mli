open! Core
module P = Prigh_protocol

(** The whole frontend as a pure state machine. [update] never performs I/O; it
    returns [Command.t]s for the platform to execute, and their results come
    back as [Action.Reply]. *)

module Reply_tag : sig
  type t =
    | Ignore
    | Show_error (** report only failures *)
    | Initial_state
    | Initial_messages
    | Reload_messages
    | Auth_refresh
    | Auth_show
    | Auth_login_picker
    | Auth_logout_picker
    | Models_for_picker of string (** initial filter *)
    | Models_for_switch of string (** the argument to resolve *)
    | Models_after_login of string (** provider *)
    | Sessions_picker
    | Set_model_done
    | Compact_done
    | Abort_done
    | Notice_on_success of string
  [@@deriving sexp_of, equal]
end

module Command : sig
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * P.Json.t) list
        ; tag : Reply_tag.t
        }
    | Open_browser of string
    | Quit
  [@@deriving sexp_of, equal]
end

module Action : sig
  type t =
    | Start (** issues the initial requests *)
    | Key of Key.t
    | Intent of Intent.t
    | Event of P.Event.t
    | Protocol_error of string
    | Stderr of string
    | Backend_closed
    | Reply of Reply_tag.t * (P.Json.t, string) Result.t
    | Tick
    | Resize of
        { width : int
        ; height : int
        }
  [@@deriving sexp_of]
end

module Model : sig
  type t =
    { state : P.State.t option
    ; models : P.Model.t list
    ; auth : P.Auth_status.t list
    ; transcript : Transcript.t
    ; editor : Editor.t
    ; mode : Mode.t
    ; queued : Queue_counts.t
    ; viewport : Viewport.t
    ; pending_quit : bool
    ; spinner : int
    ; verbosity : Verbosity.t
    ; width : int
    ; height : int
    ; quitting : bool
    }
  [@@deriving sexp_of]

  val running : t -> bool
end

(** Wrapped transcript lines currently visible, mirroring [Render.screen] for
    [Editing] mode; approximate for dialog modes. *)
val transcript_rows : Model.t -> int

val init : Model.t
val format_tokens : int -> string
val update : Model.t -> Action.t -> Model.t * Command.t list
