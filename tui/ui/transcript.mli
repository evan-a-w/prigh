open! Core
module P = Prigh_protocol

(** Conversation history plus the live streaming tails. Pure. *)

module Subagent : sig
  type status =
    | Running
    | Done of
        { turns : int
        ; cost_usd : float
        }
    | Failed of string
  [@@deriving sexp_of, equal]

  type t =
    { agent_id : string
    ; task : string
    ; model : string
    ; status : status
    ; turns : int
    ; report : string option
    ; last_tool : string option
    ; nested : string list
    }
  [@@deriving sexp_of, equal]
end

module Item : sig
  type t =
    | User of string
    | Assistant of
        { text : string
        ; final : bool
        }
    | Thinking of string
    | Tool of
        { call : P.Tool_call.t
        ; result : P.Message.Tool_result.t option
        ; live_tail : string option
        ; subagent : Subagent.t option
        }
    | Notice of Severity.t * string
    | Block of Content.t
    | Compaction of string
  [@@deriving sexp_of, equal]
end

module Stream_kind : sig
  type t =
    | Text
    | Thinking
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of]

val empty : t
val items : t -> Item.t list
val add : t -> Item.t -> t
val add_message : t -> P.Message.t -> t

(** The single event-to-transcript function used for the main transcript and for
    every subagent's transcript. *)
val apply : t -> P.Event.t -> t

val notice : ?severity:Severity.t -> t -> string -> t
val clear : t -> t

(** Streaming: [append] accumulates; a kind change or [flush] turns the
    accumulated text into an item. *)
val append : t -> Stream_kind.t -> string -> t

val flush : t -> t

(** Re-tags the most recent assistant item as final (end of turn). *)
val mark_final : t -> t

(** Adds a tool call with no result yet. Flushes any open text stream. *)
val add_tool : t -> P.Tool_call.t -> t

(** Appends streamed output to the matching open tool, keeping the last five
    lines. *)
val append_tool_output : t -> call_id:string -> string -> t

(** Fills in the result of the matching open tool (adding a tool item if the
    start was missed). *)
val end_tool : t -> call:P.Tool_call.t -> result:P.Message.Tool_result.t -> t

(** Renders wrapped lines, newest last. Only the last [rows + skip] lines are
    produced; [skip] is how many bottom lines to hide (scrolling). *)
val render_tail
  :  t
  -> width:int
  -> rows:int
  -> skip:int
  -> verbosity:Verbosity.t
  -> Content.t

(** Total wrapped line count, for scroll clamping. *)
val line_count : t -> width:int -> verbosity:Verbosity.t -> int

(** Every wrapped line, oldest first. *)
val render_all : t -> width:int -> verbosity:Verbosity.t -> Content.t

(** Wrapped line index where each [User] item starts, oldest first. *)
val user_message_lines : t -> width:int -> verbosity:Verbosity.t -> int list

(** Renders the wrapped lines in [\[top, top + rows)]. *)
val render_window
  :  t
  -> width:int
  -> rows:int
  -> top:int
  -> verbosity:Verbosity.t
  -> Content.t

val render_item : Item.t -> verbosity:Verbosity.t -> Content.t
