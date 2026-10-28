open! Core
open! Import

(** Conversation history plus the live streaming tails. Pure. *)

module Subagent : sig
  module Status : sig
    type t =
      | Running
      | Done of
          { turns : int
          ; cost_usd : float
          }
      | Failed of string
    [@@deriving sexp_of, equal]
  end

  type t =
    { agent_id : string
    ; task : string
    ; model : string
    ; forked : bool (** it started from its caller's conversation *)
    ; status : Status.t
    ; turns : int
    ; report : string option
    ; last_tool : string option
    ; nested : string list
    }
  [@@deriving sexp_of, equal]
end

module Item : sig
  type t =
    | User of P.Message.User.t
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
    | Server_tool of
        { tool : P.Content.Server_tool.t
        ; ended : bool
          (** its message ended: without output, it was interrupted *)
        } (** a tool the provider ran (web search), among the reply's text *)
    | Notice of Severity.t * string
    | Block of Content.t
    | Compaction of string
    | Trimmed of
        { after : string option
          (** a trim after a message: its range's entry ({!P.Hidden.t.entry});
              [None]: the trims before one *)
        ; messages : int
        ; tokens : int
        ; shown : P.Message.t list option
          (** unfolded ([/trim show]): the messages out of the context *)
        }
    (** the divider standing for what is trimmed from the model's context:
        at the transcript's start for the trims before a message, where the
        messages were for a trim after one *)
    | Delivery of string
    (** finished background subagents' reports handed to the main agent *)
    | Skill of
        { skill : Skill_message.t
        ; user : P.Message.User.t
        } (** a user message invoking a skill ([/skill:NAME ARGS], expanded) *)
    | Note of
        { kind : P.Origin.Kind.t
        ; title : string
        ; text : string (** what the model saw *)
        ; joined : bool
          (** follows another note directly, without the blank line that
              starts a turn: a message's notes and the note of a message
              of the harness's read as one group *)
        }
    (** a message the harness wrote in the user's place: a hand-over, a goal,
        [/review], ... (reports of background work are [Delivery], the
        compaction summary [Compaction]) *)
  [@@deriving sexp_of, equal]

  (** What [/filter] calls it: a message's notes go with it. [None]: always
      on show (the divider in place of what is trimmed, which [/jump] goes
      to). *)
  val kind : t -> Transcript_filter.Kind.t option
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

(** Unfolds ([Some messages]) or folds the [Trimmed] divider whose [after]
    is [after]; [false] when there is none. *)
val show_trimmed
  :  t
  -> after:string option
  -> P.Message.t list option
  -> t * bool

val clear : t -> t

(** Without the items (and the text streaming) of the kinds [filter] hides. *)
val filter : t -> Transcript_filter.t -> t

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

(** How many items are complete: messages, notices, tools with their result
    (a running tool and the text still streaming do not count). *)
val finished : t -> int

(** Total wrapped line count, for scroll clamping. *)
val line_count : t -> width:int -> verbosity:Verbosity.t -> int

(** Every wrapped line, oldest first. *)
val render_all : t -> width:int -> verbosity:Verbosity.t -> Content.t

(** Wrapped line index of each user turn's first line, oldest first, and of
    the [Trimmed] divider's (its messages are not turns of the
    transcript), in [t] as [filter] (default: everything) shows it: where
    it hides your messages, the first line shown after each. *)
val user_message_lines
  :  ?filter:Transcript_filter.t
  -> t
  -> width:int
  -> verbosity:Verbosity.t
  -> int list

(** Renders the wrapped lines in [\[top, top + rows)]. *)
val render_window
  :  t
  -> width:int
  -> rows:int
  -> top:int
  -> verbosity:Verbosity.t
  -> Content.t

(** The item's lines in the work log, wrapped at [width] if given. *)
val render_item : ?width:int -> Item.t -> verbosity:Verbosity.t -> Content.t
