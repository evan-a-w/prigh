open! Core
open! Import

(** A session's transcript a page at a time, newest first, for frontends
    that show the latest messages at once and load older ones as the user
    scrolls up ([get_messages_page]). Long tool outputs come cut to their
    first and last lines ({!Elided}); [get_tool_output] has them whole. *)

module Elided : sig
  (** Where a text was cut and how long it was: the text kept is [head ^
      tail], [at] the length of [head]. *)
  type t =
    { at : int
    ; lines : int (** of the whole text, as [Output_view] counts them *)
    ; bytes : int (** of the whole text *)
    }
  [@@deriving sexp_of]

  val to_json : t -> Json.t

  (** [None] when [text] is at most [above] bytes long (by default 8 KiB);
      otherwise its first and last [keep] lines (20), each end at most a
      tenth of [above]. *)
  val cut : ?above:int -> ?keep:int -> string -> (string * t) option
end

type t =
  { messages : Json.t list (** oldest first, as [get_messages] has them *)
  ; older : int (** how many messages come before these *)
  ; before : string option
    (** the entry id to ask for the page before this one with ([None] when
        [older] is 0) *)
  }

(** The [limit] messages before the one with entry id [before] (or the
    newest [limit]), from [Session.identified_messages]. A page never starts
    with a tool result (its call comes with it) or a compaction summary (its
    pinned message does), so it may be longer than [limit]. Fails when
    [before] is not on the session's active path any more. With [from], an
    entry id, the page starts with that message instead (whatever [limit]):
    a frontend fetches again what it has loaded, and what came since. *)
val create
  :  ?elide_above:int
  -> ?from:string
  -> (string option * Timed_message.t) list
  -> limit:int
  -> before:string option
  -> t Or_error.t

val to_json : t -> Json.t

(** One message as [get_messages] has it, a long tool output cut, with
    [elided] saying where. *)
val message : ?elide_above:int -> Timed_message.t -> Json.t

(** The result of the tool call [call_id], whole: what [get_tool_output]
    answers. *)
val find_output
  :  Timed_message.t list
  -> call_id:string
  -> Message.Tool_result.t option
