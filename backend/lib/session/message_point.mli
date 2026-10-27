open! Core
open! Import

(** The messages of a session's context, with what trimming before each,
    deleting it or trimming after it would take out ([get_message_points],
    [/trim], [/delete], [/trim after]). *)

module Role : sig
  type t =
    | User
    | Assistant
    | Tool_result
  [@@deriving sexp_of]

  (** [user], [assistant], [tool_result]. *)
  val name : t -> string
end

module Effect : sig
  type t =
    { messages : int
    ; tokens : int (** estimated ({!Message.estimate_tokens}) *)
    }
  [@@deriving sexp_of]
end

type t =
  { message : string (** the message's entry id, for [delete], [trim_after] *)
  ; index : int (** its position, as {!Outline.Point.index} counts *)
  ; role : Role.t
  ; text : string
    (** its start: what the user wrote, the reply's text (or its tool calls),
        the tool's name; cut as {!Outline.Point.text} *)
  ; at : float option
  ; trim_before : Effect.t option
    (** what trimming before it takes out: what comes before it; [None] for
        the first message, a tool result (a trim before its call's reply
        keeps them together) and where that would part a call from its
        result *)
  ; delete : Effect.t option
    (** what deleting it takes out (with its calls' results); [None] for a
        tool result, which goes with its call *)
  ; trim_after : Effect.t option
    (** what trimming after it takes out; [None] where that is refused *)
  }
[@@deriving sexp_of]

(** From [Session.identified_messages], oldest first; the compaction summary
    is left out. *)
val create : (string option * Timed_message.t) list -> t list

(** [{points: [{message, index, role: user|assistant|tool_result, text,
    at?, trim_before?: {messages, tokens}, delete?: {messages, tokens},
    trim_after?: {messages, tokens}}]}]. *)
val to_json : t list -> Json.t

(** Whether any call made in [dropped] has its result in [kept]. *)
val splits_a_call
  :  dropped:(string option * Timed_message.t) list
  -> kept:(string option * Timed_message.t) list
  -> bool

(** The ids of the messages deleting [message] takes out: it and, for an
    assistant message, the results of its calls (those before the next
    assistant message, as [Tool_pairing] pairs them). *)
val deleted_with
  :  (string option * Timed_message.t) list
  -> message:string
  -> String.Set.t

(** Why the context cannot be trimmed after [after], if it cannot. *)
val trim_after_error
  :  (string option * Timed_message.t) list
  -> after:string
  -> string option
