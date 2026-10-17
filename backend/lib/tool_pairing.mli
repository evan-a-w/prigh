open! Core
open! Import

(** Providers refuse a conversation in which a tool call is not answered by
    its result right after the call's message (Anthropic: "tool_use ids
    were found without tool_result blocks immediately after"; OpenAI: "No
    tool output found"). A session can hold one when prigh stopped while
    tools ran (a backend restart, a crash) and a message followed later. *)

(** The result standing for a call that never returned. *)
val interrupted : Content.Tool_call.t -> Message.Tool_result.t

(** [messages] with every assistant message's calls answered right after
    it, in the calls' order: by their results found before the next
    assistant message (moved up if something came in between), else by
    {!interrupted}. Results that answer no call there are dropped. Already
    well-formed lists come back unchanged. *)
val repair : Message.t list -> Message.t list

(** The calls of the last assistant message that have no result, when only
    tool results follow it. *)
val unanswered : Message.t list -> Content.Tool_call.t list
