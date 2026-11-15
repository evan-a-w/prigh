open! Core
open! Import

(** Sends a connection's events through its {!Outbox}, merging the
    [message_update]s of a streamed message that wait there, so that a peer
    that reads slower than the model writes (a slow link, a busy frontend)
    gets fewer, larger deltas instead of falling ever further behind. An
    update still waiting takes in the next one of the same stream (and
    subagent) when both deltas are of one kind and block (text, thinking,
    one tool call's arguments): the text is joined, [seq] is the later's and
    [from] the earlier's first. Of updates with the whole message so far
    ([partial], for clients that do not take deltas, see {!Stream_sync}) the
    later [partial] is kept; a resync is not merged with a delta. Anything
    else sent in between ends the merging, so that the order of events is
    kept. *)

type t

val create : Outbox.t -> t

(** [line]: [Json.to_string json], shared with other connections; written
    unless the update is merged. *)
val send : t -> ?line:string Lazy.t -> Json.t -> unit
