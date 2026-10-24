open! Core
open! Import

(** Sends a connection's events through its {!Outbox}, merging the
    [message_update]s of a streamed message that wait there: each carries
    the whole message so far, so a peer that reads slower than the model
    writes (a slow link, a busy frontend) would otherwise be sent ever
    larger copies of it until the outbox overflows and the connection
    drops. An update still waiting takes in the next one of the same
    message (and subagent): the later [partial], and both deltas joined
    when they are of one kind (text, thinking, one tool call's arguments).
    Anything else sent in between ends the merging, so that the order of
    events is kept. *)

type t

val create : Outbox.t -> t
val send : t -> Json.t -> unit
