open! Core
open! Import

(** A transcript as DOM: user messages, deliveries of background work,
    assistant text (markdown), thinking, tool cards (with subagents' nested
    transcripts), compactions, notices and how a reply stopped. Messages show
    when they were sent (replies: when they ended) and a separator marks
    where the day changes. *)
val view : Message_time.t -> Chat.t -> Node.t

(** A [data:] URL. *)
val image_src : Image.t -> string
