open! Core
open! Import

(** A transcript as DOM: user messages, deliveries of background work,
    assistant text (markdown), thinking, tool cards (with subagents' nested
    transcripts), compactions, notices and how a reply stopped. *)
val view : Chat.t -> Node.t

(** A [data:] URL. *)
val image_src : Image.t -> string
