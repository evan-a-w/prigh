open! Core
open! Import

(** A transcript as DOM: user messages, deliveries of background work,
    assistant text (markdown), thinking, tool cards (with subagents' nested
    transcripts), compactions, notices and how a reply stopped. Messages show
    when they were sent (replies: when they ended) and a separator marks
    where the day changes. Tool outputs fold as [fold] (default
    [Fold.default]) says. With [trim_after], the user's messages numbered
    after it have "Trim everything before this" ([trim_attribute]). *)
val view : ?fold:Fold.t -> ?trim_after:int -> Message_time.t -> Chat.t -> Node.t

(** The attribute of a message's "Trim everything before this": the entry's
    number. *)
val trim_attribute : string

(** A [data:] URL. *)
val image_src : Image.t -> string
