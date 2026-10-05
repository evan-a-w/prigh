(** The agents panel's document listeners: a click on a subagent's card
    ([data-agent], in the chat or in the panel) opens it in the panel; the
    panel's left edge resizes it (the width is remembered); the panel's body
    follows new output unless scrolled up. *)
val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit

(** Scrolls the chat to a subagent's card, given the [subagent] call ids from
    the top-level card down, opening the transcripts it is folded in, and
    highlights it for a moment. *)
val reveal : string list -> unit
