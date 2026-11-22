(** A click on a subagent's card in the chat ([data-agent]) opens its
    chat. *)
val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit
