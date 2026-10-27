(** The transcript's document listeners: a click on a [button.copy] copies the
    [.copy-text] of its [.copyable] block and says so on the button for a
    moment; a click on a fold's button ([Fold.attribute]) toggles it; a
    message's ⋯ ([Chat_view.menu_attribute]), a right-click on it or a long
    press opens its context menu (but on a link or an image, or with text
    selected in it: the browser's, to copy it); Escape closes an enlarged
    image. *)
val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit
