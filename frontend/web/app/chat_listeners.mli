(** The transcript's document listeners: a click on a [button.copy] copies the
    [.copy-text] of its [.copyable] block and says so on the button for a
    moment; a click on a fold's button ([Fold.attribute]) toggles it; a
    right-click on a message, or a long press, opens its context menu at the
    pointer (Shift+F10 or the Menu key: under it), but on a link or an image,
    or with text selected in it, the browser's, to copy it; Escape closes an
    enlarged image. *)
val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit
