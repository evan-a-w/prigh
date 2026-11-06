open! Core

(** Copies made in the user's gesture. Bonsai applies an action, and so
    performs its [Command.Copy], a frame later, outside the click or key
    press: WebKit (Safari, the desktop app's webview) then refuses the
    fallback's [execCommand "copy"]. So the listeners that see the gesture
    run [App.update] on the current model themselves and copy at once; the
    command, when it comes, takes that copy's result. *)

(** In a click or key handler: does the copies among the commands of the
    action it is about to schedule. *)
val in_gesture : Prigh_web.App.Command.t list -> unit

(** For [Command.Copy]: the result of the copy of [text] made in the gesture,
    or else a copy now. *)
val copy : string -> bool Async_kernel.Deferred.t
