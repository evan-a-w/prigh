open! Core
open! Import

(** The toasts ([App.Model.toasts]) and the notice that a new version is
    ready, drawn as Sonner's toasts (loon's [Sonner], the markup's look, not
    its toaster: App owns them, their lifetimes and their Undo). Over the
    bottom of the chat column, above the composer, newest nearest it;
    style.css places them (above a dialog, beside the jump button, on a
    phone). *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
