open! Core
open! Import

(** The composer, in two tiers. The box: the prompt editor with pending
    images and attachments and the completion popup, and under it a row with
    + (attach files or images) on the left and the microphone (dictation) and
    a round send (steer; stop while running with nothing typed) on the
    right. Under the box, a recessed strip with the model and host chips,
    each opening its menu. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
