open! Core
open! Import

(** Context hygiene for computer use: a screenshot is worth little once
    newer ones show the screen, and each costs a thousand tokens or more.
    Requests carry only the latest few as images; the session keeps them
    all. *)

(** The placeholder text an omitted screenshot leaves: ["[screenshot
    omitted]"]. *)
val placeholder : string

(** [messages] with the images of [computer] results other than the last
    [keep] replaced by {!placeholder} (a line after the result's text). *)
val keep_last : int -> Message.t list -> Message.t list
