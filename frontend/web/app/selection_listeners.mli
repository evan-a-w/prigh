open! Core

(** The page's side of commenting on selected code: reads the selection
    ([window.getSelection]) when it settles (the mouse let go, or a touch's
    long press, or keys) and reports it to [Code_review] as
    [Select_text] (its ends as rows and offsets, [Prigh_web.Code_selection]
    making them a range) with where its Comment button goes, or
    [Clear_selection] once it is gone. Only selections in a review's code
    ([.review-code]) count, not ones in its threads or fields. *)

val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit

(** Drops the page's selection ([Code_review.Out.Clear_text_selection]). *)
val clear : unit -> unit
