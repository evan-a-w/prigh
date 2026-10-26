open! Core

(** The page's side of the review's comment margin
    ([Prigh_web.Review_code_view], [.review-margin]): each thread (or the
    draft) at the height of its line ([data-side], [data-line]), moved down
    when the one above it is in the way, and the margin as tall as they
    are. Laid out again when the code view changes or anything in it
    resizes. *)

val install : unit -> unit
