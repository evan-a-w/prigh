open! Core

(** Whether a model's error says an image was too large ("image exceeds 5
    MB maximum", "image dimensions exceed max allowed size"): the backend's
    [Image_limit.matches], without regular expressions. [/downsize-images]
    recovers from it. *)
val too_large : string -> bool
