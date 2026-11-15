open! Core

(** The review page's next (1) or previous (-1) change ([tr.change-start]
    in [#review-scroll]) from what is in view, scrolled to under the top;
    [false] when there is none that way (the caller goes to the next or
    previous file). *)
val scroll : int -> bool
