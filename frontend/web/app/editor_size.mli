open! Core

(** The composer's editor ([#editor]) is as tall as its text, up to its CSS
    max-height, past which it scrolls. Its height is a whole number of pixels
    ([scrollHeight]) while its text may be a fraction taller (a line of 22.5px,
    a zoom of 90%), so it shows a scrollbar only once it is at its
    max-height. *)

(** Fits it once the DOM is patched. [grown]: text was only added at its end,
    so it can only have grown (one layout, not two). *)
val fit : grown:bool -> unit

(** Fits it again when its width changes (the sidebar, the panels) or the web
    fonts finish loading: its text may wrap differently. *)
val install : unit -> unit
