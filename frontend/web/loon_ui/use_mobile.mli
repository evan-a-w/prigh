(** Whether the window is narrower than shadcn/ui's mobile breakpoint
    (768px, Tailwind's [md]). Port of shadcn/ui's [useIsMobile] hook. *)

open! Import

(** 768: widths below it are mobile. *)
val breakpoint : int

(** The window's width now is below {!breakpoint} ([false] outside a
    browser). *)
val is_mobile : unit -> bool

(** Follows the window's width as it changes ([false] until activated, and
    outside a browser). *)
val component : local_ Bonsai.graph -> bool Bonsai.t
