open! Core

(** When the chat follows new output, and when the page shows the jump
    button: the pure part of the page's scroll handling (web_main.ml). *)

(** The chat's scroll position, in CSS pixels ([scrollTop] may be
    fractional at a zoom level, and past the end while a phone's elastic
    overscroll gives way). *)
module Position : sig
  type t =
    { top : float
    ; height : float
    ; client : float
    }
  [@@deriving sexp_of]
end

(** Close enough to the end to follow new output (40px). *)
val at_end : Position.t -> bool

(** Whether following must move the chat: not when it is at its very end
    already. Setting [scrollTop] there changes nothing but cancels what the
    browser is doing with the user's scroll (the elastic overscroll past the
    end, a smooth scroll), which flickered at every chunk of a streamed
    reply. *)
val off_end : Position.t -> bool

(** A wheel turned up, or a finger moving down, over the chat while it
    followed: it stops following at once, so that output arriving before the
    scroll lands cannot undo it, but the page shows the jump button only once
    the chat has moved (a scroll event). A turn the chat does not take (the
    overscroll past the end giving way, a box inside it scrolling instead)
    moves nothing, and the chat follows again a few frames later. *)
module Turn_up : sig
  type t [@@deriving sexp_of]

  module Outcome : sig
    type nonrec t =
      | Taken (** The chat moved up: the scroll listener decides. *)
      | Waiting of t
      | Not_taken (** Follow again, from the end. *)
    [@@deriving sexp_of]
  end

  (** At the chat's [top] when the turn was seen. *)
  val start : top:float -> t

  (** Another turn, before the first was settled. *)
  val again : t -> t

  (** At an animation frame; [held] while a finger is on the screen (counting
      starts when it lifts). *)
  val frame : t -> top:float -> held:bool -> Outcome.t
end
