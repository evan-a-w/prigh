(** A carousel with motion and swipe built using Embla. Port of shadcn/ui's
    [Carousel] (Embla's behaviour, done here: the slides move to snap
    points, by the previous and next buttons, the left and right arrow
    keys, or dragging and swiping them; snap points are measured from the
    slides, trimmed so the slides never leave space at the ends).

    {[
      let%sub carousel = Carousel.component graph in
      let%arr carousel in
      Carousel.root carousel ~class_:"w-full max-w-xs"
        [ Carousel.content carousel
            [ Carousel.item carousel [ Node.text "1" ]
            ; Carousel.item carousel [ Node.text "2" ]
            ]
        ; Carousel.previous carousel
        ; Carousel.next carousel
        ]
    ]} *)

open! Import

module Orientation : sig
  type t =
    | Horizontal
    | Vertical
  [@@deriving enumerate, sexp_of, compare, equal]

  (** ["horizontal"] or ["vertical"]. *)
  val name : t -> string
end

(** Where a slide sits in the viewport when it is selected (Embla's
    [align]). *)
module Align : sig
  type t =
    | Start
    | Center
    | End
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** The selected snap point and how many there are (0 until the slides are
    measured in the page). *)
module State : sig
  type t =
    { selected : int
    ; count : int
    }
  [@@deriving sexp_of, compare, equal]
end

(** A carousel's state, for its parts, and its API: Embla's
    [selectedScrollSnap], [scrollSnapList().length], [canScrollPrev],
    [scrollTo]... *)
type t

(** Controlled. [id] (unique on the page) names the carousel's state in
    the DOM. [align] is [Center] by default; [loop] goes from the last
    slide to the first and back; [autoplay] (milliseconds) moves to the
    next slide on a timer, paused while the pointer is over the carousel
    and stopped for good once the slides are dragged (Embla's Autoplay
    plugin with [stopOnInteraction]). *)
val create
  :  ?orientation:Orientation.t
  -> ?align:Align.t
  -> ?loop:bool
  -> ?autoplay:int
  -> id:string
  -> state:State.t
  -> on_change:(State.t -> unit Effect.t)
  -> unit
  -> t

(** Keeps the state. *)
val component
  :  ?orientation:Orientation.t
  -> ?align:Align.t
  -> ?loop:bool
  -> ?autoplay:int
  -> local_ Bonsai.graph
  -> t Bonsai.t

val selected : t -> int
val count : t -> int
val can_scroll_prev : t -> bool
val can_scroll_next : t -> bool
val scroll_prev : t -> unit Effect.t
val scroll_next : t -> unit Effect.t
val scroll_to : t -> int -> unit Effect.t

(** The carousel ([Carousel]): a [role="region"]; the left and right arrow
    keys move it. *)
val root : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t

(** The viewport and the track of slides that moves in it
    ([CarouselContent]); [class_] and [attrs] go on the track. *)
val content : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t

(** A slide ([CarouselItem]); a [basis-*] class sets how much of the
    viewport it takes (all of it by default). *)
val item : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t

(** The button to the previous slide ([CarouselPrevious]): outline, icon
    sized, disabled at the start. *)
val previous
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Button.Variant.t
  -> ?size:Button.Size.t
  -> t
  -> Node.t

(** The button to the next slide ([CarouselNext]). *)
val next
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Button.Variant.t
  -> ?size:Button.Size.t
  -> t
  -> Node.t
