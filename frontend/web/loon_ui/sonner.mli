(** An opinionated toast component. Port of shadcn/ui's [Sonner] (its
    [Toaster], over a port of Emil Kowalski's Sonner: the same DOM, data
    attributes and stylesheet, [src/css/sonner.css]).

    Put a {!toaster} once in the page; then {!toast}, {!success}, ... from
    anywhere are effects that show a toast in it:

    {[
      let%sub toaster = Sonner.toaster graph in
      ...
      Button.view ~on_click:(Sonner.toast "Event has been created") [ ... ]
    ]}

    Toasts stack in a corner, the newest in front and up to
    [visible_toasts] (3) peeking behind it; hovering the stack spreads them
    out and pauses their timers. Each closes itself after its [duration]
    (4 seconds), unless it is loading. *)

open! Import

module Type : sig
  type t =
    | Default
    | Success
    | Info
    | Warning
    | Error
    | Loading
  [@@deriving enumerate, sexp_of, compare, equal]

  (** Sonner's name, as in [data-type]: [success], ... ([Default]: none). *)
  val name : t -> string option
end

module Position : sig
  type t =
    | Top_left
    | Top_center
    | Top_right
    | Bottom_left
    | Bottom_center
    | Bottom_right
  [@@deriving enumerate, sexp_of, compare, equal]

  (** Sonner's name: [bottom-right], ... *)
  val name : t -> string
end

module Theme : sig
  (** [System] (the default): light, or dark under loon's [.dark] class. *)
  type t =
    | Light
    | Dark
    | System
  [@@deriving enumerate, sexp_of, compare, equal]
end

(** A [duration] that never ends: the toast stays until dismissed. *)
val infinity : Time_ns.Span.t

(** A toast with no type (no icon). The options, as Sonner's:
    - [id]: a toast with the same id that is showing is updated instead;
    - [description]: a second, muted line;
    - [action]: a button (its label, what it does) that also dismisses;
    - [cancel]: a quieter button, likewise;
    - [duration]: how long it shows (default: the toaster's, 4 seconds);
    - [dismissible] (default [true]): [false] keeps it until dismissed by
      id, without a close button;
    - [close_button], [rich_colors]: override the toaster's;
    - [class_]: classes on the toast. *)
val toast
  :  ?id:string
  -> ?description:string
  -> ?action:string * unit Effect.t
  -> ?cancel:string * unit Effect.t
  -> ?duration:Time_ns.Span.t
  -> ?dismissible:bool
  -> ?close_button:bool
  -> ?rich_colors:bool
  -> ?class_:string
  -> string
  -> unit Effect.t

(** The type of a toast's argument list, for {!success} and the rest. *)
type 'a with_options :=
  ?id:string
  -> ?description:string
  -> ?action:string * unit Effect.t
  -> ?cancel:string * unit Effect.t
  -> ?duration:Time_ns.Span.t
  -> ?dismissible:bool
  -> ?close_button:bool
  -> ?rich_colors:bool
  -> ?class_:string
  -> 'a

val success : (string -> unit Effect.t) with_options
val info : (string -> unit Effect.t) with_options
val warning : (string -> unit Effect.t) with_options
val error : (string -> unit Effect.t) with_options

(** A spinning loader; it stays until updated (same [id]) or dismissed. *)
val loading : (string -> unit Effect.t) with_options

(** A [loading] toast while [effect] runs, then one of type [Success] with
    [success]'s text for its value, or [Error] with [error]'s. *)
val promise
  :  ?description:string
  -> ?duration:Time_ns.Span.t
  -> loading:string
  -> success:('a -> string)
  -> error:(Error.t -> string)
  -> 'a Or_error.t Effect.t
  -> unit Effect.t

(** Closes the toast [id], or every toast. *)
val dismiss : ?id:string -> unit -> unit Effect.t

(** The toasts' container (shadcn's [<Toaster />]): Sonner's, with
    shadcn's icons and colours (the popover's). Toasts go to the last
    toaster put in the page. [position] defaults to [Bottom_right],
    [visible_toasts] to 3, [duration] to 4 seconds, [gap] to 14 (px).
    [expand]: always spread out; [rich_colors]: coloured by type;
    [close_button]: a close button on each toast. *)
val toaster
  :  ?class_:string
  -> ?position:Position.t
  -> ?theme:Theme.t
  -> ?expand:bool
  -> ?rich_colors:bool
  -> ?close_button:bool
  -> ?visible_toasts:int
  -> ?duration:Time_ns.Span.t
  -> ?gap:int
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t
