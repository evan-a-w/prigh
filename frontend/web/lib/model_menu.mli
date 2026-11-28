open! Core
open! Import

(** The model chip's menu beyond its list ([Dialog.Popover] holds the list's
    filter and highlight): which provider's models it shows (or the
    favourites), whether all of them, and whether the access or engine menu
    in its foot is open. Also what the menu offers, and in which order. *)

module Tab : sig
  type t =
    | Favourites
    | Provider of string
  [@@deriving sexp_of, equal]
end

type t =
  { mode : Engine_mode.t
  ; tab : Tab.t
  ; more : bool (** the provider's older models too ("Show more") *)
  ; access_open : bool (** the foot's menu of modes *)
  ; engine_open : bool (** the foot's menu of engine modes *)
  }
[@@deriving sexp_of, equal]

val init : t

(** How many of a provider's models show before "Show more". *)
val shown : int

(** The models to choose from: not legacy, of providers that are signed in
    (every provider while the backend has not said which are), and
    [current] whatever it is. In the backend's order. *)
val offered
  :  Llm.t list
  -> auth:Auth_status.t list
  -> current:string option
  -> Llm.t list

(** Newest first: by the version in the name ("GPT-6.2 Sol": 6.2), those
    without one last; equal versions keep their order. A CLI's models (its
    provider has a [default]) keep the order it gave. *)
val newest_first : Llm.t list -> Llm.t list

(** Models belonging to a given execution mode ({!Engine_mode.of_provider}). *)
val models_for_mode : Engine_mode.t -> Llm.t list -> Llm.t list

(** The rail for [mode]: the favourites, then each provider with a model on
    offer in [mode], in the order of their first. *)
val tabs : mode:Engine_mode.t -> Llm.t list -> Tab.t list

(** Opened on [mode] and the current model's provider if in [mode] (else the
    first provider in [mode], else {!Tab.Favourites}). *)
val opening : Llm.t list -> mode:Engine_mode.t -> current:string option -> t

(** Switch the browsed execution mode, closing the mode popover, resetting
    [more], and picking [current]'s provider tab if in the new mode else its
    first provider tab. *)
val set_mode : ?current:string -> t -> Engine_mode.t -> Llm.t list -> t

(** The rows of the tab (favourites in the order they were starred, a
    provider's newest first, cut to {!shown} unless [more]), and how many
    "Show more" adds; the [current] model is always among them. *)
val rows
  :  ?current:string
  -> t
  -> Llm.t list
  -> favourites:string list
  -> Llm.t list * int

(** The next tab along the rail, wrapping. *)
val step : t -> Llm.t list -> int -> t

val set_tab : t -> Tab.t -> t

(** Short for the composer's chip: a maker's name goes ("GPT-6.2 Sol" is
    "6.2 Sol", "Claude Opus 5.5" "Opus 5.5"); others stay whole ("GPT-OSS
    120B (Medium)", "Gemini 3.8 Flash"). *)
val short_name : string -> string
