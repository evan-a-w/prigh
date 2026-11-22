open! Core
open! Import

(** The model chip's menu beyond its list ([Dialog.Popover] holds the list's
    filter and highlight): which provider's models it shows (or the
    favourites), whether all of them, and whether the access menu in its
    foot is open. Also what the menu offers, and in which order. *)

module Tab : sig
  type t =
    | Favourites
    | Provider of string
  [@@deriving sexp_of, equal]
end

type t =
  { tab : Tab.t
  ; more : bool (** the provider's older models too ("Show more") *)
  ; access_open : bool (** the foot's menu of modes *)
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
    without one last; equal versions keep their order. *)
val newest_first : Llm.t list -> Llm.t list

(** The rail: the favourites, then each provider with a model on offer, in
    the order of their first. *)
val tabs : Llm.t list -> Tab.t list

(** Opened on the current model's provider. *)
val opening : Llm.t list -> current:string option -> t

(** The rows of the tab (favourites in the order they were starred, a
    provider's newest first, cut to {!shown} unless [more]), and how many
    "Show more" adds. *)
val rows : t -> Llm.t list -> favourites:string list -> Llm.t list * int

(** The next tab along the rail, wrapping. *)
val step : t -> Llm.t list -> int -> t

val set_tab : t -> Tab.t -> t

(** Short for the composer's chip: the family goes ("GPT-6.2 Sol" is "6.2
    Sol", "Claude Opus 5.5" "Opus 5.5"); names of two words or fewer stay
    whole. *)
val short_name : string -> string
