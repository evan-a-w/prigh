open! Core

(** A list with a fuzzy filter and a highlight, for dialogs and the completion
    popup. *)

module Item : sig
  type t =
    { id : string
    ; label : string
    ; detail : string
    ; search : string (** what the filter matches *)
    ; marked : bool (** the current value *)
    ; dimmed : bool (** e.g. its provider is not logged in *)
    }
  [@@deriving sexp_of, equal]

  val create
    :  ?detail:string
    -> ?search:string
    -> ?marked:bool
    -> ?dimmed:bool
    -> id:string
    -> string
    -> t
end

type t [@@deriving sexp_of, equal]

(** The highlight starts on the marked item. *)
val create : ?query:string -> title:string -> Item.t list -> t

val title : t -> string
val query : t -> string

(** The items matching [query], best first. *)
val visible : t -> Item.t list

val selected : t -> int
val selected_item : t -> Item.t option
val set_query : t -> string -> t

(** Moves the highlight by [delta], clamped. *)
val move : t -> int -> t
