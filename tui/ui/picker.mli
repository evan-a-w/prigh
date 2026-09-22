open! Core

(** A modal list with a fuzzy filter. Pure; rendering lives in [Render]. *)

module Item : sig
  type t =
    { id : string
    ; label : string
    ; detail : string
    ; search : string (** what the filter matches against *)
    ; marked : bool (** the current value *)
    ; dimmed : bool (** e.g. provider not logged in *)
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

type t [@@deriving sexp_of]

val create
  :  ?query:string
  -> ?multi:bool
  -> ?checked:String.Set.t
  -> title:string
  -> Item.t list
  -> t

val title : t -> string
val query : t -> string
val visible : t -> Item.t list
val selected : t -> int
val selected_item : t -> Item.t option

(** Whether Space/Enter and the [checked] set apply (multi-select mode). *)
val multi : t -> bool

(** The ids currently checked in a multi-select picker. *)
val checked : t -> String.Set.t

module Outcome : sig
  type nonrec t =
    | Continue of t
    | Selected of Item.t
    | Cancelled
end

(** Enter with no match keeps the picker open; only Esc cancels. *)
val handle : t -> Intent.t -> page:int -> Outcome.t
