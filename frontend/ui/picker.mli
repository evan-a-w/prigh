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
    ; section : string
      (** items are listed by section, in the order sections first appear, with
        a divider naming each non-empty one; [""] by default *)
    }
  [@@deriving sexp_of, equal]

  val create
    :  ?detail:string
    -> ?search:string
    -> ?marked:bool
    -> ?dimmed:bool
    -> ?section:string
    -> id:string
    -> string
    -> t
end

type t [@@deriving sexp_of, equal]

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

(** The ids currently checked in a multi-select picker, or marked with
    [toggle_check]. *)
val checked : t -> String.Set.t

(** Replaces the items, keeping the query, the checks of items still there
    (or setting [checked]) and the highlight on [select] (default: the
    highlighted item), else at the same row. *)
val set_items
  :  ?title:string
  -> ?select:string
  -> ?checked:String.Set.t
  -> t
  -> Item.t list
  -> t

(** Checks or unchecks the highlighted item and moves to the next one. *)
val toggle_check : t -> t

module Outcome : sig
  type nonrec t =
    | Continue of t
    | Selected of Item.t
    | Cancelled
end

(** Enter with no match keeps the picker open; only Esc cancels. *)
val handle : t -> Intent.t -> page:int -> Outcome.t
