open! Core
open! Import

(** The page's state of the [render_ui] components on show: what the user
    did to them (a tab picked, a card flipped, a field typed in, a table
    sorted: strings under {!key}s), the replies on their way, and the links
    to the images they show. It belongs to the session: switching resets
    it. *)

module Image : sig
  type t =
    | Loading
    | Url of string
    | Failed of string (** what to tell the user *)
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of]

val empty : t

(** A component's piece of state: [call_id/path/name]. *)
val key : call_id:string -> path:string -> string -> string

val find : t -> string -> string option
val set : t -> key:string -> string -> t

(** A reply sent through [component] of [call_id] and not yet seen in the
    transcript: its values. *)
val sending : t -> call_id:string -> component:string -> Json.t option

val send : t -> Ui_action.t -> t

(** The reply failed: the component can send again. *)
val unsend : t -> call_id:string -> component:string -> t

val image : t -> string -> Image.t option
val set_image : t -> path:string -> Image.t -> t

(** Of [paths], those not asked for yet. *)
val unasked : t -> string list -> string list
