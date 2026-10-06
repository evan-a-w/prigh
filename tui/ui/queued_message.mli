open! Core

(** A message this client queued while the agent runs, shown above the editor
    until the backend delivers it. *)

module Kind : sig
  type t =
    | Steer (** delivered at the next tool boundary *)
    | Follow_up (** delivered after the turn ends *)
  [@@deriving sexp_of, equal]

  val name : t -> string
end

type t =
  { kind : Kind.t
  ; text : string
  }
[@@deriving sexp_of, equal]

(** Drops the first message with [text] (delivered, or failed to queue). *)
val remove_first : t list -> text:string -> t list

(** Drops the last message with [text] (popped back into the editor). *)
val remove_last : t list -> text:string -> t list
