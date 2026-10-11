open! Core
open! Import

(** A message queued while the agent runs (by any frontend), shown above the
    editor until the backend delivers it, with actions to pull it back into
    the editor or remove it ([remove_queued]). *)

module Kind : sig
  type t =
    | Steer (** delivered at the next tool boundary *)
    | Follow_up (** delivered after the turn ends *)
  [@@deriving sexp_of, equal]

  (** ["steer"], ["follow-up"]. *)
  val name : t -> string

  (** When it reaches the agent, in a few words. *)
  val describe : t -> string
end

type t =
  { kind : Kind.t
  ; index : int (** its position in its queue *)
  ; text : string
  }
[@@deriving sexp_of, equal]

(** The queues as [queue_update] gives them: steers first (they are delivered
    first), then follow-ups. *)
val of_queues : steer:string list -> follow_up:string list -> t list

(** E.g. ["steer:0"], for picker items. *)
val id : t -> string

val find : t list -> id:string -> t option

(** [remove_queued]'s parameters. *)
val remove_params : t -> (string * P.Json.t) list

(** The text on one line, for lists. *)
val one_line : t -> string
