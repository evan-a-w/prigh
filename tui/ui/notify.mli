open! Core
open! Import

(** Telling the user that something needs them while they look elsewhere: a
    turn ended, a tool or a plan waits for confirmation, the model asks a
    question, background work finished.
    Both frontends decide the same way; the terminal rings or raises a
    desktop notification, prigh-web a browser one. *)

module Mode : sig
  type t =
    | Off
    | Bell
    | Desktop
  [@@deriving sexp_of, equal, enumerate]

  val to_string : t -> string

  (** The config's [notify]; unknown or missing is [Bell]. *)
  val of_config : P.Config.t option -> t

  val of_string : string -> t option
end

type t =
  { title : string
  ; body : string
  }
[@@deriving sexp_of, equal]

(** What a backend event of the main agent is worth telling ([None] for the
    rest). [where] names the session (its name, else its directory). *)
val of_event : P.Event.t -> where:string -> t option

(** The session as [of_event] names it. *)
val where : P.State.t option -> string
