open! Core

(** Where a session's tools run: the backend or a connected client. Clients may
    be attached to another session than the one whose state lists them. *)
type t =
  { id : string
  ; name : string
  ; cwd : string
  ; session_id : string option (** absent for the backend *)
  ; session_name : string option
  ; home : string option
    (** its home directory, for showing paths under it as [~]; absent from
        older backends and tool hosts *)
  }
[@@deriving sexp_of, equal]

val backend_id : string
val of_json : Json.t -> t Or_error.t
