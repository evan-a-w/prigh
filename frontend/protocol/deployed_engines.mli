open! Core

(** What a backend deploys, from its [hello] reply: absent (a backend with
    the harness alone, or from before engines) is the harness alone. *)
type t =
  { engines : string list (** the first is new sessions' default *)
  ; adapters : string list (** the composer's, the first its default *)
  }
[@@deriving sexp_of, equal]

val harness_only : t

(** The composer is one of them: new sessions may choose an engine and an
    adapter ([new_session]'s [engine] and [adapter]). *)
val has_choice : t -> bool

val of_hello_json : Json.t -> t Or_error.t
