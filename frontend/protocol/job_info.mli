open! Core

(** A background shell job as listed by [list_jobs]. *)
type t =
  { id : string
  ; command : string
  ; running : bool
  ; exit : string option (** e.g. [exited 0], [killed], once finished *)
  ; delivered : bool (** its report reached the main agent *)
  ; elapsed : float (** seconds, until now or until it finished *)
  ; bytes : int (** total output *)
  ; last_line : string option
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
