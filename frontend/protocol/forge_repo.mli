open! Core

(** The repository of the session's directory on its tool host, as found
    from [git remote]: which forge it is on and its owner and name. *)

type t =
  { host : string
  ; kind : string (** [github] or [forgejo] *)
  ; owner : string
  ; name : string
  ; remote : string (** the git remote it came from, e.g. [origin] *)
  ; url : string (** its web page *)
  }
[@@deriving sexp_of, equal]

(** [owner/name]. *)
val slug : t -> string

val of_json : Json.t -> t Or_error.t
