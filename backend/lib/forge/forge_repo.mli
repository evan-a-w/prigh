open! Core
open! Import

(** A repository on a git host, found from a git remote. *)

type t =
  { instance : Forge_instance.t
  ; owner : string
  ; name : string
  ; remote : string (** the git remote it came from *)
  }
[@@deriving sexp_of]

val web_url : t -> string

(** [/repos/OWNER/NAME] under the instance's API. *)
val api_url : t -> string

(** [{host, kind, owner, name, remote, url}] *)
val to_json : t -> Json.t
