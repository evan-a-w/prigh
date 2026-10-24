open! Core

(** Changing the session's directory ([set_cwd], or [set_active_host] with a
    [cwd]) when it may not exist yet: the frontends ask ([Ask]), then create
    it ([Create]) once confirmed. *)

module If_missing : sig
  type t =
    | Fail
    | Ask (** a directory that could be created is answered [Missing] *)
    | Create (** created with its parents on the tool host *)
  [@@deriving sexp_of, equal]

  (** The [if_missing] parameter. *)
  val to_json : t -> Json.t
end

(** Where the directory is for. *)
module Target : sig
  type t =
    | Cwd (** the session's, on the active tool host ([set_cwd]) *)
    | Host of
        { id : string
        ; name : string
        } (** switching to that tool host ([set_active_host]) *)
  [@@deriving sexp_of, equal]
end

module Outcome : sig
  type t =
    | Changed of
        { cwd : string option (** absent from backends predating [Ask] *)
        ; created : bool
        }
    | Missing of string (** the absolute path, not created: ask first *)
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end
