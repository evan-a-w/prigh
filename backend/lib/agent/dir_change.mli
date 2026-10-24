open! Core
open! Import

(** Changing the session's directory ([set_cwd], and [set_active_host]'s
    [cwd]) when it may not exist yet: whether to fail, ask or create it, and
    the [$make_dir] host operation that creates it on the tool host. *)

module If_missing : sig
  type t =
    | Fail (** a missing directory is an error (the default) *)
    | Ask
    (** a missing directory that could be created is answered with
        [Missing], changing nothing, so that the frontend can ask *)
    | Create (** created with its parents ([mkdir -p]) *)
  [@@deriving sexp_of, equal]

  (** The [if_missing] parameter: ["fail"], ["ask"] or ["create"]. *)
  val of_param : string option -> t Or_error.t
end

module Outcome : sig
  type t =
    | Changed of
        { cwd : string
        ; created : bool
        }
    | Missing of string (** the absolute path that would be created *)
  [@@deriving sexp_of, equal]

  (** [{"cwd": ..., "created": ...}] or [{"missing": ...}] *)
  val to_json : t -> Json.t
end

(** [$make_dir {path, check}]: creates [path] (relative to the cwd, [~/]
    expanded) with its parents and answers its real path; an existing
    directory is fine. With [check], only says whether it could be created:
    the absolute path, or why not (a file in the way, no permission). *)
val op : string

val args : path:string -> check:bool -> Json.t
val run : cwd:string -> check:bool -> string -> Tool.Result.t

(** A host predating [$make_dir] answered [result]: what to do instead. *)
val older_host : Tool.Result.t -> host:string -> path:string -> string option
