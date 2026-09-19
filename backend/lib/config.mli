open! Core
open! Import

type t =
  { scoped_models : string list
  ; confirm_tools : bool
  }
[@@deriving sexp_of]

val default : t

(** Reads [~/.prigh/config.json] under [home]. A missing file yields
    {!default}; unknown fields are ignored. *)
val load : home:string -> t Or_error.t

(** Writes [~/.prigh/config.json], creating parent directories, as pretty
    JSON. *)
val save : home:string -> t -> unit Or_error.t

val to_json : t -> Json.t
val of_json : Json.t -> t Or_error.t
