open! Core
open! Import

type t =
  { scoped_models : string list
  ; confirm_tools : bool
  ; default_model : string option
    (** model key for new sessions, set by [/change_default] *)
  ; default_thinking : Thinking.t option
  }
[@@deriving sexp_of]

val default : t

(** Reads [~/.prigh/config.json] under [home]. A missing file yields
    {!default}; unknown fields are ignored. *)
val load : home:string -> t Or_error.t

(** Writes [~/.prigh/config.json], creating parent directories, as pretty
    JSON. Fields it does not own (such as [providers]) are kept. *)
val save : home:string -> t -> unit Or_error.t

(** The file's top-level fields ([[]] when it is missing), for the parts of
    the file other modules own. *)
val read_fields : home:string -> (string * Json.t) list Or_error.t

(** Replaces the file atomically. *)
val write_fields : home:string -> (string * Json.t) list -> unit Or_error.t

val path : home:string -> string
val to_json : t -> Json.t
val of_json : Json.t -> t Or_error.t
