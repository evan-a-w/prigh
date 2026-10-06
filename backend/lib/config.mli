open! Core
open! Import

type t =
  { scoped_models : string list
  ; confirm_tools : bool
  ; default_model : string option
    (** model key for new sessions, set by [/change_default] *)
  ; default_thinking : Thinking.t option
  ; fallback_models : string list
    (** model keys, in order: when a model's usage runs out (or it has no
        credentials), a run hands over to the next one after it *)
  ; default_cwd : string option
    (** where new sessions start on the backend host *)
  }
[@@deriving sexp_of]

val default : t

(** The model for new sessions: [default_model], else the first of
    [fallback_models]. *)
val start_model : t -> string option

(** Reads [~/.prigh/config.json] under [home]. A missing file yields
    {!default}; unknown fields are ignored. *)
val load : home:string -> t Or_error.t

(** What is wrong with the file, each saying where and what to do: settings
    of the wrong type (which make {!load} fail) and unknown top-level fields.
    Unreadable or invalid JSON is {!read_fields}'s error, not repeated here. *)
val problems : home:string -> string list

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
