open! Core
open! Import

(** The models a namespace can use: the built-in {!Model.all} followed by the
    models of its custom providers ([config.json]'s [providers]), each from
    the provider's [GET {base_url}/models] (cached in memory) merged with the
    config's per-model overrides.

    Fetches run at creation (in the background), after [/login] of a custom
    provider, when a provider appears or changes in the config, and on
    {!refresh}. Problems (bad config entries, failed fetches) are kept in
    {!problems} and announced to {!subscribe}rs, each saying what to fix. *)

type t

val create
  :  env:Env.t
  -> sw:Switch.t
  -> ?timeout:Time_ns.Span.t (** for [GET /models]; default 10 s *)
  -> ?auto_fetch:bool
       (** fetch new and changed providers in the background (default);
           otherwise only {!refresh} fetches *)
  -> home:string
  -> store:Auth_store.t
  -> getenv:(string -> string option)
  -> unit
  -> t

(** Only the built-in models (tests, scripted runs). *)
val builtin : unit -> t

val home : t -> string option

(** Rereads [config.json]: providers added or changed by hand are fetched in
    the background, removed ones disappear. [listed] is a provider's fresh
    model list (the login flow just fetched it). *)
val reload : ?listed:string * Custom_provider.Listed_model.t list -> t -> unit

val providers : t -> Custom_provider.t list
val find_provider : t -> string -> Custom_provider.t option
val models : t -> Model.t list

(** Like {!Model.find_in}; a key naming a custom provider resolves even when
    the server did not list the id (sessions and config may name models that
    a failed or pending fetch has not returned). *)
val find : t -> string -> Model.t option

(** Like {!Model.resolve_in}; [<custom>/<id>] is accepted for an unlisted id
    only while that provider's list is unknown (not fetched yet, or failed). *)
val resolve : t -> string -> Model.t Or_error.t

(** [GET {base_url}/models]. Errors say what failed (connection, timeout, the
    HTTP status and the server's message). *)
val fetch_models
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?timeout:Time_ns.Span.t
  -> Custom_provider.t
  -> key:string option
  -> Custom_provider.Listed_model.t list Or_error.t

(** Fetches with the provider's stored or environment key. *)
val fetch
  :  t
  -> ?cancel:Cancellation.t
  -> Custom_provider.t
  -> Custom_provider.Listed_model.t list Or_error.t

(** Records a list fetched elsewhere (the login flow). *)
val set_listed : t -> string -> Custom_provider.Listed_model.t list -> unit

(** Fetches the named providers (default: all) now, in this fiber. *)
val refresh : t -> ?only:string list -> unit -> unit

val problems : t -> string list
val subscribe : t -> f:(string -> unit) -> unit
