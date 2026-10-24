open! Core
open! Import

(** The interactive flows for custom providers ([/login custom], [/logout
    <name>]), through {!Auth_interaction} prompts so that every frontend can
    run them.

    Login asks for the name (unless editing [name]), the base URL, the API
    style and the API key, each prefilled with the current value when
    editing; an invalid answer is asked again with the error and the answer
    prefilled. It then lists [GET {base_url}/models]; if that fails it offers
    to save anyway, change the settings, or cancel. Nothing is written until
    the end: the definition goes to [config.json], the key to [auth.json],
    and the registry learns the models. *)

val login
  :  env:Env.t
  -> models:Model_registry.t
  -> store:Auth_store.t
  -> getenv:(string -> string option)
  -> ?name:string
  -> Auth_interaction.t
  -> Custom_provider.t Or_error.t

module Logout : sig
  type t =
    | Key_removed
    | Provider_removed
    | Kept (** the user chose to keep everything *)
  [@@deriving sexp_of]
end

(** Asks whether to remove only the key or the provider definition too. *)
val logout
  :  models:Model_registry.t
  -> store:Auth_store.t
  -> string
  -> Auth_interaction.t
  -> Logout.t Or_error.t
