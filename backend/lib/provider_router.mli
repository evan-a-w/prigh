open! Core
open! Import

(** A provider that picks the backend from [request.model.provider] and
    resolves credentials (refreshing OAuth tokens as needed) on every request,
    so logging in or out takes effect immediately. Missing credentials are
    reported as an [Error] stop reason naming the [/login] command. Custom
    providers are looked up in [models] on every request; one without a key
    is called without authorisation (local servers). *)

val create
  :  env:Env.t
  -> ?timeout:Time_ns.Span.t
  -> ?getenv:(string -> string option)
  -> ?models:Model_registry.t
  -> store:Auth_store.t
  -> unit
  -> Provider.t

val not_configured_message : Provider_id.t -> string
