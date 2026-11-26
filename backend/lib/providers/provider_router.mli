open! Core
open! Import

(** A provider that picks the backend from [request.model.provider] and
    resolves credentials (refreshing OAuth tokens as needed) on every request,
    so logging in or out takes effect immediately. Missing credentials are
    reported as an [Error] stop reason naming the [/login] command. A
    request's [account] picks the login (default: the provider's default);
    the reply records the one that wrote it. Custom
    providers are looked up in [models] on every request; one without a key
    is called without authorisation (local servers). *)

val create
  :  env:Env.t
  -> ?timeout:Time_ns.Span.t
  -> ?getenv:(string -> string option)
  -> ?models:Model_registry.t
  -> ?member:Account_shares.Member.t
       (** the user of [prigh serve -tokens] whose requests these are: a
           request may name a login another user shares with them, and one
           naming none uses the first shared one when they have none of
           their own *)
  -> ?ledger:Account_ledger.t
       (** where each request on a login is recorded
           ({!Account_shares.record_request}; default: nowhere) *)
  -> store:Auth_store.t
  -> unit
  -> Provider.t

val not_configured_message : Provider_id.t -> string
