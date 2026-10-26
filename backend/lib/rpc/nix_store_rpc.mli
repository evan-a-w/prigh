open! Core
open! Import

(** The Nix store's RPC methods, for superusers only ({!Nix_store}):

    - [nix_store_status {}]: {!Nix_store_status.to_json}.
    - [nix_store_gc {delete_generations?: days | "all"}]: deletes the
      profile generations older than that many days ([0] or ["all"]: all
      but the current ones), then collects garbage. While it runs, the
      caller gets [nix_gc_progress] events ({!Nix_gc_progress.to_json});
      the answer is {!Nix_gc_report.to_json}. *)

val methods : string list

(** [None] for other methods, and for everyone but superusers, so that to
    them these methods do not exist. [signed_in] is who the connection signed
    in as (not whom a superuser acts as). *)
val dispatch
  :  Nix_store.t option
  -> signed_in:User_access.Signed_in.t option
  -> send:(Json.t -> unit)
  -> meth:string
  -> params:Json.t
  -> Json.t Or_error.t option
