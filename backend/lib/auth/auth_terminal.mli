open! Core
open! Import

(** An [Auth_interaction.t] for the CLI: notices go to stderr, answers are
    read from stdin (without echo for secrets), auth URLs are opened with the
    platform's browser launcher. *)

val create
  :  env:Env.t
  -> sw:Switch.t
  -> ?open_urls:bool
  -> unit
  -> Auth_interaction.t

val open_browser : env:Env.t -> sw:Switch.t -> string -> unit
