open! Core
open! Import

(** The [ask_session] tool: the model asks another saved session of its
    namespace a question. One tool-less model call ({!Btw.request}, asked
    as a session) over that session's context answers it, and only the
    answer comes back. The other session's file is read and never written:
    the call's spend goes to [record_spend], the asking session's. *)

val name : string

val create
  :  sessions_dir:(unit -> string)
  -> current:(unit -> string option) (** the calling session's id *)
  -> provider:Provider.t
  -> resolve_model:(string -> Model.t Or_error.t)
       (** the asked session's model, by the key it recorded *)
  -> current_model:(unit -> Model.t)
       (** the caller's, when the asked session's does not resolve *)
  -> record_spend:(usage:Usage.t -> cost_usd:float -> unit)
  -> unit
  -> Tool.t
