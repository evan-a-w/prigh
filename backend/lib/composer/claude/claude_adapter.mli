open! Core
open! Import

(** Claude Code, driven over [claude] stream-json and its control protocol
    ({!Claude_codec}). *)

include Adapter.S

(** [create ~backend_hosted:true ()] runs Claude Code on the backend with its
    built-in tools disabled ([--tools ""]) and all prigh tools bridged over the
    in-band SDK MCP server. *)
val create : ?backend_hosted:bool -> unit -> Adapter.t
