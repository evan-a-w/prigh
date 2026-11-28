open! Core
open! Import

(** Codex, driven over [codex app-server] ({!Codex_codec}). *)

include Adapter.S

(** [create ~backend_hosted:true ()] runs Codex on the backend in read-only
    sandbox mode, declining built-in local tool approvals so it uses prigh's
    bridged MCP tools instead. *)
val create : ?backend_hosted:bool -> unit -> Adapter.t
