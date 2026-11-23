open! Core
open! Import

(** The adapter driving [id]'s harness, or why this build has none. *)
val find : Adapter_id.t -> Adapter.t Or_error.t
