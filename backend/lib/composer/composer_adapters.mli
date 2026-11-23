open! Core
open! Import

(** The adapter driving [id]'s harness on [engines]' backend (an ACP agent's
    command comes from there), or why there is none. [Prigh] has none: it is
    {!Prigh_adapter}, run in the backend. *)
val find : Engines.t -> Adapter_id.t -> Adapter.t Or_error.t
