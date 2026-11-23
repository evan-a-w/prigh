open! Core
open! Import

(** While the session cannot run on this backend (the state's engine's
    [unavailable]: its engine is not deployed here, or its adapter's CLI is
    missing or signed out on the tool host), a strip above the composer
    says why and what to do (the whole reason in the title), with
    "New session", which starts one on what this backend runs. Sending is
    disabled meanwhile. Nothing otherwise. *)

(** The reason, when the strip shows. *)
val reason : App.Model.t -> string option

val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
