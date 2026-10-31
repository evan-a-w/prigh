open! Core

(** What to say when running [git] on a tool host fails because there is no
    [git] on the PATH the host process started with. That PATH is fixed when
    prigh starts (for a service, often without the user's profile), so the
    message names it: installing git somewhere else does not help until it
    is on that PATH and prigh restarts. *)
val message : unit -> string
