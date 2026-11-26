open! Core
open! Import

(** The composer's [prigh] adapter: a composer session whose turns prigh's
    own harness runs, in the backend, with everything the harness can do.
    [harness ()] is the session's harness runner, taken afresh for each
    turn so that model and provider changes apply. *)
val runner : harness:(unit -> Runner.t) -> Runner.t
