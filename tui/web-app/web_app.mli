open! Core

(** Mounts the frontend in the page: connects to the backend named by the
    [?backend=] query parameter, the saved setting or this page's origin
    ([/ws]), sends [hello] (with [?token=], [?session=], [?name=]) and runs the
    shared Bonsai component in [#app]. Shows a connect form instead when the
    backend cannot be reached. *)
val run : unit -> unit
