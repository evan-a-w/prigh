open! Core

(** Mounts the frontend in the page: connects to the backend named by the
    [?backend=] query parameter or this page's origin ([/ws]), sends [hello]
    (with the saved token and [?session=]/[?name=]) and runs the shared Bonsai
    component in [#app]. Shows a connect form instead when the backend cannot be
    reached. *)
val run : unit -> unit

module For_testing : sig
  val choose_backend : query:string option -> same_origin:string -> string

  val href_with_backend
    :  pathname:string
    -> search:string
    -> backend:string
    -> string
end
