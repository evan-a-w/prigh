open! Core

(** Mounts the frontend in the page: connects to the backend named by the
    [?backend=] query parameter or this page's origin ([/ws]), sends [hello]
    (with the saved user name and password ({!Login}) and [?session=]/[?name=])
    and runs the shared Bonsai component in [#app]. Shows a connect form instead
    when the backend cannot be reached. The page's [?session=] follows the
    session, so a reload rejoins it. A button opens a terminal on the backend
    ({!Terminal_panel}); another (and [/signout]) forgets the saved login and
    reloads to the connect form. *)
val run : unit -> unit

module For_testing : sig
  val choose_backend : query:string option -> same_origin:string -> string

  val href_with_backend
    :  pathname:string
    -> search:string
    -> backend:string
    -> string

  val terminal_url
    :  backend:string
    -> user:string option
    -> token:string option
    -> session:string option
    -> string

  val with_query_param : search:string -> string -> string -> string
  val without_query_param : search:string -> string -> string

  (** The localStorage key of the prompt history: per token, as each token is a
      separate namespace. *)
  val history_key : token:string option -> string
end
