open! Core

(** [prigh-tui -detached]: the tmux command that starts the TUI in a new,
    detached tmux session, so that the shell returns at once. *)

type t =
  { tmux : string (** the tmux executable *)
  ; session : string
  ; args : string list (** tmux's arguments: [new-session -d ...] *)
  }
[@@deriving sexp_of]

(** [$PRIGH_TMUX] if set and non-empty, else [tmux] (on [PATH]). *)
val tmux : prigh_tmux:string option -> string

(** Replaces what tmux rejects or expands in a session name ([.], [:], [#],
    spaces, ...) with [_]. *)
val sanitise : string -> string

(** [prigh-<basename of dir>], sanitised; [prigh] for [/]. *)
val base_name : dir:string -> string

(** [base], else the first of [base-2], [base-3], ... that does not
    [exist]. *)
val free_name : string -> exists:(string -> bool) -> string

(** The variables of [environment] ([NAME=VALUE], as from
    [Unix.environment]) passed on to the session, which a running tmux
    server would not otherwise give it: [PATH], [HOME], the user and shell,
    every [PRIGH_*], the locale ([LANG], [LC_*]), [EDITOR]/[VISUAL],
    [XDG_*], temporary and tmux directories, TLS certificates, proxies and
    provider keys ([*_API_KEY], [ANTHROPIC_OAUTH_TOKEN]). Never [TMUX] or
    [TERM]: tmux sets those for the pane. *)
val environment : string list -> (string * string) list

(** The session for [command] (the TUI and its arguments), named [name]
    (sanitised) or after [dir] by {!base_name}, made free with
    {!free_name}, starting in [dir]. A failing [command] leaves the pane
    open until Enter, so that its error can be read. *)
val create
  :  tmux:string
  -> exists:(string -> bool)
  -> ?name:string
  -> environment:string list
  -> dir:string
  -> command:string list
  -> unit
  -> t

(** What to print: the session and how to attach to it ([switch-client]
    from inside tmux). *)
val message : t -> inside_tmux:bool -> string
