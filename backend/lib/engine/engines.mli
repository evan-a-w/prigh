open! Core
open! Import

(** What a backend deploys: its engines and, when the composer is one of
    them, the adapters it may drive. Set once per backend
    ([prigh serve -engines harness,composer -adapters codex,claude]) and
    shared by all its namespaces, so users, settings and logins are the same
    whichever engine a session runs on. *)
type t = private
  { engines : Engine_id.t list
    (** never empty, no duplicates; the first is the default for new
        sessions *)
  ; adapters : Adapter_id.t list
    (** the composer's, the first its default; empty without the composer *)
  ; acp_agents : Acp_agent.t list
    (** ACP agents given with their command ([-adapters acp:name=command]) *)
  }
[@@deriving sexp_of, equal]

(** The harness alone: what a backend runs unless told otherwise. *)
val default : t

(** The adapters a composer drives when none are named: [codex], [claude]. *)
val default_adapters : Adapter_id.t list

val create
  :  engines:Engine_id.t list
  -> ?adapters:Adapter_id.t list (** default: {!default_adapters} *)
  -> ?acp_agents:Acp_agent.t list
  -> unit
  -> t Or_error.t

(** From [-engines] and [-adapters] (comma-separated; an ACP agent may come with
    its command, [acp:name=command args]), else
    [$PRIGH_ENGINES] and [$PRIGH_ADAPTERS], else {!default}. *)
val of_flags
  :  engines:string option
  -> adapters:string option
  -> getenv:(string -> string option)
  -> t Or_error.t

(** The ACP agent [acp:name] starts: one given with its command, else one
    prigh knows ({!Acp_agent.known}). *)
val acp_agent : t -> string -> Acp_agent.t option

(** Whether this backend can drive [adapter]: every one but an ACP agent
    with no known command. A backend asked for one fails at startup with
    {!check_build}'s error. *)
val drivable : t -> Adapter_id.t -> bool

val check_build : t -> unit Or_error.t
val engine_strings : t -> string list
val adapter_strings : t -> string list

(** The engine (and adapter) of a new session that names none. *)
val default_choice : t -> Engine_choice.t

(** A new session's engine from the request's [engine] and [adapter]:
    either may be left out (the default engine; the composer's first
    adapter); one this backend does not deploy is an error saying what it
    does. *)
val choose
  :  t
  -> engine:string option
  -> adapter:string option
  -> Engine_choice.t Or_error.t

(** Why a session on [choice] cannot run on this backend (it can still be
    read), with what to do; [None] when it can. [buildable] (default
    {!drivable}) is for tests. *)
val unavailable
  :  ?buildable:(Adapter_id.t -> bool)
  -> t
  -> Engine_choice.t
  -> string option
