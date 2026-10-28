open! Core
open! Import

(** Spans of where a session's time goes: model requests, tool calls and the
    waits between them (see ARCHITECTURE.md, "Tracing").

    The tracer and the current span are fiber-local ({!Eio.Fiber.with_binding}):
    a span opened in a fiber is the parent of those its code opens, also in
    fibers it forks (parallel tool calls, subagents, background tasks), so
    nothing threads a tracer through. Outside {!run}, every function here is a
    no-op. A span is handed to the tracer's [write] when it ends; it holds
    names, sizes, ids and timings, never message or tool content. *)

module Level : sig
  type t =
    | Off
    | Default (** a few spans a turn: model requests, tools, waits *)
    | Detailed
    (** also each streamed delta's wait and handling, request building:
        for performance work on prigh *)
  [@@deriving sexp_of, equal, compare, enumerate]

  val to_string : t -> string
  val of_string : string -> t Or_error.t

  (** [PRIGH_TRACE] ([off], [default] or [detailed]) when set to one of
      them, else [configured]. *)
  val effective : configured:t -> t
end

module Value : sig
  type t =
    | Int of int
    | Float of float
    | String of string
    | Bool of bool
  [@@deriving sexp_of, equal]

  val to_json : t -> Json.t
  val of_json : Json.t -> t option
  val to_string_hum : t -> string
end

module Status : sig
  type t =
    | Ok
    | Cancelled
    | Error
  [@@deriving sexp_of, equal]

  val to_string : t -> string
end

module Span : sig
  type t =
    { id : string
    ; parent : string option
    ; name : string
    ; track : string
      (** the agent it belongs to: [main], a subagent's id, a job's ([j1],
          [a1/j1]) *)
    ; wall : float (** wall-clock start, seconds since the epoch *)
    ; start : float (** monotonic start, seconds *)
    ; stop : float (** monotonic end, seconds *)
    ; status : Status.t
    ; attrs : (string * Value.t) list
    }
  [@@deriving sexp_of]

  val duration : t -> float
  val attr : t -> string -> Value.t option
  val to_json : t -> Json.t
  val of_json : Json.t -> t Or_error.t
end

module Clock : sig
  type t =
    { mono : unit -> float (** seconds *)
    ; wall : unit -> float (** seconds since the epoch *)
    }

  val of_env : Env.t -> t
end

type t

(** [id_prefix] (default: from the time and a count of tracers) keeps the ids of spans written by
    different tracers to one file apart. *)
val create
  :  ?id_prefix:string
  -> clock:Clock.t
  -> level:Level.t
  -> write:(Span.t -> unit)
  -> unit
  -> t

val level : t -> Level.t
val set_level : t -> Level.t -> unit

(** Runs [f] with [t] as the fiber's tracer, outside any span, on [track]
    (default [main]). *)
val run : t -> ?track:string -> (unit -> 'a) -> 'a

(** Whether spans of that level are recorded here: [detailed] (default
    false) asks about {!Level.Detailed}. *)
val enabled : ?detailed:bool -> unit -> bool

(** The current tracer's monotonic time, when there is one. *)
val now : unit -> float option

(** A span being recorded. *)
module Open : sig
  type t
end

(** Opens a span, a child of the current one or of its current {!phase}.
    [detailed] (default false) records it only at {!Level.Detailed}.
    [root] makes it a root; [under] the child of the closest enclosing span
    of that name (the current one's parent when there is none). [track]
    starts a new track (an agent of its own). [at] is its start (monotonic,
    default now; the wall-clock start is shifted to match). [phase] starts
    its first phase (see {!phase}). *)
val start
  :  ?detailed:bool
  -> ?root:bool
  -> ?under:string
  -> ?track:string
  -> ?at:float
  -> ?phase:string
  -> ?attrs:(string * Value.t) list
  -> string
  -> Open.t option

(** Ends [span] (and its current phase), adding [attrs]; a span ends once. *)
val finish : ?status:Status.t -> ?attrs:(string * Value.t) list -> Open.t option -> unit

(** Runs [f] with [span] as the current span. An exception ends the span
    [Cancelled] ([Eio.Cancel.Cancelled]) or [Error]. *)
val within : Open.t option -> (unit -> 'a) -> 'a

(** [start], [within] and [finish] around [f]. *)
val span
  :  ?detailed:bool
  -> ?root:bool
  -> ?under:string
  -> ?track:string
  -> ?phase:string
  -> ?attrs:(string * Value.t) list
  -> string
  -> (unit -> 'a)
  -> 'a

(** Sets attributes of the current span, or of the closest enclosing one
    named [under] (none when there is no such span). *)
val add_attrs : ?under:string -> (string * Value.t) list -> unit

val add_attrs_to : Open.t option -> (string * Value.t) list -> unit

(** In the closest enclosing span opened with [phase]: ends its current
    phase and starts a child [name] in its place, which spans opened
    meanwhile go under. Nothing outside such a span, or when [name] is its
    current phase already. *)
val phase : string -> unit

(** The name of that span's current phase. *)
val current_phase : unit -> string option

(** The current span's track ([main] outside any). *)
val track : unit -> string

(** A string attribute, at most [max] (default 120) bytes. *)
val short : ?max:int -> string -> Value.t
