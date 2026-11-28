open! Core
open! Import

(** What one ACP agent does its own way, beyond the protocol. *)
module Quirks : sig
  (** How to read the [usage] a [session/prompt] answer carries (ACP leaves
      what its counts mean open). *)
  module Prompt_usage : sig
    type t =
      | Ignored (** unproven: the agent's [usage_update]s are enough *)
      | Gemini
      (** Antigravity's: [inputTokens] leaves out [cachedReadTokens], and
          [outputTokens] already counts [thoughtTokens] *)
    [@@deriving sexp_of, equal]
  end

  (** How a message reaches a turn that is running. *)
  module Steer : sig
    type t =
      | Between_turns (** it waits for the turn to end *)
      | Prompt
      (** Antigravity's: another [session/prompt] while one runs; the
          running one ends [end_turn] and the new one carries the turn on *)
      | Xai_interject
      (** Grok's [_x.ai/interject]: the model reads it at its next step,
          the running tool finishing first *)
    [@@deriving sexp_of, equal]
  end

  type t =
    { login : string option
      (** the command that logs it in, run on the tool host *)
    ; login_file : string option
      (** a file whose presence (never its contents) suggests it is logged
          in, [~] the host's home: Antigravity's token file, which it
          keeps only when the keyring is out of reach *)
    ; min_version : string option
      (** the oldest version in which [command] speaks ACP *)
    ; prompt_usage : Prompt_usage.t
    ; steer : Steer.t
    ; xai_extensions : bool
      (** Grok's [x.ai/*] requests and notifications (questions, plans,
          background tasks) *)
    }
  [@@deriving sexp_of, equal]

  (** An agent prigh knows nothing particular about. *)
  val none : t
end

(** An ACP agent the composer can drive ([acp:<name>]): the command that
    starts it speaking ACP on its stdio. *)
type t =
  { name : string (** as in [acp:<name>] *)
  ; display_name : string
  ; command : string list (** program and arguments, never empty *)
  ; quirks : Quirks.t
  }
[@@deriving sexp_of, equal]

(** The agents prigh knows how to start: [opencode] ([opencode acp]),
    [antigravity] ([agy acp]), [grok] ([grok agent stdio]), [gemini]
    ([gemini --experimental-acp]) and [goose] ([goose acp]). *)
val known : t list

val find_known : string -> t option

(** A [command args] or [name=command args] spec (from [-adapters acp:...]):
    the command is split on whitespace. Without [name=], the agent's name is
    inferred from the basename of the command's executable. With [name=], a
    known agent's name keeps its display name and quirks. *)
val of_spec : string -> t Or_error.t

(** Whether [version] (as [--version] prints it: ["1.2.3"]) is older than
    [than]; [false] when either is not dotted numbers. *)
val older : string -> than:string -> bool

(** What to do when the agent exited before starting, saying [stderr]: to
    update it, when it did not know the command that starts ACP and has a
    minimum version. *)
val start_hint : t -> stderr:string -> string option
