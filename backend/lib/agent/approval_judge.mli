open! Core
open! Import

(** The approval model of the [smart] approval mode: one request per tool
    call that would otherwise ask the user, answering whether it may run
    without asking ([Allow]) or the user should decide ([Ask]). It never
    refuses a call by itself.

    The model sees the call (tool, arguments, host, directory), what the
    user asked for (their own messages, the session's goal and plan) and,
    for a subagent, the task it was given; never tool results, fetched
    pages or the assistant's text, so that text the agent read cannot talk
    it into allowing anything. It answers by calling its one tool,
    [verdict]; anything else is a failure, and a failure asks the user. *)

module Verdict : sig
  type t =
    | Allow of string (** the reason, one line *)
    | Ask of string
  [@@deriving sexp_of, equal]
end

module Input : sig
  type t =
    { tool : string
    ; arguments : Json.t
    ; host : string (** the tool host's name *)
    ; elsewhere : bool (** on a host other than the session's ([on_host]) *)
    ; cwd : string
    ; requests : string list
      (** the user's messages, oldest first (see {!requests}) *)
    ; task : string option
      (** a subagent's task, written by the agent that started it *)
    ; goal : string option
    ; plan : string option
    ; plan_mode : bool
    }
  [@@deriving sexp_of]
end

(** The user's own messages in [messages], as [Input.requests] wants them:
    the text they wrote (and the goals, reviews and [/init]s they started),
    without the harness's notes, attachments, compaction summaries, command
    output or other sessions' questions; the last ten, each cut to 4000
    bytes. *)
val requests : Message.t list -> string list

(** The system prompt: the guidance the model judges by. *)
val system_prompt : string

(** The one user message: the call and its context, in tagged sections. *)
val prompt : Input.t -> string

(** The lowest thinking the model takes: off when it can be. *)
val thinking : Model.t -> Thinking.t

(** The request for [input]; [cache_key] groups a session's. *)
val request
  :  ?cache_key:string
  -> model:Model.t
  -> Input.t
  -> Provider.Request.t

(** The verdict in a reply: its [verdict] tool call (else a JSON object in
    its text) with [decision] [allow] or [ask] and a non-empty [reason];
    otherwise an error saying what was wrong. *)
val parse : Message.Assistant.t -> (Verdict.t, string) Result.t

(** Judges [input], returning the verdict (or why there is none) and the
    reply's usage. *)
val judge
  :  ?cache_key:string
  -> provider:Provider.t
  -> model:Model.t
  -> cancel:Cancellation.t
  -> Input.t
  -> (Verdict.t, string) Result.t * Usage.t

(** The [purpose] of its [Session.Spend] entries: ["approval"]. *)
val purpose : string
