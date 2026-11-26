open! Core
open! Import

(** The models of composer sessions run by an external harness (Codex,
    Claude Code, an ACP agent). They are recorded as [ADAPTER/MODEL]
    ([codex/gpt-5], [acp:antigravity/gemini-3-pro]) under the provider
    [Custom ADAPTER], always the real model the harness listed (its default
    first); only before it listed them, or when it lists none, is the model a
    placeholder ([default]: the harness picks). Such a
    model has no price of prigh's: its cost is what the harness reported,
    recorded as spend. *)

(** [ADAPTER/MODEL]. *)
val name : adapter:string -> string option -> string

(** Whether a recorded model name is an external harness's. *)
val is_external : string -> bool

(** The placeholder's id: the harness picks. *)
val default_id : string

(** A model of [adapter]'s, as the session's [Model.t]: free, with the
    context window and thinking levels the harness gave (when it did). Without
    a window, its family's ({!known_window}); else, for Codex and Claude Code,
    200k, and for an ACP agent 0: unknown. *)
val model
  :  adapter:string
  -> ?name:string
  -> ?context_window:int
  -> ?thinking_levels:Thinking.Level.t list
  -> ?can_disable_thinking:bool
  -> ?images:bool
  -> string
  -> Model.t

(** The context window of a model family known by a word of [id] or [name]
    (Gemini, Claude, GPT-OSS). *)
val known_window : id:string -> name:string option -> int option

(** [adapter]'s placeholder ({!default_id}) before it listed its models,
    named as pending. *)
val pending : adapter:string -> Model.t

(** [adapter]'s placeholder ({!default_id}) when it lists no models. *)
val unlisted : adapter:string -> Model.t

(** Whether [m] is a placeholder, not a model the harness listed. *)
val is_placeholder : Model.t -> bool

(** Whether [model] is one of [adapter]'s. *)
val of_adapter : adapter:string -> Model.t -> bool

(** The id to send the harness: [None] for a placeholder. *)
val native_id : Model.t -> string option

(** The model the session recorded as [key] ([ADAPTER/ID]) among
    [models], else made from the key ([default], recorded before prigh named
    the harness's default: the first of [models], [None] before they are
    listed); [None] when [key] is not [adapter]'s. *)
val restore : adapter:string -> models:Model.t list -> string -> Model.t option

(** Whether a model's name ([Opus 5 (1M context)]) and an id a harness
    replied under ([claude-opus-5-20260101]) are the same model: equal once
    lowercased with [claude-] dropped, or one is the other with a date or a
    bracketed note added ([sonnet-4] is not [sonnet-4-5]). *)
val same_model : string -> string -> bool
