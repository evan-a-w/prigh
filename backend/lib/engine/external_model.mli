open! Core
open! Import

(** The models of composer sessions run by an external harness (Codex,
    Claude Code, an ACP agent). They are recorded as [ADAPTER/MODEL]
    ([codex/gpt-5], [claude/default], [acp:opencode/default]) under the
    provider [Custom ADAPTER]; [default] is the harness's own choice. Such a
    model has no price of prigh's: its cost is what the harness reported,
    recorded as spend. *)

(** [ADAPTER/MODEL]. *)
val name : adapter:string -> string option -> string

(** Whether a recorded model name is an external harness's. *)
val is_external : string -> bool

(** The id that means the harness's own default model. *)
val default_id : string

(** A model of [adapter]'s, as the session's [Model.t]: free, with the
    context window and thinking levels the harness gave (when it did). *)
val model
  :  adapter:string
  -> ?name:string
  -> ?context_window:int
  -> ?thinking_levels:Thinking.Level.t list
  -> ?can_disable_thinking:bool
  -> ?images:bool
  -> string
  -> Model.t

(** [adapter]'s default model ({!default_id}). *)
val default : adapter:string -> Model.t

(** Whether [model] is one of [adapter]'s. *)
val of_adapter : adapter:string -> Model.t -> bool

(** The id to send the harness: [None] for its default. *)
val native_id : Model.t -> string option

(** The model the session recorded as [key] ([ADAPTER/ID]) among
    [models], else made from the key; [None] when [key] is not [adapter]'s. *)
val restore : adapter:string -> models:Model.t list -> string -> Model.t option

(** Whether a model's name ([Opus 5 (1M context)]) and an id a harness
    replied under ([claude-opus-5-20260101]) are the same model: equal once
    lowercased with [claude-] dropped, or one is the other with a date or a
    bracketed note added ([sonnet-4] is not [sonnet-4-5]). *)
val same_model : string -> string -> bool
