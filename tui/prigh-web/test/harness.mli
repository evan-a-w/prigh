open! Core
open Prigh_web

(** Drives [App.update] like the page does and shows what the user sees. *)

type t

(** Started and connected: [state] (a fresh session, default [state_json ()]),
    the models ([models_json]), the providers ([auth_json]), the config
    ([config_json]) and [sessions] answered, and the clock at [now]. The startup commands are printed only
    when [verbose]. *)
val create : ?verbose:bool -> ?sessions:string -> ?state:string -> unit -> t

val model : t -> App.Model.t

(** Applies an action and prints the commands it issued. *)
val act : t -> App.Action.t -> unit

(** Runs [f] without printing the commands issued. *)
val quiet : t -> (unit -> unit) -> unit

(** A key press as the page sees it (in the editor with the caret at the end,
    unless [target]): prints the action [Keys.handle] chose, and applies it. *)
val key
  :  ?shift:bool
  -> ?alt:bool
  -> ?ctrl:bool
  -> ?meta:bool
  -> ?selection:bool (** text is selected *)
  -> ?code:string (** [KeyboardEvent.code]: [Key<letter>] for letters *)
  -> ?target:Keys.Target.t
  -> t
  -> string
  -> unit

(** Typing: the editor's text, the caret at its end. *)
val type_ : t -> string -> unit

(** An event from the backend, decoded from its JSON. *)
val event : t -> string -> unit

(** Answers the oldest unanswered [Rpc] command whose method is [method_]. *)
val reply : t -> string -> string -> unit

(** Fails the oldest unanswered [Rpc] command whose method is [method_]. *)
val fail : t -> string -> string -> unit

(** The page as indented HTML; [selector] picks part of it. *)
val show : ?selector:string -> t -> unit

(** The page's visible text: a line per block element, buttons and links in
    (parentheses; their title when they only have an icon), fields as
    [\[value\]]. *)
val text : ?selector:string -> t -> unit

(** The tag and class of each element [selector] matches. *)
val elements : selector:string -> t -> unit

(** A state as JSON, with [fields] overriding the defaults. *)
val state_json : ?fields:(string * Jsonaf.t) list -> unit -> string

val session_json
  :  ?name:string
  -> ?description:string
  -> ?first_prompt:string
  -> ?cwd:string
  -> ?updated_at:string
  -> ?messages:int
  -> ?live:bool
  -> ?running:bool
  -> string
  -> string

(** A model as [list_models] and states give it, keyed [provider/id]. *)
val model_json
  :  ?thinking:bool
  -> provider:string
  -> id:string
  -> name:string
  -> unit
  -> string

(** GPT-6 (openai), Claude Opus 5.5 and Claude Sonnet 5 (anthropic), and
    DeepSeek Chat (deepseek, no thinking). *)
val models_json : string

(** No scoped models, no confirmation, no defaults. *)
val config_json : string

(** Anthropic logged in (oauth); OpenAI and DeepSeek not. *)
val auth_json : string

(** 2026-10-05 10:00 UTC. *)
val now : Time_ns.t
