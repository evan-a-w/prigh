open! Core
open! Import

(** Translates the calls of models' built-in computer-use tools
    ({!Native_computer}) to and from the [computer] tool's own arguments
    ({!Computer_action}'s JSON). A native call is stored as those arguments,
    which the tool host runs and the frontends show whichever form the model
    used, plus:

    - ["native"]: the call as the model wrote it, with its form
      ([{"form": "anthropic_toolset", "name": "left_click", "input": {...}}],
      [{"form": "anthropic", "input": {...}}] or
      [{"form": "openai", "action": {...}}]), replayed unchanged to a model
      of the same form: the provider checks that earlier turns are what it
      sent (Anthropic's thinking blocks are bound to their prefix);
    - ["safety_checks"]: an OpenAI call's [pending_safety_checks], which
      its output acknowledges. A call carrying them needs approval in Ask
      mode, and its result says they were acknowledged. *)

module Safety_check : sig
  type t =
    { id : string
    ; code : string option
    ; message : string option
    }
  [@@deriving sexp_of]

  val to_json : t -> Json.t
end

(** The safety checks stored in a call's arguments. *)
val safety_checks : Json.t -> Safety_check.t list

(** The line a result starts with when its call had safety checks:
    ["OpenAI safety checks acknowledged: malicious_instructions: ..."]. *)
val safety_note : Safety_check.t list -> string

(** The arguments for a provider without the native tool: without
    ["native"] and ["safety_checks"]. *)
val generic : string -> string

(** {2 Anthropic} *)

(** The stored arguments of a [tool_use]: a toolset member [name] with its
    [input], or (not [toolset]) the [computer] tool's [input] with its
    [action]. *)
val of_anthropic : toolset:bool -> name:string -> Json.t -> string

(** The [tool_use] name and input to replay stored [arguments] as: the
    original when it was in the same form, otherwise translated. *)
val to_anthropic : toolset:bool -> string -> string * Json.t

(** {2 OpenAI} *)

(** The stored arguments of one action of a [computer_call]; the first of
    a call's actions carries its [pending_safety_checks]. [item_id]: the
    [computer_call] item's [id]. *)
val of_openai
  :  ?item_id:string
  -> ?safety_checks:Json.t list
  -> Json.t
  -> string

(** The action to replay stored [arguments] as, with the call's
    [pending_safety_checks] and its item's [id] if it had them. *)
val to_openai : string -> Json.t * Json.t list * string option
