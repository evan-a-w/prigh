open! Core
open! Import

(** A model's built-in computer-use tool, which prigh's [computer] tool is
    offered as when the model has one ([Tool_spec.Native.Computer]); its
    calls are translated to and from {!Computer_action}'s JSON
    ({!Computer_native}), so the session, the tool host and the frontends
    only ever see that. *)
type t =
  | Anthropic_toolset
  (** [computer_toolset_20260801] (Claude 5.5 and later, no beta header):
      one [tool_use] per action, named after the action, with
      [toolset_name: "computer"] *)
  | Anthropic_tool of
      { version : string (** e.g. [computer_20251124] *)
      ; beta : string (** e.g. [computer-use-2025-11-24] *)
      ; zoom : bool (** sends [enable_zoom] *)
      } (** the [computer] tool with [input.action], behind a beta header *)
  | Openai
  (** the Responses API's [computer] tool: [computer_call] items carrying
      a batch of [actions], answered by one [computer_call_output] with a
      screenshot *)
[@@deriving sexp_of, equal]

(** What the catalogue's models have, as documented by their provider. *)
val of_model : Provider_id.t -> string -> t option

(** A short name, stored with a call's original native form: ["anthropic"],
    ["anthropic_toolset"], ["openai"]. Calls in the same form are replayed
    as the model sent them. *)
val form : t -> string
