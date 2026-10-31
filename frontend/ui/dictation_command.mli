open! Core
open! Import

(** [/dictation], shared by the terminal and browser frontends: [/dictation]
    shows which model turns dictated speech into text, [/dictation model
    MODEL|off] sets the config's [dictation_model] ([provider/id]: an OpenAI
    or OpenAI-compatible provider's transcription model; [off]: the
    backend's default, {!P.Config.default_dictation_model}). *)

module Action : sig
  type t =
    | Show
    | Set_model of string option (** [dictation_model] *)
    | Error of string (** what was wrong, with the usage *)
  [@@deriving sexp_of, equal]
end

val usage : string
val parse : string -> Action.t

(** The transcription models offered: OpenAI's. Any other [provider/id]
    (a custom OpenAI-compatible provider's) is accepted as typed. *)
val suggestions : (string * string) list

(** [set_config]'s fields for [Set_model]. *)
val fields : Action.t -> (string * P.Json.t) list

(** [{config with dictation_model}] for [Set_model]. *)
val apply : Action.t -> P.Config.t -> P.Config.t

(** ["dictation is transcribed by MODEL (...)"]. *)
val summary : P.Config.t -> string

(** What [rest] (all that follows [/dictation ], without leading blanks)
    completes, like {!Naming_command.complete}: [model], then [off] or
    {!suggestions}. *)
val complete : string -> (int * (string * string) list) option
