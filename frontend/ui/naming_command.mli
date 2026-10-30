open! Core
open! Import

(** [/naming], shared by the terminal and browser frontends: [/naming] shows
    which model names sessions and subagents, [/naming model MODEL|off] sets
    the config's [naming_model] (the backend resolves the name as [/model]
    does; [off]: [generate_model], else the session's model). *)

module Action : sig
  type t =
    | Show
    | Set_model of string option (** [naming_model] *)
    | Error of string (** what was wrong, with the usage *)
  [@@deriving sexp_of, equal]
end

val usage : string
val parse : string -> Action.t

(** [set_config]'s fields for [Set_model]. *)
val fields : Action.t -> (string * P.Json.t) list

(** [{config with naming_model}] for [Set_model]. *)
val apply : Action.t -> P.Config.t -> P.Config.t

(** ["sessions and subagents are named by MODEL (...)"]. *)
val summary : P.Config.t -> string

(** What [rest] (all that follows [/naming ], without leading blanks)
    completes, like {!Compaction_command.complete}: [model], then [off] or
    [models] (their keys). *)
val complete
  :  models:P.Model.t list
  -> string
  -> (int * (string * string) list) option
