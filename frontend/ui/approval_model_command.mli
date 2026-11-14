open! Core
open! Import

(** [/approval-model], shared by the terminal and browser frontends:
    [/approval-model] shows which model judges tool calls in the [smart]
    approval mode, [/approval-model MODEL|off] sets the config's
    [approval_model] (the backend resolves the name as [/model] does;
    [off]: the session's model). *)

module Action : sig
  type t =
    | Show
    | Set_model of string option (** [approval_model] *)
    | Error of string (** what was wrong, with the usage *)
  [@@deriving sexp_of, equal]
end

val usage : string
val parse : string -> Action.t

(** [set_config]'s fields for [Set_model]. *)
val fields : Action.t -> (string * P.Json.t) list

(** [{config with approval_model}] for [Set_model]. *)
val apply : Action.t -> P.Config.t -> P.Config.t

(** ["in smart mode, MODEL judges the tool calls that would ask
    (/approval-model MODEL|off)"]. *)
val summary : P.Config.t -> string

(** What [rest] (all that follows [/approval-model ], without leading
    blanks) completes, like {!Generate_model_command.complete}: [off], then
    [models] (their keys). *)
val complete
  :  models:P.Model.t list
  -> string
  -> (int * (string * string) list) option
