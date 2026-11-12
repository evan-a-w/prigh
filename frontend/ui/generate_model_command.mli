open! Core
open! Import

(** [/generate-model], shared by the terminal and browser frontends:
    [/generate-model] shows which model drafts commit messages, pull requests
    and branch names (✦ Generate), [/generate-model MODEL|off] sets the
    config's [generate_model] (the backend resolves the name as [/model]
    does; [off]: the session's model). *)

module Action : sig
  type t =
    | Show
    | Set_model of string option (** [generate_model] *)
    | Error of string (** what was wrong, with the usage *)
  [@@deriving sexp_of, equal]
end

val usage : string
val parse : string -> Action.t

(** [set_config]'s fields for [Set_model]. *)
val fields : Action.t -> (string * P.Json.t) list

(** [{config with generate_model}] for [Set_model]. *)
val apply : Action.t -> P.Config.t -> P.Config.t

(** ["commit messages, pull requests and branch names are drafted by MODEL
    (...)"]. *)
val summary : P.Config.t -> string

(** What [rest] (all that follows [/generate-model ], without leading blanks)
    completes, like {!Naming_command.complete}: [off], then [models] (their
    keys). *)
val complete
  :  models:P.Model.t list
  -> string
  -> (int * (string * string) list) option
