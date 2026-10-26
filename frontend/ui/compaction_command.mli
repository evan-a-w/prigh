open! Core
open! Import

(** [/compaction], shared by the terminal and browser frontends: [/compaction]
    shows when conversations are compacted and which model summarises them,
    [/compaction at TOKENS|off] sets the config's [compact_at_tokens] (the
    cap under 80% of the window; [off]: 80% only) and [/compaction model
    MODEL|off] its [compaction_model] (the backend resolves the name as
    [/model] does; [off]: the conversation's model), and [/compaction mask
    on|off] its [mask_observations]. *)

module Action : sig
  type t =
    | Show
    | Set_at of int (** [compact_at_tokens]; [0]: off *)
    | Set_model of string option (** [compaction_model] *)
    | Set_mask of bool (** [mask_observations] *)
    | Error of string (** what was wrong, with the usage *)
  [@@deriving sexp_of, equal]
end

val usage : string
val parse : string -> Action.t

(** [set_config]'s fields for [Set_at], [Set_model] and [Set_mask]. *)
val fields : Action.t -> (string * P.Json.t) list

(** [{config with ...}] for [Set_at], [Set_model] and [Set_mask]. *)
val apply : Action.t -> P.Config.t -> P.Config.t

(** ["compaction: at 200k tokens, or 80% of the model's window if that is
    less; summaries by the session's model; observation masking on
    (...)"] *)
val summary : P.Config.t -> string

(** ["200k"], ["1.5M"], ["1234"] *)
val tokens : int -> string

(** What [rest] (all that follows [/compaction ], without leading blanks)
    completes, like {!Remind_command.complete}: the subcommands, [off], or
    [models] (their keys) after [model], [on] and [off] after [mask]. *)
val complete
  :  models:P.Model.t list
  -> string
  -> (int * (string * string) list) option
