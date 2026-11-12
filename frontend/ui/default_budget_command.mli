open! Core
open! Import

(** [/default-budget], shared by the terminal and browser frontends:
    [/default-budget] shows the cost budget of sessions that set none with
    [/budget], [/default-budget DOLLARS|off] sets the config's
    [max_budget_usd]. *)

module Action : sig
  type t =
    | Show
    | Set of float option (** dollars; [None]: no budget *)
    | Error of string (** what was wrong, with the usage *)
  [@@deriving sexp_of, equal]
end

val usage : string

(** [5], [$2.50], [off]. *)
val parse : string -> Action.t

(** [set_config]'s fields for [Set]. *)
val fields : Action.t -> (string * P.Json.t) list

(** [{config with max_budget_usd}] for [Set]. *)
val apply : Action.t -> P.Config.t -> P.Config.t

(** ["$5.00"] *)
val dollars : float -> string

(** ["sessions without their own /budget stop at $5.00 (...)"] *)
val summary : P.Config.t -> string
