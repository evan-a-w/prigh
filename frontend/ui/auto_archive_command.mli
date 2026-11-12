open! Core
open! Import

(** [/auto-archive], shared by the terminal and browser frontends:
    [/auto-archive] shows how long a session goes without activity before it
    is archived, [/auto-archive DAYS|off] sets the config's
    [archive_after_days] ([off] or [0]: never). Every client's session list
    follows at once. *)

module Action : sig
  type t =
    | Show
    | Set of float (** days; [0.]: never *)
    | Error of string (** what was wrong, with the usage *)
  [@@deriving sexp_of, equal]
end

val usage : string

(** The most days it takes: [365]. *)
val max_days : float

(** [7], [7d], [2.5 days], [off], [never]. *)
val parse : string -> Action.t

(** [set_config]'s fields for [Set]. *)
val fields : Action.t -> (string * P.Json.t) list

(** [{config with archive_after_days}] for [Set]. *)
val apply : Action.t -> P.Config.t -> P.Config.t

(** ["7 days"], ["1 day"], ["0.5 days"]. *)
val days : float -> string

(** ["sessions are archived after 7 days without activity (...)"] *)
val summary : P.Config.t -> string
