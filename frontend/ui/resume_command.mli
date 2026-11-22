open! Core
open! Import

(** [/resume], shared by the terminal and browser frontends: [/resume] shows
    whether a run waits for a usage limit to reset and whether runs resume
    by themselves, [/resume now] retries at once, [/resume cancel] stays
    stopped, and [/resume on|off] sets the config's [resume_on_limit_reset]
    (off also cancels a scheduled resume). *)

module Action : sig
  type t =
    | Show
    | Now
    | Cancel
    | Set of bool (** [resume_on_limit_reset] *)
    | Error of string (** what was wrong, with the usage *)
  [@@deriving sexp_of, equal]
end

val usage : string
val parse : string -> Action.t

(** [set_config]'s fields for [Set]. *)
val fields : Action.t -> (string * P.Json.t) list

(** [{config with resume_on_limit_reset}] for [Set]. *)
val apply : Action.t -> P.Config.t -> P.Config.t

(** ["runs stopped by a usage limit resume by themselves when it resets
    (/resume off ...)"] *)
val summary : P.Config.t -> string
