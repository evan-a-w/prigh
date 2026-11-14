open! Core
open! Import

(** A tool confirmation as both frontends show it: what is asked and the
    answers, each with its key. *)

module Answer : sig
  type t =
    | Allow (** once *)
    | Always (** and allow the confirmation's rules for the session *)
    | Deny
    | Deny_with_reason (** the reason is typed next *)
  [@@deriving sexp_of, equal]
end

(** e.g. [Run bash: make test?], [Run on laptop: bash make test?], or [Let
    prigh control desk's screen, mouse and keyboard?] for computer use. *)
val title : P.Pending.Confirm.t -> string

(** A line under the title: for computer use, how long the consent lasts.
    For a call the approval model left to the user ([smart] mode), its
    reason ({!judged}). *)
val detail : P.Pending.Confirm.t -> string option

(** ["MODEL left this to you: REASON"]: why the approval model held a call
    for the user. *)
val judged : model:string -> reason:string -> string

(** ["allowed by MODEL: REASON"]: the note on a call the approval model let
    run ([tool_allowed]). *)
val allowed : model:string -> reason:string -> string

(** The answers offered, with their keys ([y], [a], [n], [r]) and labels;
    [Always] only when the confirmation has rules, its label naming them. *)
val answers : P.Pending.Confirm.t -> (Answer.t * string * string) list

(** What the reason prompt asks. *)
val reason_question : string

(** [tool_confirm_respond]'s parameters; [reason] only with [Deny]. *)
val params
  :  P.Pending.Confirm.t
  -> Answer.t
  -> reason:string option
  -> (string * P.Json.t) list

(** The note for the transcript once answered. *)
val acknowledgement : P.Pending.Confirm.t -> Answer.t -> string
