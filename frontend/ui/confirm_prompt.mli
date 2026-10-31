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

(** e.g. [Run bash: make test?] *)
val title : P.Pending.Confirm.t -> string

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
