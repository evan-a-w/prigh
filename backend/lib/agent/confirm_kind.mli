open! Core
open! Import

(** What a [Tool_confirm] asks, so that frontends word it right. *)

type t =
  | Tool (** whether one tool call may run *)
  | Judged of
      { model : string (** the approval model's key *)
      ; reason : string
        (** why it asks: its one-line reason, or why it gave no verdict *)
      }
  (** whether one tool call may run, which the approval model ([smart] mode,
      {!Approval_judge}) left to the user *)
  | Computer_use of { host : string }
  (** whether prigh may control the screen, mouse and keyboard of [host]
      (its name) until control ends ({!Computer_consent}); never offers
      "don't ask again" *)
[@@deriving sexp_of, equal]

(** The fields a [tool_confirm] (and [get_pending]'s confirmation) adds:
    none for [Tool]; [kind: "judged"] with [model] and [reason], or [kind:
    "computer_use"] and [host], otherwise. *)
val to_json_fields : t -> (string * Json.t) list
