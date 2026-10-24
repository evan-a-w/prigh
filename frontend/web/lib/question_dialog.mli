open! Core
open! Import

(** An [ask_user] call being answered: its questions, one at a time, as a
    [Prigh_ui.Question_form] that keys and clicks both drive. *)

module Action : sig
  type t =
    | Up
    | Down
    | Prev (** the previous question *)
    | Next (** the next question, without answering this one *)
    | Row of int (** the cursor to a row (the own answer's is the last) *)
    | Choose of int (** clicked: picks or toggles the choice *)
    | Toggle (** Space *)
    | Insert of string (** typed outside the own answer's field *)
    | Backspace
    | Set_own of string (** the own answer's field *)
    | Accept (** Enter: the next question, or the answers after the last *)
    | Decline (** Esc *)
  [@@deriving sexp_of, equal]
end

type t =
  { call_id : string
  ; form : Prigh_ui.Question_form.t
  }
[@@deriving sexp_of, equal]

val create : Pending.Ask.t -> t

val update
  :  t
  -> Action.t
  -> [ `Show of t | `Answer of string list list | `Decline ]

(** [ask_user_respond]'s parameters. *)
val answer_params : t -> string list list -> (string * Json.t) list

val decline_params : t -> (string * Json.t) list

(** The element that should have the focus: the own answer's field while
    the cursor is on it, else the dialog. *)
val focus : t -> string
