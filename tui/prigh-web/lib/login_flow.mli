open! Core
open! Import

(** A provider login in progress, as the login dialog shows it. *)

type t =
  { provider : string
  ; url : (string * string) option (** the page to open, and instructions *)
  ; progress : string list
  ; prompt : (string * Auth_event.Prompt.t) option (** its id and question *)
  ; input : string (** the answer being typed *)
  ; selected : int (** the highlighted option of a [Select] prompt *)
  ; failed : string option
  }
[@@deriving sexp_of, equal]

val start : string -> t

(** [Done] and [Logged_out] are for the caller: they leave [t] unchanged. *)
val apply : t -> Auth_event.t -> t

val move : t -> int -> t

(** The answer to send for the current prompt. *)
val answer : t -> (string * string) option
