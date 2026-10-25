open! Core
open! Import

(** A provider login (or a custom provider's logout, which asks what to
    remove) in progress, as the login dialog shows it. *)

module Purpose : sig
  type t =
    | Login
    | Logout
  [@@deriving sexp_of, equal]
end

type t =
  { provider : string (** ["custom"] while adding a custom provider *)
  ; purpose : Purpose.t
  ; url : (string * string) option (** the page to open, and instructions *)
  ; progress : string list
  ; prompt : (string * Auth_event.Prompt.t) option (** its id and question *)
  ; input : string (** the answer being typed; a [Text] prompt's default *)
  ; selected : int (** the highlighted option of a [Select] prompt *)
  ; failed : string option
  ; forge : string option
    (** a git host's sign-in ([provider] is the host): the [/login]
        arguments that try again *)
  }
[@@deriving sexp_of, equal]

val start : ?purpose:Purpose.t -> ?forge:string -> string -> t

(** A device code in the instructions ([Enter the code ABCD-1234 at ...]),
    to show big and copy. *)
val device_code : t -> string option

(** [Done] and [Logged_out] are for the caller: they leave [t] unchanged. *)
val apply : t -> Auth_event.t -> t

val move : t -> int -> t

(** The answer to send for the current prompt: [Text] prompts and [Secret]
    ones that allow it may be answered with nothing. *)
val answer : t -> (string * string) option
