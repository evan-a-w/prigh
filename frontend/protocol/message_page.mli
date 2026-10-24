open! Core

(** A page of a session's transcript ([get_messages_page]): oldest first, the
    newest page first; [before] asks for the page before it. *)
type t =
  { messages : Message.t list
  ; older : int (** how many messages come before these *)
  ; before : string option (** [None] when [older] is 0 *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
