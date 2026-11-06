open! Core

(** The reply of [delete_sessions] and [delete_archived]: what was deleted and
    what the backend refused to delete (running, open elsewhere, the caller's
    own session without a {!Switch_to}), with why; and the session the client
    was moved to, when it was. *)

module Deleted : sig
  type t =
    { path : string
    ; id : string
    ; bytes : int
    }
  [@@deriving sexp_of, equal]
end

module Refused : sig
  type t =
    { path : string
    ; error : string (** says what to do *)
    }
  [@@deriving sexp_of, equal]
end

type t =
  { deleted : Deleted.t list
  ; refused : Refused.t list
  ; moved_to : State.t option (** the new session's, as [switch_session]'s *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** ["Deleted 3 sessions (1.2 MB)"] (unless nothing was but something was
    refused), then each refusal: ["Not deleted: <why and what to do>"]. *)
val summary : t -> string
