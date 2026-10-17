open! Core
open! Import

(** A JSON-RPC 2.0 peer over newline-delimited JSON (what ACP speaks on
    stdio): both sides send requests and notifications. Every incoming
    request and notification is handled in its own fiber, so a long one
    (an ACP prompt turn) never holds up the others, nor the responses to
    this side's own requests. *)

module Error : sig
  type t =
    { code : int
    ; message : string
    }
  [@@deriving sexp_of]

  val parse_error : string -> t
  val invalid_request : string -> t
  val method_not_found : string -> t
  val invalid_params : string -> t
  val internal : string -> t

  (** -32000 *)
  val auth_required : string -> t

  (** -32002 *)
  val resource_not_found : string -> t

  (** -32800 *)
  val request_cancelled : string -> t
end

module Reply : sig
  type t =
    | Result of Json.t
    | Result_then of Json.t * (unit -> unit)
    (** the function runs once the response is on its way (to send
        notifications the result makes meaningful, e.g. about a session the
        client did not know yet) *)
    | Error of Error.t
end

type t

(** [write_line] is called from one fiber at a time, in order. *)
val create : sw:Switch.t -> write_line:(string -> unit) -> t

(** Reads messages until [read_line] returns [None]; then the requests this
    side still waits for fail. *)
val serve
  :  t
  -> read_line:(unit -> string option)
  -> on_request:(method_:string -> params:Json.t -> Reply.t)
  -> on_notification:(method_:string -> params:Json.t -> unit)
  -> unit

(** Sends a request and waits for its response. [on_id] gets the request's
    id as it is sent (to cancel it with {!cancel_request}). *)
val request
  :  ?on_id:(Json.t -> unit)
  -> t
  -> string
  -> Json.t
  -> (Json.t, Error.t) Result.t

val notify : t -> string -> Json.t -> unit

(** [$/cancel_request] for a request of ours the peer has not answered: it
    should answer it at once (as cancelled). *)
val cancel_request : t -> Json.t -> unit
