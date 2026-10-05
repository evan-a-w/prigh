open! Core
open! Import

(** A streaming POST whose 2xx body is parsed as text/event-stream. Non-2xx
    bodies become ["HTTP <status>: <message>"] failures, which is the format
    [Agent_loop.is_retryable_error] inspects. *)

module Outcome : sig
  type t =
    | Completed
    | Aborted
    | Failed of string
  [@@deriving sexp_of]
end

val run
  :  env:Env.t
  -> ?timeout:Time_ns.Span.t
  -> cancel:Cancellation.t
  -> url:string
  -> headers:(string * string) list
  -> body:string
  -> on_event:(Sse.Event.t -> unit)
  -> unit
  -> Outcome.t

(** With [limit_rejected] (default false), or an error code saying the
    usage allowance is used up, the message ends in {!Usage_limit.marker}. *)
val error_message_of_body
  :  ?limit_rejected:bool
  -> status:int
  -> string
  -> string

val member_string : string -> Json.t -> string option
