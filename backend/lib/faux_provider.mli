open! Core
open! Import

(** A scripted provider for tests. Each call to [stream] consumes the next
    scripted reply; [on_request] observes every request. Running out of
    replies yields an [Error] stop reason. Between events the fiber yields
    (or runs [delay_between_events]) so runs behave asynchronously. *)

module Reply : sig
  type t =
    { events : Assistant_event.t list
    ; stop_reason : Stop_reason.t
    ; usage : Usage.t
    }
  [@@deriving sexp_of]

  val text : ?stop_reason:Stop_reason.t -> string -> t

  val tool_call
    :  ?text:string
    -> id:string
    -> name:string
    -> arguments:string
    -> unit
    -> t

  val tool_calls : ?text:string -> (string * string * string) list -> t

  (** A failed request: no content, no usage. *)
  val error : string -> t

  (** Anthropic's error for a request over the context window. *)
  val context_overflow : unit -> t

  (** A usage limit reached ({!Usage_limit.unavailable}), with its reset time
      noted ({!Usage_limit.reset_note}) when given. *)
  val usage_limit : ?resets_at:float -> unit -> t

  (** Parses a single scripted reply:
      [{ text; thinking; tool_calls; stop_reason; usage; chunks }], where
      [stop_reason] is [end_turn], [length], [error] (with [error], the
      message), [context_overflow] or [usage_limit] (with an optional
      [resets_at], seconds since the epoch). *)
  val of_json : Json.t -> t Or_error.t
end

(** Parses a JSON script file: an array of {!Reply.t} objects. *)
val of_script_file : string -> Reply.t list Or_error.t

(** [loop] restarts the script once it is exhausted. *)
val create
  :  ?on_request:(Provider.Request.t -> unit)
  -> ?delay_between_events:(unit -> unit)
  -> ?loop:bool
  -> Reply.t list
  -> Provider.t
