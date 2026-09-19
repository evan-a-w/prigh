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

  (** Parses a single scripted reply:
      [{ text; thinking; tool_calls; stop_reason; usage; chunks }]. *)
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
