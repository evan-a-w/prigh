open! Core
open! Import

module Request = struct
  type t =
    { model : Model.t
    ; system : string option
    ; messages : Message.t list
    ; tools : Tool_spec.t list
    ; thinking : Thinking.t
    ; max_tokens : int option
    }
  [@@deriving sexp_of]
end

(** [stream] never raises for network/API failures; those are reported through
    [stop_reason] of the returned message ([Error _] or [Aborted]). *)
type t =
  { name : string
  ; stream :
      Request.t
      -> cancel:Cancellation.t
      -> on_event:(Assistant_event.t -> unit)
      -> Message.Assistant.t
  }
