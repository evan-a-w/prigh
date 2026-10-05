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

  let omit_unsupported_images t =
    if t.model.supports_images
    then t
    else { t with messages = List.map t.messages ~f:Message.omit_images }
  ;;
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
