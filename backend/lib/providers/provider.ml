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
    ; cache_key : string option
      (** groups requests that share a prompt prefix (a session, a
          subagent), for providers that route caching by key *)
    }
  [@@deriving sexp_of]

  let omit_unsupported_images t =
    if t.model.supports_images
    then t
    else { t with messages = List.map t.messages ~f:Message.omit_images }
  ;;

  (* Anthropic refuses a conversation that does not start with the user's
     turn, as one does once its first message is deleted. *)
  let lead_with_user = function
    | ([] | Message.User _ :: _) as messages -> messages
    | messages ->
      Message.user "[the conversation's earlier messages were removed]"
      :: messages
  ;;

  let prepare t =
    omit_unsupported_images
      { t with messages = lead_with_user (Tool_pairing.repair t.messages) }
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
