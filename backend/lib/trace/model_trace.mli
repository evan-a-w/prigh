open! Core
open! Import

(** [provider.stream] in a [model_request] span: its phases are [connect]
    (until the response's first byte, see {!Http_client}), [first_token],
    then [thinking] and [stream] as those deltas arrive. A failed request
    ends [Error] (its message in [error]), an aborted one [Cancelled]. At
    {!Trace.Level.Detailed} each delta after the first adds [delta_wait] (the
    gap before it) and [delta_handle] (what [on_event] took). *)
val stream
  :  Provider.t
  -> Provider.Request.t
  -> cancel:Cancellation.t
  -> on_event:(Assistant_event.t -> unit)
  -> Message.Assistant.t
