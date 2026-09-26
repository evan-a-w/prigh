open! Core
open! Import

(** The Bonsai component both platforms mount. The platform supplies the effects
    that commands need; everything else is shared. *)

module Platform : sig
  type t =
    { rpc :
        string
        -> (string * P.Json.t) list
        -> (P.Json.t, string) Result.t Bonsai.Effect.t
    ; open_browser : string -> unit Bonsai.Effect.t
    ; quit : unit Bonsai.Effect.t
    ; load_history : unit -> (P.Json.t, string) Result.t Bonsai.Effect.t
    ; append_history : string -> unit Bonsai.Effect.t
    ; copy_to_clipboard : string -> unit Bonsai.Effect.t
    ; suspend : unit Bonsai.Effect.t
    ; edit_externally : string -> (string, string) Result.t Bonsai.Effect.t
    ; reconnect :
        delay_ms:int
        -> session:string option
        -> (P.Json.t, string) Result.t Bonsai.Effect.t
    (** Waits, connects again and sends [hello]; the result is the hello reply. *)
    }
end

(** [start_on_activate] defaults to true. A driver that must wire input before
    its first browser frame can disable it and schedule [App.Action.Start]
    itself. *)
val create
  :  ?start_on_activate:bool
  -> Platform.t Bonsai.t
  -> local_ Bonsai.graph
  -> App.Model.t Bonsai.t * (App.Action.t -> unit Bonsai.Effect.t) Bonsai.t
