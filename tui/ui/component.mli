open! Core
module P = Prigh_protocol

(** The Bonsai component both platforms mount. The platform supplies the effects
    that commands need; everything else is shared. *)

module Platform : sig
  type t =
    { rpc :
        string
        -> (string * P.Json.t) list
        -> (P.Json.t, string) Result.t Bonsai.Effect.t
    ; list_paths : prefix:string -> (P.Json.t, string) Result.t Bonsai.Effect.t
    ; open_browser : string -> unit Bonsai.Effect.t
    ; quit : unit Bonsai.Effect.t
    ; load_history : unit -> (P.Json.t, string) Result.t Bonsai.Effect.t
    ; append_history : string -> unit Bonsai.Effect.t
    ; copy_to_clipboard : string -> unit Bonsai.Effect.t
    ; suspend : unit Bonsai.Effect.t
    ; edit_externally : string -> (string, string) Result.t Bonsai.Effect.t
    }
end

val create
  :  Platform.t Bonsai.t
  -> local_ Bonsai.graph
  -> App.Model.t Bonsai.t * (App.Action.t -> unit Bonsai.Effect.t) Bonsai.t
