open! Core
open! Import

(** Runs at most one interactive login at a time, turning the flow's prompts
    and notices into events for the RPC client, which answers prompts by id. *)

module Event : sig
  type t =
    | Auth_url of
        { url : string
        ; instructions : string
        }
    | Prompt of
        { id : string
        ; prompt : Auth_interaction.Prompt.t
        }
    | Prompt_cancelled of { id : string }
    (** The flow no longer needs the answer (e.g. the browser callback won). *)
    | Progress of string
    | Done of
        { provider : Provider_id.t
        ; method_ : Provider_auth.Method.t
        }
    | Failed of
        { provider : string (** ["custom"] while a new one has no name yet *)
        ; error : string
        }
    | Logged_out of Provider_id.t
  [@@deriving sexp_of]
end

type t

val create
  :  env:Env.t
  -> sw:Switch.t
  -> ?getenv:(string -> string option)
  -> ?models:Model_registry.t
  -> store:Auth_store.t
  -> unit
  -> t

val store : t -> Auth_store.t
val subscribe : t -> f:(Event.t -> unit) -> unit
val models : t -> Model_registry.t

(** Rereads the custom providers first, so hand edits show. *)
val status : t -> Provider_auth.Status.t list Or_error.t

val in_progress : t -> bool

(** Starts the flow in the background; completion is reported by [Done] or
    [Failed]. A custom provider runs {!Custom_login.login} to edit it. *)
val start : t -> Provider_id.t -> Provider_auth.Method.t -> unit Or_error.t

(** [/login custom]: defines a new custom provider (or edits [name]). *)
val start_custom : t -> ?name:string -> unit -> unit Or_error.t

val respond : t -> id:string -> string -> unit Or_error.t
val cancel : t -> unit
val wait : t -> unit

(** For a configured custom provider this is a flow ({!Custom_login.logout}:
    its prompt asks whether to remove the definition too) ending in
    [Logged_out], or nothing when the user keeps everything or cancels. *)
val logout : t -> Provider_id.t -> unit Or_error.t
