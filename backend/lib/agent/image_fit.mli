open! Core
open! Import

(** Images sent within a model's limit ({!Image_limit}): those over it are
    downscaled for the request, the session keeping the original. Each
    image is downscaled once per limit (remembered until the backend
    restarts). *)

module Outcome : sig
  type t =
    { downscaled : int (** images sent smaller than recorded *)
    ; unfit : string list (** why images still over the limit are *)
    }
  [@@deriving sexp_of]
end

(** The images in user messages and tool results. *)
val count : Message.t list -> int

val messages
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?search_path:string list
  -> Image.Limit.t
  -> Message.t list
  -> Message.t list * Outcome.t

(** Forgets the downscaled images, for tests. *)
val reset : unit -> unit
