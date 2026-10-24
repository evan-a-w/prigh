open! Core
open! Import

(** An image a model can see: PNG, JPEG, GIF or WebP, base64-encoded. *)
type t =
  { mime_type : string
  ; data : string
  }
[@@deriving sexp, jsonaf, equal]

(** The MIME type of [bytes] from its magic number, if it is one of the
    formats models accept. *)
val sniff : string -> string option

(** Width and height read from the image header. *)
val dimensions : string -> (int * int) option

(** Images are sent as they are when they fit in this many pixels each way
    and their base64 in [max_base64_bytes]; larger ones are downscaled. *)
val max_dimension : int

val max_base64_bytes : int

type image := t

module Loaded : sig
  type t =
    { image : image
    ; width : int option
    ; height : int option
    ; note : string option (** how the image was downscaled, for the model *)
    }
  [@@deriving sexp_of]
end

(** Checks [bytes] and downscales it when it is too large, with ImageMagick
    ([magick] or [convert]) or [sips] when one is found in [search_path]
    (default [$PATH]). Errors say what to do. *)
val load
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?search_path:string list
  -> string
  -> Loaded.t Or_error.t

(** [load] on base64 [data] from a client; [mime_type] is what the client
    declared, for the error when it is not an image models accept. *)
val of_base64
  :  env:Env.t
  -> ?search_path:string list
  -> mime_type:string
  -> string
  -> t Or_error.t

(** ["image/png, 800x600"] *)
val describe : Loaded.t -> string
