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

(** What a model accepts per image. *)
module Limit : sig
  type t =
    { max_base64_bytes : int
    ; max_dimension : int (** pixels, either way *)
    }
  [@@deriving sexp, equal, compare, hash]

  (** Anthropic's, with a margin: 4.5 MB of base64, 8000 px. *)
  val default : t

  (** ["3.4 MB, 8000 px"] (the decoded size) *)
  val describe : t -> string

  (** Stricter, for when a provider refused images within [t]: half the
      bytes, and at most 2000 px, then three quarters of it (never under
      256 KB or 512 px). *)
  val tighten : t -> t

  val min : t -> t -> t
end

(** Whether [t] is over [limit] in bytes or pixels. *)
val exceeds : Limit.t -> t -> bool

module Fitted : sig
  type nonrec t =
    | Fits
    | Downscaled of t
    | Cannot of string (** why, and what to do *)
  [@@deriving sexp_of]
end

(** [t] downscaled to within [limit] when it is over it, like [load]. *)
val fit
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?search_path:string list
  -> Limit.t
  -> t
  -> Fitted.t

(** ["image/png, 800x600"] *)
val describe : Loaded.t -> string
