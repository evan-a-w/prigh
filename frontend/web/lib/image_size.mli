open! Core
open! Import

(** Whether an image attached to the next prompt fits the model, and how
    small to make it when it does not (the page re-encodes it on a canvas,
    app/downsize.ml). The backend downsizes what is still too large anyway;
    doing it here shows the result first and uploads less. *)

(** Width and height from the image's header (PNG, JPEG, GIF, WebP). *)
val dimensions : Image.t -> (int * int) option

(** Used when the backend does not say: Anthropic's, the strictest common
    one (5 MB of base64, about 3.5 MB decoded, and 8000 px). *)
val default_limit : Llm.Image_limit.t

module Verdict : sig
  type t =
    | Fits
    | Too_large of string
    (** ["4.8 MB, over 3.4 MB"], ["9000×120 px, over 8000"] *)
  [@@deriving sexp_of, equal]
end

val check : Llm.Image_limit.t -> Image.t -> Verdict.t

(** What to re-encode an image to: the largest width and height within the
    limit's pixels, and the bytes to stay under (the page tries JPEG
    qualities, then smaller sizes, until it does). [None] when it fits. *)
module Target : sig
  type t =
    { width : int
    ; height : int
    ; max_bytes : int (** decoded, with a 10% margin *)
    }
  [@@deriving sexp_of, equal]
end

val target : Llm.Image_limit.t -> Image.t -> Target.t option

(** [Base64] decoding of the first bytes, for the header. *)
val decode_prefix : string -> len:int -> string

(** ["640 KB"], ["4.8 MB"] *)
val size_hum : int -> string

(** The MIME type models accept (PNG, JPEG, GIF, WebP) from base64 [data]'s
    magic number, whatever the file claimed. *)
val sniff : string -> string option

(** Whether a file is worth reading as an image: its MIME type is an image
    one, or it has none (or a generic one) and its name ends in an image
    extension. Those that turn out not to be ({!sniff}, and the browser
    cannot draw them) are attached as files. *)
val looks_like_image : mime_type:string -> name:string -> bool
