open! Core

(** An image in a message. Sexps leave out the (base64) [data]. *)
type t =
  { mime_type : string
  ; bytes : int
  ; data : string
  }
[@@deriving sexp_of, equal]

(** Decodes [{"mime_type", "data"}], [data] being base64. *)
val of_json : Json.t -> t Or_error.t

(** [\[image: image/png, 34.2 KB\]] *)
val to_string_hum : t -> string

(** The decoded size of base64 [data], padded or not. *)
val decoded_size : string -> int
