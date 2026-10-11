open! Core

(** [fs_preview]'s reply: what the file browser shows of a file. *)

module Content : sig
  type t =
    | Text of
        { text : string (** at most 256 KiB, valid UTF-8 *)
        ; truncated : bool
        }
    | Image of
        { mime : string
        ; url : string option
        (** a link on the backend's web server ([/files/download?...]);
            [None] when the image is too large to preview *)
        }
    | Binary
  [@@deriving sexp_of, equal]
end

type t =
  { path : string
  ; name : string
  ; size : int
  ; mtime : Time_ns.t option
  ; content : Content.t
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
