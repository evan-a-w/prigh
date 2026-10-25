open! Core
open! Import

(** The backend's side of the file browser: {!Host_files} operations on a
    session's active tool host (the backend itself or a connected client),
    with the streaming of downloads and uploads. Errors from hosts that
    predate them say to update prigh there. *)

(** The largest file [upload] takes (2 GiB). *)
val max_upload : int

(** Text previews show at most this much of a file (256 KiB). *)
val max_preview : int

(** Images larger than this (20 MB) are not previewed inline. *)
val max_image_preview : int

(** Where files uploaded into the chat go, under the session cwd. *)
val uploads_dir : string

(** [name] with [arguments] on the active host; its JSON reply. *)
val call : Agent.t -> string -> (string * Json.t) list -> Json.t Or_error.t

(** [{path, name, size, mtime, kind: text, text, truncated}], [{..., kind:
    image, mime, url}] ([url] from [image_url], unless the image is too
    large: then [too_large: true]) or [{..., kind: binary}]. A whole text
    file also has [md5], its bytes' digest (hex), for {!write}. *)
val preview
  :  Agent.t
  -> path:string
  -> image_url:(path:string -> mime:string -> string)
  -> Json.t Or_error.t

(** Writes [text] to [path] with the [write] tool on the active host
    (unchecked by [Seen_files]: the agent is told to read it again before
    writing it). With [expected], fails without writing unless the file's
    bytes still have that [md5]. Returns [{path, md5}]. *)
val write
  :  Agent.t
  -> path:string
  -> text:string
  -> expected:string option
  -> Json.t Or_error.t

(** Streams the file (or [archive]: the directory as .tar.gz) to [write],
    which is called in the caller's fiber and may raise when the receiver is
    gone (the host then stops). Returns the bytes sent. *)
val download
  :  Agent.t
  -> path:string
  -> archive:bool
  -> write:(string -> unit)
  -> int Or_error.t

module Conflict : sig
  type t =
    | Fail
    | Overwrite
    | Rename (** a free [name-N.ext] next to it *)
  [@@deriving sexp_of, equal]

  val to_string : t -> string
end

(** Writes what [read n] returns (at most [n] bytes per call, [""] at the
    end; it may raise when the sender is gone) to [path] on the host,
    through a temporary file next to it, at most a few chunks ahead of the
    host's acknowledgements. With [make_dir], the directory is created
    first (as the chat's uploads directory, ignored by git). Returns
    [{path, bytes}]. *)
val upload
  :  Agent.t
  -> path:string
  -> conflict:Conflict.t
  -> size:int
  -> ?make_dir:bool
  -> read:(int -> string)
  -> unit
  -> Json.t Or_error.t
