open! Core
open! Import

(** The file browser's RPC methods, on the calling client's session's active
    tool host:

    - [fs_list {path?, hidden?}] (default: the session cwd): [$fs_list]'s
      reply (see {!Host_files}).
    - [fs_preview {path}]: {!File_ops.preview}'s, an image's [url] being an
      [Inline] ticket's.
    - [fs_mkdir {path}], [fs_rename {from, to}], [fs_delete {path}]:
      [{path}].
    - [fs_download {path, archive?}]: [{url, name}], a ticket for
      [GET url]: the file, or with [archive] the directory as
      [NAME.tar.gz].
    - [fs_upload {name, size, dir?, overwrite?}]: [{url, path}], a ticket
      for [PUT url] with the file as the body. Into [dir], failing when
      [name] exists there unless [overwrite]; without [dir], into the chat's
      uploads directory {!File_ops.uploads_dir} under the session cwd, under
      a free name.

    URLs are paths on the web server ({!File_http}). *)

val methods : string list

(** [None] for other methods. *)
val dispatch
  :  Agent.t
  -> meth:string
  -> params:Json.t
  -> Json.t Or_error.t option
