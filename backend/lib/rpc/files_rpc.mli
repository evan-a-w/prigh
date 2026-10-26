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

    - [fs_write {path, text, expected?, review_root?}]: {!File_ops.write}'s
      [{path, md5}]; [expected] is the [md5] of the text the editor started
      from. With [review_root] (the review's editor, of a file under that
      worktree), the save is an action on the file in the review: it is
      snapshotted ({!Agent.update_review_file}, by [by]).

    URLs are paths on the web server ({!File_http}). *)

val methods : string list

(** [None] for other methods. *)
val dispatch
  :  ?by:string
  -> Agent.t
  -> meth:string
  -> params:Json.t
  -> Json.t Or_error.t option
