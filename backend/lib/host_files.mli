open! Core
open! Import

(** The file browser's operations on a tool host's filesystem (pseudo-tools
    run by {!Host_ops.execute}, so they reach whichever host the session
    uses). Paths are resolved against the exec's cwd ([~] is the host's
    home). Replies are JSON text:

    - [$fs_list {path, hidden}]: [{path, parent, home, entries: [{name, kind,
      size, mtime, target?}], total, hidden}] ([kind]: [file], [dir],
      [link] (with [target]: [file], [dir] or [missing]) or [other];
      [mtime] in ms; at most {!max_entries} entries, [total] counts them
      all, [hidden] the dot files left out). A file's [path] lists its
      directory, with [select]: the file's name.
    - [$fs_stat {path}]: [{path, kind, size, mtime}].
    - [$fs_read {path, offset, length}]: [{size, mtime, data, eof}], [data]
      base64, at most {!max_read} bytes.
    - [$fs_mkdir {path, parents?, ignore_all?}]: [{path}]; [ignore_all]
      puts a [.gitignore] of [*] in a directory it creates.
    - [$fs_rename {from, to, overwrite?}]: [{path}].
    - [$fs_delete {path}]: [{path}], directories with their contents
      (symlinks are removed, never followed); not [/] or the home.
    - [$fs_download {path, archive?, window?}]: streams the file (or, with
      [archive], [tar -cz] of the directory) as output chunks of base64,
      at most [window] (default 16) ahead of the credits the stdin feed
      grants (one per [+]); [{bytes}].
    - [$fs_upload {path, conflict?, size?}]: writes the stdin feed's base64
      chunks to a temporary file next to [path], acknowledging each with an
      output [+], and on the end of input moves it to [path] ([conflict]:
      [fail] (default), [overwrite] or [rename]: [name-2.ext], ...);
      [{path, bytes}]. Cancelled, the temporary file is removed. *)

val is_op : string -> bool
val max_entries : int
val max_read : int

(** Raw bytes per [$fs_download] chunk, and the size [$fs_upload] chunks
    should have (a multiple of 3, so their base64 has no padding). *)
val chunk_size : int

val list_op : string
val stat_op : string
val read_op : string
val mkdir_op : string
val rename_op : string
val delete_op : string
val download_op : string
val upload_op : string

(** A file or directory name the user typed: not empty, [.] or [..], and
    without [/] or NUL. *)
val check_name : string -> unit Or_error.t

val execute
  :  env:Env.t
  -> ?stdin:Stdin_feed.t
  -> cancel:Cancellation.t
  -> on_output:(string -> unit)
  -> cwd:string
  -> name:string
  -> arguments:Json.t
  -> unit
  -> Tool.Result.t
