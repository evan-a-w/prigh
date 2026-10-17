open! Core
open! Import

(** How prigh's tool calls look to an ACP client: their kind, title,
    locations and content blocks. Paths are absolute (resolved against the
    session cwd). *)

(** [execute] (bash), [read] (read, ls), [search] (grep, find), [edit]
    (edit, write, apply_patch), [fetch] (web_fetch, web_search), [think]
    (update_plan), [switch_mode] (exit_plan_mode), else [other]. *)
val kind : string -> string

(** A one-line title: the command, the path, the pattern, the URL... *)
val title : name:string -> args:Json.t -> cwd:string -> string

(** [{path, line?}] of the files the call reads or changes. *)
val locations : name:string -> args:Json.t -> cwd:string -> Json.t list

(** Whether the tool changes files (its result shows diffs). *)
val edits_files : string -> bool

(** A [diff] content block. *)
val diff : path:string -> old_text:string option -> new_text:string -> Json.t

(** The diffs a call's arguments describe, when the files themselves were
    not seen (the call ran on another host, or is replayed): an [edit]'s
    replaced texts, a [write]'s new contents, an [apply_patch]'s hunks. *)
val diffs_of_args : name:string -> args:Json.t -> cwd:string -> Json.t list

(** A [content] block of text; [execute] and [search] output is fenced. *)
val text : ?kind:string -> string -> Json.t

(** The arguments of a call as prigh sends them (JSON text), as an object
    ([{}] when they do not parse). *)
val parse_args : string -> Json.t
