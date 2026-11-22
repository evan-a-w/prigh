open! Core
open! Import

(** The editor mode: files of the session's tool host open in an editor
    (CodeMirror, in the page), the session's chat beside it on a wide window
    and under it as a peek on a phone. Saving writes the file on the host,
    refusing when it changed there since it was read; when the agent changes
    an open file, a clean buffer takes the new text and shows each of its
    changes as a diff to keep or undo, a dirty one says so instead of losing
    either. Go to file matches the query against every file under the cwd
    ({!Prigh_ui.Fuzzy_path}), listed once by the host. *)

module Lang : sig
  (** What the page highlights it as. *)
  type t =
    | Ocaml
    | Markdown
    | Javascript
    | Typescript
    | Rust
    | Python
    | Nix
    | Shell
    | Json
    | Css
    | Html
    | Dune (** dune files and other s-expressions *)
    | Plain
  [@@deriving sexp_of, equal]

  val of_path : string -> t

  (** As the page's glue names it. *)
  val name : t -> string
end

module Hunk : sig
  (** One change of the agent's, in the buffer's text: [added] lines from
      line [line] (1-based) replaced [removed]. *)
  type t =
    { line : int
    ; removed : string list
    ; added : string list
    }
  [@@deriving sexp_of, equal]

  val of_texts : before:string -> after:string -> t list

  (** [text] with the change taken back, where its lines are (at [line], or
      the nearest place they moved to): the text, the line it starts at and
      how many lines that added; [None] when its lines are no longer there. *)
  val revert : t -> string -> (string * int * int) option

  (** The changes' lines after the text went from [before] to [after]. *)
  val remap : t list -> before:string -> after:string -> t list
end

module Conflict : sig
  type t =
    | Changed_on_disk
    (** the file changed on the host while the buffer had edits *)
    | Save_refused of string (** the backend's reason *)
  [@@deriving sexp_of, equal]
end

module Buffer : sig
  type t =
    { path : string (** absolute, on the session's tool host *)
    ; text : string (** as in the editor *)
    ; disk : string (** as last read or written *)
    ; md5 : string option (** of [disk]; [None]: too large to save *)
    ; revision : int
      (** bumped when [text] is replaced here (read, reloaded, undone): the
        page then puts it in the editor *)
    ; saving : bool
    ; conflict : Conflict.t option
    ; hunks : Hunk.t list (** the agent's changes on show (Keep / Undo) *)
    ; kept : Hunk.t list (** those kept: a badge in the gutter reopens one *)
    }
  [@@deriving sexp_of, equal]

  val dirty : t -> bool

  (** Lines the agent changed (first, last), for the gutter's bar. *)
  val marks : t -> (int * int) list
end

module Selection : sig
  type t =
    { first : int
    ; last : int (** lines, 1-based *)
    ; text : string (** empty: just the caret *)
    }
  [@@deriving sexp_of, equal]
end

module Chip : sig
  (** The code the next prompt is about, a chip in the composer. *)
  type t =
    { path : string (** relative to the cwd where it is under it *)
    ; first : int
    ; last : int
    ; text : string (** the selected code; empty: the caret's line *)
    }
  [@@deriving sexp_of, equal]

  (** [lib/compact.ml:48–58], [lib/compact.ml:52] *)
  val label : t -> string

  (** The prompt's text and the pasted texts it goes with. *)
  val prompt : t -> string -> string * string list
end

module Command : sig
  (** What the page's editor does for us. *)
  type t =
    | Insert of string (** at the caret, replacing the selection *)
    | Indent
    | Undo
    | Redo
    | Left
    | Right
    | Goto_line of int
    | Find
    | Focus
  [@@deriving sexp_of, equal]
end

module Reply : sig
  type t =
    | Opened of string
    | Checked of string (** read again after the agent's tools ran *)
    | Saved of string
    | Listed
    | Files of string (** the cwd it listed *)
  [@@deriving sexp_of, equal]
end

module Index : sig
  (** Every file under the cwd, for Go to file. *)
  type t =
    { files : string array (** relative to the cwd *)
    ; truncated : bool (** the host stopped listing (a million files) *)
    ; loading : bool
    ; loaded : bool
    }
  [@@deriving sexp_of]
end

module Out : sig
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * Json.t) list
        ; reply : Reply.t
        }
    | Command of Command.t
    | Toast of
        { text : string
        ; error : bool
        }
    | Focus of string (** an element's id, once rendered *)
    | Focus_composer
    | Refresh_changes (** the git marks of the tree *)
    | Send of
        { chip : Chip.t
        ; text : string
        } (** a prompt about [chip], from the box under the lines *)
  [@@deriving sexp_of]
end

type t =
  { open_ : bool
  ; cwd : string
  ; buffers : Buffer.t list (** in the order opened *)
  ; current : string option (** a buffer's path *)
  ; selection : Selection.t option (** in the current buffer *)
  ; chip : Chip.t option
  ; asking : Chip.t option (** the ask box, under these lines *)
  ; tree : bool (** the file tree, docked on a wide window *)
  ; drawer : bool (** the file tree, a drawer on a phone *)
  ; dir : File_listing.t option (** the tree's directory *)
  ; filter : string (** Go to file *)
  ; index : Index.t
  ; matches : Prigh_ui.Fuzzy_path.Match.t array (** for [filter], best first *)
  ; matched : string (** the query [matches] are for *)
  ; list_top : int (** the first of [matches] on screen *)
  ; chat : bool (** a wide window's chat beside it *)
  ; sheet : bool (** a phone's chat, over the editor *)
  ; menu : bool (** the ⋯ menu *)
  ; goto : string option (** the Go to line field *)
  ; strip_hidden : bool (** a phone's top strip, scrolled away *)
  ; loading : string option
  }
[@@deriving sexp_of]

val empty : t
val current : t -> Buffer.t option

(** [path] relative to [cwd] where it is under it. *)
val relative : t -> string -> string

(** The path as typed ([~], relative to [cwd]), made absolute. *)
val absolute : t -> string -> string

module Action : sig
  type t =
    | Enter of string option (** the mode, opening the file (as typed) *)
    | Leave
    | Open of string (** absolute *)
    | Switch of string
    | Step of int (** the next (1) or previous (-1) open file *)
    | Swiped of int (** [Step], from a swipe on a phone *)
    | Close of string
    | Edited of
        { path : string
        ; text : string
        }
    | Selected of Selection.t option
    | Save
    | Overwrite (** save over the host's newer file *)
    | Reload (** the host's file, dropping the buffer's edits *)
    | Keep of int (** the agent's change on show, by its index *)
    | Undo of int (** takes it back, and saves *)
    | Keep_all
    | Undo_all
    | Reopen of int (** a kept change, by its index, back on show *)
    | Ask (** the selection (or the caret's line) as the next prompt's chip *)
    | Ask_inline (** the ask box under the selection *)
    | Ask_send of string (** the ask box's prompt *)
    | Ask_cancel
    | Drop_chip
    | Toggle_tree
    | Toggle_drawer
    | List of string option (** the tree's directory; [None]: the cwd *)
    | Filter of string
    | List_scrolled of int (** the first of the matches on screen *)
    | Find_file (** the Go to file field *)
    | Toggle_chat
    | Toggle_sheet
    | Toggle_menu
    | Goto_open
    | Goto_input of string
    | Goto_line
    | Goto_cancel
    | Scrolled of [ `Down | `Up ]
    | Command of Command.t
  [@@deriving sexp_of]
end

val update : t -> Action.t -> t * Out.t list
val reply : t -> Reply.t -> (Json.t, string) Result.t -> t * Out.t list

(** The session's cwd (a new session or host closes the mode: its files were
    another host's). *)
val set_where : t -> session_changed:bool -> cwd:string -> t

(** The agent's tools may have changed files: the open ones are read
    again. *)
val changed : t -> t * Out.t list

(** The list of files Go to file matches, asked of the host if not yet. *)
val load_index : t -> t * Out.t list

(** The chip was sent with a prompt. *)
val sent : t -> t
