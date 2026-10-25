open! Core
open! Import

(** Reviewing the working tree's changes: the session's diff (kept fresh
    while something shows it), the review the backend keeps ([Review]: the
    comments with their threads and the files' approvals), and what the user
    is doing with them: the review page's file and mode, a selection of lines
    and the comment being written on them, replies being typed, the file
    being edited. Shared by the review page and the file browser's preview of
    a changed file. Pure: [update] and [reply] return [Out.t]s, which [App]
    turns into requests and toasts.

    Paths are relative to the repository's root, as [git diff] names
    them. *)

module Mode : sig
  type t =
    | Comment (** select lines and comment on them *)
    | Edit (** edit the file's text and save it on the tool host *)
  [@@deriving sexp_of, equal]
end

module Layout : sig
  type t =
    | Unified (** the changed hunks *)
    | Full (** the whole file with the changes inline *)
  [@@deriving sexp_of, equal]
end

module Selection : sig
  (** Lines of the file as it is now, 1-based, inclusive. *)
  type t =
    { path : string
    ; first : int
    ; last : int
    ; from : int (** the line clicked first: shift-clicks extend from it *)
    }
  [@@deriving sexp_of, equal]
end

module Draft : sig
  (** A comment being written on the selected lines. *)
  type t =
    { path : string
    ; first : int
    ; last : int
    ; text : string
    ; busy : bool (** sent, waiting for the backend *)
    }
  [@@deriving sexp_of, equal]
end

module Content : sig
  (** A file's text now, for the whole-file layout, anchoring comments and
      editing. *)
  type t =
    | Loading
    | Loaded of
        { text : string
        ; md5 : string option (** [None]: too large to edit here *)
        }
    | Failed of string
  [@@deriving sexp_of, equal]
end

module Editor : sig
  type t =
    { path : string
    ; text : string
    ; md5 : string option (** of the file as it was opened or last saved *)
    ; dirty : bool
    ; saving : bool
    ; conflict : bool (** it changed on disk: the next save overwrites it *)
    ; error : string option
    }
  [@@deriving sexp_of, equal]
end

type t =
  { session : string option (** whose review this is *)
  ; review : Review.t
  ; diff : Git_diff.t option
  ; files : Prigh_ui.Diff_files.File.t list
  ; diff_error : string option
  ; loading : bool (** a [git_diff] is on its way *)
  ; again : bool (** and another is wanted once it is in *)
  ; open_ : bool (** the review page shows, in the chat's place *)
  ; current : string option (** the page's file *)
  ; mode : Mode.t
  ; layout : Layout.t
  ; selection : Selection.t option
  ; draft : Draft.t option
  ; replies : string String.Map.t (** what is typed under a comment, by id *)
  ; busy : String.Set.t (** comments with a request on its way *)
  ; contents : Content.t String.Map.t
  ; editor : Editor.t option
  ; submitting : bool
  }
[@@deriving sexp_of]

val empty : t

module Reply : sig
  (** Why we called the backend. *)
  type t =
    | Diff
    | Review
    | Comment_added
    | Changed of string option (** a comment's id *)
    | Decided of string (** a proposal of the comment's: the file changed *)
    | Content of string (** a path *)
    | Saved of string
    | Submitted
  [@@deriving sexp_of, equal]
end

module Out : sig
  type t =
    | Rpc of
        { method_ : Request.Method.t
        ; reply : Reply.t
        }
    | Get_diff (** [git_diff] for the whole tree, answered as [Reply.Diff] *)
    | Toast of
        { text : string
        ; error : bool
        }
    | Focus of string (** an element's id, once rendered *)
  [@@deriving sexp_of, equal]
end

module Action : sig
  type t =
    | Open of string option (** the page, at a file *)
    | Close
    | Toggle
    | Go of string (** the page's file *)
    | Step of int (** the next (1) or previous (-1) file *)
    | Set_mode of Mode.t
    | Set_layout of Layout.t
    | Select_line of
        { path : string
        ; line : int
        ; extend : bool (** shift-click: from the first line clicked *)
        }
    | Clear_selection
    | Start_comment (** on the selection *)
    | Draft_input of string
    | Draft_cancel
    | Draft_send
    | Reply_input of
        { id : string
        ; text : string
        }
    | Reply_send of string
    | Set_status of
        { id : string
        ; status : Review_comment.Status.t
        }
    | Delete of string
    | Decide of
        { id : string
        ; post : int
        ; accept : bool
        }
    | Toggle_approval of string
    | Submit (** send the new comments and replies to the agent *)
    | Edit_input of string
    | Save
    | Reload (** the file, dropping the editor's changes *)
    | Refresh
  [@@deriving sexp_of]
end

val update : t -> Action.t -> t * Out.t list
val reply : t -> Reply.t -> (Json.t, string) Result.t -> t * Out.t list

(** The session changed (or the page connected): a new one starts over and
    reads its review, and its changes too while something shows them
    ([watching]). *)
val set_session : t -> string option -> watching:bool -> t * Out.t list

(** The review changed (the [review] event). *)
val set_review : t -> Review.t -> t

(** The files may have changed (the agent edited, a run ended): the diff is
    read again while something shows it ([watching]). *)
val changed : t -> watching:bool -> t * Out.t list

(** Read the diff now (the file browser opened). *)
val refresh : t -> t * Out.t list

(** The file browser's preview of a file: its text, so comments and the
    whole-file layout need not read it again. *)
val preview_text : t -> abs:string -> text:string -> md5:string option -> t

(** The repository's root, once the diff is in. *)
val root : t -> string option

(** [abs] relative to the root, if inside it. *)
val relative : t -> abs:string -> string option

val file : t -> string -> Prigh_ui.Diff_files.File.t option

(** For the file browser's rows: whether [abs] is a changed file (with its
    approval) or a directory with changes inside. *)
val mark
  :  t
  -> abs:string
  -> [ `File of Prigh_ui.File_approval.Status.t | `Dir of int | `None ]

val approval
  :  t
  -> Prigh_ui.Diff_files.File.t
  -> Prigh_ui.File_approval.Status.t

val approved : t -> int

(** Comments with posts the agent has not seen. *)
val unsent : t -> int

(** The page's keys (focus on the page, not in a field): [\]] / [n] / [j]
    next file, [\[] / [p] / [k] previous, [a] approve, [c] comment on the
    selection, [e] edit or comment mode, [v] the whole file or the hunks,
    [s] send the comments, Esc clears the selection then closes. *)
val key
  :  t
  -> key:string
  -> shift:bool
  -> ctrl:bool
  -> alt:bool
  -> Action.t option

(** Alt+R (by the key's position): open or close the review page. *)
val toggle_key
  :  code:string
  -> shift:bool
  -> ctrl:bool
  -> alt:bool
  -> meta:bool
  -> bool

(** In a comment or reply field: Ctrl/⌘+Enter sends, Esc cancels. In the
    editor: Ctrl/⌘+S saves. *)
val field_key
  :  t
  -> field:[ `Draft | `Reply of string | `Editor ]
  -> key:string
  -> ctrl:bool
  -> meta:bool
  -> Action.t option
