open! Core
open! Import

(** Reviewing the working tree's changes: the session's diff (kept fresh
    while something shows it), the review the backend keeps ([Review]: the
    comments with their threads and the files' approvals), and what the user
    is doing with them: the review page's file and mode, the code selected
    (text dragged across, or lines by their numbers) and the comment being
    written on it, replies being typed, the file
    being edited. Shared by the review page and the file browser's preview of
    a changed file. Pure: [update] and [reply] return [Out.t]s, which [App]
    turns into requests and toasts.

    The page reviews one worktree at a time ([Target]): the session's own,
    or another (from [/review WORKTREE], the page's pickers, the worktree
    picker, the sidebar), or a branch that is not checked out (read-only:
    its files are read with [git_show], it cannot be edited and proposals
    cannot be accepted), against its base or one chosen. A base that is no
    revision goes back to what was reviewed before, with a toast naming the
    closest. [review] is that worktree's part of the
    session's review ([all]): comments and approvals of other worktrees stay
    out of it, and what is sent or approved here is the worktree's.

    Paths are relative to the worktree's root, as [git diff] names them. *)

module Target : sig
  (** What the page reviews: a worktree ([Dir None]: the session's) or a
      branch that is not checked out ([Rev]: read-only), compared with
      [base] ([None]: its default, see [git_diff]) since their merge base,
      or [direct]ly. *)
  type t =
    { reviewed : Git_diff.Target.t
    ; base : string option
    ; direct : bool
    }
  [@@deriving sexp_of, equal]

  val default : t

  (** Of the session's own worktree. *)
  val own : t -> bool
end

module Mode : sig
  type t =
    | Comment (** select code and comment on it *)
    | Edit (** edit the file's text and save it on the tool host *)
  [@@deriving sexp_of, equal]
end

module Layout : sig
  type t =
    | Unified (** the changed hunks *)
    | Full (** the whole file with the changes inline *)
    | Since
    (** what changed since the user's last action on the file (or one
        picked from its history): its snapshot then to the file now *)
  [@@deriving sexp_of, equal]
end

module Pop : sig
  (** Where the selection's Comment button shows: pixels from the code
      view's top left corner, below the selection's end. *)
  type t =
    { left : int
    ; top : int
    }
  [@@deriving sexp_of, equal]
end

module Selection : sig
  (** Code selected to comment on. *)
  type t =
    { path : string
    ; range : Review_comment.Range.t
    ; anchor : string (** its lines' text *)
    ; from : Code_selection.Point.t option
      (** selected by a line's number: the line clicked first, which
          Shift+clicks extend from *)
    ; pop : Pop.t option (** selected as text: where its button shows *)
    }
  [@@deriving sexp_of, equal]
end

module Draft : sig
  (** A comment being written on the selection. *)
  type t =
    { path : string
    ; range : Review_comment.Range.t
    ; anchor : string
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
  ; target : Target.t
  ; fallback : Target.t option
    (** what was reviewed before [target], until its diff is in *)
  ; requested : Target.t (** of the [git_diff] on its way *)
  ; own_root : string option
    (** the session's worktree, once a diff of it came: approvals from
        before worktrees were told apart are its *)
  ; all : Review.t (** every worktree's *)
  ; review : Review.t (** the reviewed worktree's ({!Review.scope}) *)
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
  ; hover : string option (** the comment pointed at: its thread or its code *)
  ; replies : string String.Map.t (** what is typed under a comment, by id *)
  ; busy : String.Set.t (** comments with a request on its way *)
  ; contents : Content.t String.Map.t
  ; editor : Editor.t option
  ; submitting : bool
  ; since : Review_since.File.t String.Map.t
    (** of the reviewed worktree's files with actions, since the last *)
  ; since_loading : bool (** a [review_since] is on its way *)
  ; since_again : bool
  ; picked : (int * Review_since.File.t option) String.Map.t
    (** an older action chosen to compare with, by file: its index in
        {!Review.history} and, once in, what changed since *)
  ; views : Layout.t String.Map.t (** the layout chosen for a file *)
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
    | Shown of string (** a path, as a branch not checked out has it *)
    | Saved of string
    | Submitted
    | Since of string (** the worktree's root *)
    | Since_picked of
        { root : string
        ; path : string
        ; index : int
        }
  [@@deriving sexp_of, equal]
end

module Out : sig
  type t =
    | Rpc of
        { method_ : Request.Method.t
        ; reply : Reply.t
        }
    | Get_diff of Target.t
    (** [git_diff] for the whole worktree, answered as [Reply.Diff] *)
    | Toast of
        { text : string
        ; error : bool
        }
    | Focus of string (** an element's id, once rendered *)
    | Clear_text_selection (** the page's: the selection is dropped *)
  [@@deriving sexp_of, equal]
end

module Action : sig
  type t =
    | Open of string option (** the page, at a file *)
    | Show of Target.t
    (** the page, reviewing that worktree against that base: from the
        first file when it is another *)
    | Close
    | Toggle
    | Go of string (** the page's file *)
    | Step of int (** the next (1) or previous (-1) file *)
    | Set_mode of Mode.t
    | Set_layout of Layout.t (** for the page's file, and the default *)
    | Set_view of
        { path : string
        ; layout : Layout.t
        } (** for that file (the file browser's), and the default *)
    | Pick_since of
        { path : string
        ; index : int (** in {!Review.history} *)
        } (** the since-view, against that action *)
    | Select_line of
        { path : string
        ; side : Review_comment.Side.t
        ; line : int
        ; extend : bool (** shift-click: from the first line clicked *)
        }
    | Select_text of
        { path : string
        ; start : Code_selection.Point.t
        ; end_ : Code_selection.Point.t
        ; pop : Pop.t option
        } (** the page's selection changed (see {!Code_selection.resolve}) *)
    | Clear_selection
    | Start_comment (** on the selection *)
    | Hover of string option (** a comment's thread or code, pointed at *)
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

(** The session's review, from its [review] event; new actions on the
    reviewed worktree's files read what changed since them again. *)
val set_review : t -> Review.t -> t * Out.t list

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

(** The branch reviewed without a checkout. *)
val read_only : t -> string option

(** What the review keys the reviewed worktree's comments and approvals by
    ({!Git_diff.review_root}), once the diff is in. *)
val review_root : t -> string option

(** [abs] relative to the root, if inside it (never for a branch not
    checked out: its files are not on disk). *)
val relative : t -> abs:string -> string option

val file : t -> string -> Prigh_ui.Diff_files.File.t option

(** How [path] shows: as chosen for it, else [Since] when it changed since
    the last action on it, else the page's layout; never [Since] without
    actions. *)
val layout_of : t -> string -> Layout.t

(** [path]'s review actions, oldest first. *)
val history : t -> string -> Review.Snapshot.t list

(** What changed in [path] since the action the since-view compares with
    (the last, or the one picked), once read. *)
val since : t -> string -> Review_since.File.t option

(** Whether [path] changed after the last action on it. *)
val changed_since : t -> string -> bool

(** The rows [path] shows: its hunks, the whole file with them inline
    ([Full]), what changed since an action ([Since]), or an unchanged file's
    lines. The same value until the file,
    its change or the layout changes. *)
val rows : t -> path:string -> Diff_view.Row.t list

(** [path]'s lines now, once read. *)
val lines : t -> string -> string array option

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
    selected code, [e] edit or comment mode, [v] the whole file or the hunks,
    [l] what changed since the last action or not,
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

(** In a comment or reply field: Enter (or Ctrl/⌘+Enter) sends, Shift+Enter
    is a new line, Esc cancels. In the editor: Ctrl/⌘+S saves. *)
val field_key
  :  t
  -> field:[ `Draft | `Reply of string | `Editor ]
  -> key:string
  -> shift:bool
  -> ctrl:bool
  -> meta:bool
  -> Action.t option
