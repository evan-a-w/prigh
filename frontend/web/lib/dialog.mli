open! Core
open! Import

(** The modal dialog that owns the keyboard (tool confirmations and the
    model's questions are separate: they queue up and show above it). *)

module Picker_kind : sig
  type t =
    | Models
    | Thinking
    | Login (** item ids are [provider method] *)
    | Logout
    | Verbosity
    | Confirm_tools
    | Modes (** item ids are [Session_mode.label]s *)
    | Fork of Entry.t list (** item ids are entry ids *)
    | Rewind of Entry.t list
    | Tree
    | Hosts (** item ids are host ids *)
    | Users (** act as one ([/setusr]) *)
    | Accounts (** the account menu: see [App] for the item ids *)
    | Skills (** item ids are skill names *)
    | Snippets (** item ids are snippet names *)
    | Reminders (** the session's ([/remind]); item ids are their names *)
    | Mcp of Mcp_list.t (** item ids are [Mcp_servers.id]s *)
    | Jump (** item ids are [Jump.Target.to_string]s *)
    | Worktrees of Worktree.Listing.t (** item ids are their paths *)
    | Themes (** item ids are [Theme.Name.to_string]s *)
    | Accents (** item ids are [Theme.Accent.to_string]s *)
    | Menu of Context_menu.t
    (** a session's or worktree's context menu; item ids are
        [Context_menu.Item.id]s *)
  [@@deriving sexp_of, equal]
end

module Delete_target : sig
  type t =
    | Sessions of string list (** paths *)
    | Archived (** every session shown as archived, when confirmed *)
  [@@deriving sexp_of, equal]
end

type t =
  | Picker of
      { kind : Picker_kind.t
      ; picker : Picker.t
      }
  | Popover of
      { chip : Chip.t
      ; picker : Picker.t
      } (** a composer chip's menu, over the chip *)
  | Review_pick of Review_picker.t
  (** the review page's menu of what to review or compare with, over its
      button *)
  | Help
  | Rename of string
  | Delete of Delete_target.t
  | Login of Login_flow.t
  | Auth of Auth_status.t list
  | Hotkeys
  | Settings
  (** [/settings]: notifications, the transcript, tool confirmation, the
      account, providers and keys *)
  | Scoped_models of
      { picker : Picker.t
      ; checked : String.Set.t (** model keys *)
      }
  | Prompt of Prompt.t
  | Rewind_confirm of
      { id : string
      ; text : string
      }
  | Create_dir of
      { target : Dir_change.Target.t
      ; path : string (** absolute, on the target's host *)
      ; busy : bool (** creating: waiting for the backend *)
      ; error : string option
      } (** a missing directory given to [/cd] or [/host]: create it? *)
  | Session of Session_stats.t
  | Text of
      { title : string
      ; text : string
      }
  | Mcp_tools of Mcp_server.t (** a ready server's tools *)
  | Loading of string
  (** the title of a dialog waiting for its list from the backend *)
  | Diff of Git_diff.t (** [/diff] *)
  | Pulls of Pull_requests.t (** [/pr]: the repository's pull requests *)
  | Pull of Pr_page.t (** one of them *)
  | Pr_create of Pr_create.t (** [/pr new] *)
  | Pr_checked_out of
      { number : int
      ; checkout : Pr_checkout.t
      } (** [/pr checkout N] done: work there? *)
  | Forge_host of
      { input : string
      ; error : string option
      } (** [/login forgejo]: which host? *)
  | New_worktree of New_worktree.t
  | Remove_worktree of
      { worktree : Worktree.t
      ; repo : string option
        (** its repository when not the session's: removed from the tree *)
      ; force : bool (** also with uncommitted changes ([rm -f]) *)
      ; busy : bool (** removing: waiting for the backend *)
      ; error : string option
      } (** asked before removing it *)
[@@deriving sexp_of, equal]

(** Belongs to the session (closed when switching). *)
val per_session : t -> bool
