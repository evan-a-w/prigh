open! Core
open! Import

module Picker_kind = struct
  type t =
    | Models
    | Thinking
    | Login
    | Logout
    | Verbosity
    | Confirm_tools
    | Modes
    | Fork of Entry.t list
    | Rewind of Entry.t list
    | Tree
    | Hosts
    | Users
    | Accounts
    | Skills
    | Snippets
    | Reminders
    | Mcp of Mcp_list.t
    | Jump
    | Trim of Trim.Point.t list
    | Delete_message of Message_point.t list
    | Trim_after of Message_point.t list
    | Worktrees of Worktree.Listing.t
    | Themes
    | Accents
    | Menu of Context_menu.t
  [@@deriving sexp_of, equal]
end

module Delete_target = struct
  type t =
    | Sessions of string list
    | Archived
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
      }
  | Review_pick of Review_picker.t
  | Help
  | Rename of string
  | Delete of Delete_target.t
  | Login of Login_flow.t
  | Auth of Auth_status.t list
  | Hotkeys
  | Settings
  | Scoped_models of
      { picker : Picker.t
      ; checked : String.Set.t
      }
  | Prompt of Prompt.t
  | Rewind_confirm of
      { id : string
      ; text : string
      }
  | Trim_confirm of
      { point : Trim.Point.t
      ; busy : bool
      ; error : string option
      }
  | Trim_after_confirm of
      { point : Message_point.t
      ; busy : bool
      ; error : string option
      }
  | Create_dir of
      { target : Dir_change.Target.t
      ; path : string
      ; busy : bool
      ; error : string option
      }
  | Session of Session_stats.t
  | Text of
      { title : string
      ; text : string
      }
  | Mcp_tools of Mcp_server.t
  | Loading of string
  | Diff of Git_diff.t
  | Pulls of Pull_requests.t
  | Pull of Pr_page.t
  | Pr_create of Pr_create.t
  | Pr_checked_out of
      { number : int
      ; checkout : Pr_checkout.t
      }
  | Forge_host of
      { input : string
      ; error : string option
      }
  | New_worktree of New_worktree.t
  | Remove_worktree of
      { worktree : Worktree.t
      ; repo : string option
        (** its repository when not the session's: removed from the tree *)
      ; force : bool
      ; busy : bool
      ; error : string option
      }
  | Nix_gc_confirm of Nix_generations.t
[@@deriving sexp_of, equal]

let per_session = function
  | Popover _
  | Review_pick _
  | Rename _
  | Prompt _
  | Rewind_confirm _
  | Trim_confirm _
  | Trim_after_confirm _
  | Create_dir _
  | Session _
  | Text _
  | Mcp_tools _
  | Loading _
  | Diff _
  | Pulls _
  | Pull _
  | Pr_create _
  | Pr_checked_out _
  | New_worktree _
  | Remove_worktree _
  | Picker
      { kind =
          ( Fork _
          | Rewind _
          | Tree
          | Hosts
          | Skills
          | Mcp _
          | Modes
          | Jump
          | Trim _
          | Delete_message _
          | Trim_after _ )
      ; _
      }
  | Picker { kind = Snippets | Reminders | Worktrees _ | Menu _; _ } -> true
  | Picker
      { kind =
          ( Models
          | Thinking
          | Login
          | Logout
          | Verbosity
          | Confirm_tools
          | Users
          | Accounts
          | Themes
          | Accents )
      ; _
      }
  | Help
  | Hotkeys
  | Settings
  | Scoped_models _
  | Delete _
  | Login _
  | Auth _
  | Forge_host _
  | Nix_gc_confirm _ -> false
;;
