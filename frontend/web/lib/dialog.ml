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
    | Worktrees of Worktree.Listing.t
    | Themes
    | Accents
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
  | New_worktree of New_worktree.t
  | Remove_worktree of
      { worktree : Worktree.t
      ; force : bool
      ; busy : bool
      ; error : string option
      }
[@@deriving sexp_of, equal]

let per_session = function
  | Rename _
  | Prompt _
  | Rewind_confirm _
  | Create_dir _
  | Session _
  | Text _
  | Mcp_tools _
  | Loading _
  | Diff _
  | New_worktree _
  | Remove_worktree _
  | Picker
      { kind = Fork _ | Rewind _ | Tree | Hosts | Skills | Mcp _ | Modes | Jump
      ; _
      }
  | Picker { kind = Snippets | Reminders | Worktrees _; _ } -> true
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
  | Help | Hotkeys | Settings | Scoped_models _ | Delete _ | Login _ | Auth _ ->
    false
;;
