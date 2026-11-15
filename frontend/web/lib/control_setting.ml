open! Core

type t =
  | Model
  | Thinking
  | Verbosity
  | Host
  | Mode
  | Context
  | Default_model
  | Default_thinking
  | Default_mode
  | Default_host
  | Default_directory
  | Fallback_models
  | Budget
  | Theme
  | Accent
  | Worktree_hours
  | Archive_after
  | Notifications
  | Providers
  | Drafting_model
  | Approval_model
  | Naming_model
  | Summary_model
  | Dictation_model
  | Compact_at
  | Mask
  | Memory
  | Memory_notes
  | About_you
  | Instructions_by_model
  | Instructions
  | Mcp
  | Git_hosts
  | Nix_store
  | Account
[@@deriving sexp_of, equal, enumerate]

let name = function
  | Model -> "Set model"
  | Thinking -> "Set thinking"
  | Verbosity -> "Set verbosity"
  | Host -> "Switch host"
  | Mode -> "Set mode"
  | Context -> "Context and cost"
  | Default_model -> "Default model for new sessions"
  | Default_thinking -> "Default thinking for new sessions"
  | Default_mode -> "Default mode for new sessions"
  | Default_host -> "Default tool host for new sessions"
  | Default_directory -> "Starting directory for new sessions"
  | Fallback_models -> "Fallback models"
  | Budget -> "Budget for new sessions"
  | Theme -> "Theme"
  | Accent -> "Accent colour"
  | Worktree_hours -> "Show worktrees"
  | Archive_after -> "Archive sessions after"
  | Notifications -> "Notifications"
  | Providers -> "Model providers"
  | Drafting_model -> "Drafting model"
  | Approval_model -> "Approval model"
  | Naming_model -> "Naming model"
  | Summary_model -> "Summary model"
  | Dictation_model -> "Dictation model"
  | Compact_at -> "Compact at"
  | Mask -> "Mask old tool outputs"
  | Memory -> "Use memory"
  | Memory_notes -> "Memory notes"
  | About_you -> "About you"
  | Instructions_by_model -> "Let the model change custom instructions"
  | Instructions -> "Custom instructions"
  | Mcp -> "MCP servers"
  | Git_hosts -> "Git hosts"
  | Nix_store -> "Nix store"
  | Account -> "Account"
;;

let short = function
  | Model -> "Model"
  | Thinking -> "Thinking"
  | Verbosity -> "Verbosity"
  | Host -> "Host"
  | Mode -> "Mode"
  | Default_thinking -> "New sessions' thinking"
  | Default_mode -> "New sessions' mode"
  | Theme -> "Theme"
  | Accent -> "Accent"
  | Notifications -> "Notifications"
  | Mask -> "Mask old tool outputs"
  | Memory -> "Memory"
  | Instructions_by_model -> "Model changes instructions"
  | t -> name t
;;

let keywords = function
  | Model -> "switch llm provider"
  | Thinking -> "reasoning effort level"
  | Verbosity -> "transcript detail quiet normal verbose session"
  | Host -> "tool host where tools run machine directory"
  | Mode -> "approvals ask auto-edit smart auto plan chat confirm"
  | Context -> "tokens usage cost session statistics"
  | Default_model -> "default new sessions"
  | Default_thinking -> "default new sessions reasoning"
  | Default_mode -> "default chat approvals new sessions confirm"
  | Default_host -> "default new sessions"
  | Default_directory -> "default dir cwd new sessions"
  | Fallback_models -> "default new sessions"
  | Budget -> "default cost dollars limit"
  | Theme -> "appearance dark light"
  | Accent -> "appearance color colour"
  | Worktree_hours -> "sidebar session list fold"
  | Archive_after -> "auto-archive session list days"
  | Notifications -> "notify alerts"
  | Providers -> "login logout keys auth"
  | Drafting_model -> "generate commit pull request"
  | Approval_model -> "smart mode judge"
  | Naming_model -> "titles names"
  | Summary_model -> "compaction summaries"
  | Dictation_model -> "speech microphone transcription"
  | Compact_at -> "compaction tokens"
  | Mask -> "compaction observations"
  | Memory -> "remember notes"
  | Memory_notes -> "remember"
  | About_you -> "user info name country units"
  | Instructions_by_model -> "custom instructions"
  | Instructions -> "standing preferences"
  | Mcp -> "tools servers"
  | Git_hosts -> "github forgejo forge git hosting sign in"
  | Nix_store -> "garbage collect disk"
  | Account -> "sign out user"
;;

let session = function
  | Model | Thinking | Verbosity | Host | Mode | Context -> true
  | _ -> false
;;

let keys = function
  | Model -> Some "Ctrl+L"
  | Thinking -> Some "Alt+T"
  | Verbosity -> Some "Ctrl+O"
  | Mode -> Some "Alt+M"
  | _ -> None
;;

let direct = function
  | Thinking | Verbosity | Mode | Theme | Default_mode | Default_thinking -> true
  | _ -> false
;;

let page t : (Settings_section.t * string) option =
  match t with
  | Model | Thinking | Host | Mode | Context -> None
  | Verbosity -> Some (Appearance, "Transcript detail")
  | Theme -> Some (Appearance, "Theme")
  | Accent -> Some (Appearance, "Accent")
  | Worktree_hours -> Some (Sessions, "Show worktrees")
  | Archive_after -> Some (Sessions, "Archive after")
  | Notifications -> Some (Sessions, "Notifications")
  | Default_model -> Some (New_sessions, "Model")
  | Default_thinking -> Some (New_sessions, "Thinking")
  | Default_mode -> Some (New_sessions, "Mode")
  | Default_host -> Some (New_sessions, "Tool host")
  | Default_directory -> Some (New_sessions, "Starting directory")
  | Fallback_models -> Some (New_sessions, "Fallback models")
  | Budget -> Some (New_sessions, "Budget")
  | Providers -> Some (Models, "Model providers")
  | Drafting_model -> Some (Models, "Drafting")
  | Approval_model -> Some (Models, "Approvals")
  | Naming_model -> Some (Models, "Naming")
  | Summary_model -> Some (Models, "Summaries")
  | Dictation_model -> Some (Models, "Dictation")
  | Compact_at -> Some (Models, "Compact at")
  | Mask -> Some (Models, "Mask old tool outputs")
  | Memory -> Some (Memory, "Use memory")
  | Memory_notes -> Some (Memory, "Your notes")
  | About_you -> Some (About_you, "About you")
  | Instructions_by_model ->
    Some (Custom_instructions, "Let the model change them")
  | Instructions -> Some (Custom_instructions, "Your instructions")
  | Mcp -> Some (Mcp, "MCP servers")
  | Git_hosts -> Some (Git_hosting, "Git hosting")
  | Nix_store -> Some (Nix_store, "Nix store")
  | Account -> Some (Account, "Account")
;;

let superuser_only = function
  | Nix_store -> true
  | _ -> false
;;

let to_string t =
  String.lowercase (Sexp.to_string (sexp_of_t t))
  |> String.tr ~target:'_' ~replacement:'-'
;;
