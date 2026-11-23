open! Core

(** Every setting the control centre (Ctrl+K) offers, in one table: the
    session's (what the composer's chips and the ⋯ sheet change) and the
    settings page's. A new setting is a constructor here; the compiler then
    asks for its current value and what choosing it does ([App]), and
    [test_control_centre] checks that every row of the settings page has
    one. *)

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
  | Corners
  | Font
  | Code_font
  | Archive_after
  | Notifications
  | Providers
  | Drafting_model
  | Approval_model
  | Naming_model
  | Summary_model
  | Compact_at
  | Mask
  | Memory
  | Memory_notes
  | About_you
  | Instructions_by_model
  | Instructions
  | Mcp
  | Git_hosts
  | Engines
  | Usage
  | Auto_resume
  | Nix_store
  | Account
[@@deriving sexp_of, equal, enumerate]

(** What the palette lists it as: "Set thinking", "Default mode for new
    sessions". *)
val name : t -> string

(** Its name in a value's entry: "Thinking" in "Thinking: high". *)
val short : t -> string

(** More words the filter matches besides [name]. *)
val keywords : t -> string

(** One of the session's settings: under "This session" with nothing typed. *)
val session : t -> bool

(** Its shortcut, if it has one. *)
val keys : t -> string option

(** Its values are offered as entries of their own while typing ("Thinking:
    high"), each applied in one step. *)
val direct : t -> bool

(** Where the settings page has it, and the name of its row there; the
    section's own name for a section of rows that come and go (git hosts,
    MCP servers), which it covers as a whole. *)
val page : t -> (Settings_section.t * string) option

(** Only superusers have it. *)
val superuser_only : t -> bool

(** Only on a backend deploying the composer engine. *)
val composer_only : t -> bool

(** Its id in a row's DOM id. *)
val to_string : t -> string
