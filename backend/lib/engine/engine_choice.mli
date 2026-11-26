open! Core
open! Import

(** The engine a session runs on and, for the composer, the adapter it
    drives (see {!Session.engine}). *)
type t =
  { engine : Engine_id.t
  ; adapter : Adapter_id.t option (** [None] for the harness *)
  }
[@@deriving sexp, equal, compare]

(** The harness: every session from before engines, and the default. *)
val harness : t

val is_harness : t -> bool

(** From the strings a session entry or a request carries. The composer
    needs an adapter, the harness takes none. *)
val of_strings : engine:string -> adapter:string option -> t Or_error.t

(** [harness], [composer (Codex)]. *)
val label : t -> string

(** The single key naming this choice: ["harness"], ["codex"], ["claude"],
    ["acp:antigravity"]. Matches [Deployed_engines.Choice.key] on the
    frontends. *)
val key : t -> string

(** ["prigh harness"], ["Codex"], ["Claude Code"], ["Antigravity"]. *)
val display_name : t -> string

(** Names the user may write in a message to explicitly ask for this engine
    or adapter (matched as whole words, case-insensitively). *)
val names : t -> string list
