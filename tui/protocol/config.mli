open! Core

(** TUI mirror of the backend's persisted configuration. *)

(** [permissions]: the allow/ask/deny rules, as written. *)
module Permissions : sig
  type t =
    { allow : string list
    ; ask : string list
    ; deny : string list
    }
  [@@deriving sexp_of, equal]

  val empty : t
end

type t =
  { scoped_models : string list
  ; approval_mode : Approval_mode.t
  ; permissions : Permissions.t
    (** the mode new sessions start in ([confirm_tools] from older backends) *)
  ; default_model : string option
  ; default_thinking : string option
  ; fallback_models : string list
    (** model keys that take over, in order, when a model's usage runs out *)
  ; default_cwd : string option (** where new sessions start *)
  ; archive_after_days : float
    (** sessions inactive this long show as archived; [0.]: never *)
  ; notify : string
    (** how the terminal frontend notifies: [off], [bell] (default) or
        [desktop] *)
  ; snippets : Snippet.t list (** [/snippet]'s, shared by every frontend *)
  }
[@@deriving sexp_of, equal]

val default : t
val of_json : Json.t -> t Or_error.t
val to_json : t -> Json.t
