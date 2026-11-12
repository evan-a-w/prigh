open! Core
open! Import

(** A default new sessions start with, as the settings page's "New sessions"
    section changes it: each is a field of the backend's config, saved with
    [set_config]'s [defaults_only] so that no open session changes. *)

type t =
  | Model of string option (** a model key; [None]: the backend picks *)
  | Thinking of string option (** a level; [None]: the built-in, [on] *)
  | Mode of Approval_mode.t
  | Host of string option (** a host's id; [None]: where you are *)
  | Dir of string option (** [None]: where the host's sessions start *)
  | Fallback of string list (** model keys, in order *)
  | Budget of float option (** dollars; [None]: no budget *)
[@@deriving sexp_of, equal]

(** The built-in mode new sessions start in when none is set. *)
val builtin_mode : Approval_mode.t

(** The built-in thinking level. *)
val builtin_thinking : string

(** [config] with [t] applied, and the field [set_config] sends. *)
val apply : t -> Config.t -> Config.t * string

(** The pickers the section opens. *)
module Picker : sig
  type t =
    | Model (** the default model *)
    | Fallback (** a model to add to the fallback chain *)
    | Host (** the default tool host *)
  [@@deriving sexp_of, equal]
end
