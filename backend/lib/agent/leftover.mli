open! Core
open! Import

(** Something a temporary session made on a tool host, deleted when the
    session ends ({!Agent.leftovers}). *)

module Kind : sig
  type t =
    | Upload (** a file attached in prigh-web ([<cwd>/.prigh/uploads/]) *)
    | Output (** a long command's full output ({!Tool_bash.save_output}) *)
    | Worktree (** a git worktree it made, removed only when clean *)
  [@@deriving sexp, jsonaf, equal, compare]
end

type t =
  { host : string (** the tool host's id *)
  ; kind : Kind.t
  ; path : string
  ; root : string
    (** where to run git for a worktree (its main checkout); for a file,
        the directory it is in *)
  }
[@@deriving sexp, jsonaf, equal, compare]
