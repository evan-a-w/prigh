open! Core
open! Import

(** What the git hosting commands need from the repository on a tool host,
    as the pseudo-tool [op] (its [action] argument picks one):
    - [info]: the top directory, current branch, HEAD and remotes;
    - [log {remote, base}]: the commits on HEAD not on the base, and whether
      the branch is on the remote with all of them (from the remote-tracking
      refs: nothing is fetched);
    - [push {remote, branch}]: [git push -u] (never prompts);
    - [checkout {remote, number, worktrees_dir}]: fetches
      [refs/pull/N/head] (GitHub and Forgejo both publish it) into the
      branch [pr-N], in its {!Worktree} (made where prigh makes them, or
      the one it is checked out in already, fast-forwarded). *)

module Remote : sig
  type t =
    { name : string
    ; urls : string list
      (** as configured, then as git rewrites it ([url.*.insteadOf]) *)
    }
  [@@deriving sexp, jsonaf]
end

module Info : sig
  type t =
    { root : string
    ; branch : string option (** [None]: detached *)
    ; upstream_remote : string option (** [branch.<name>.remote] *)
    ; head : string option (** [None] before the first commit *)
    ; remotes : Remote.t list
    }
  [@@deriving sexp, jsonaf]
end

module Log : sig
  type t =
    { commits : (string * string) list (** subject, body; oldest first *)
    ; pushed : bool
    }
  [@@deriving sexp, jsonaf]
end

module Checkout : sig
  type t =
    { path : string
    ; branch : string
    ; created : bool
    }
  [@@deriving sexp, jsonaf]
end

module Request : sig
  type t =
    | Info
    | Log of
        { remote : string
        ; base : string
        ; branch : string
        }
    | Push of
        { remote : string
        ; branch : string
        }
    | Checkout of
        { remote : string
        ; number : int
        ; worktrees_dir : string (** {!Config.t.worktrees_dir} *)
        }
  [@@deriving sexp_of]

  val to_json : t -> Json.t
end

val op : string

(** On the host. *)
val run
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> arguments:Json.t
  -> Tool_result.t

(** The [run]'s result as [of_json] reads it, or its error. *)
val result : Tool_result.t -> of_json:(Json.t -> 'a) -> 'a Or_error.t
