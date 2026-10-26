open! Core
open! Import

(** [/worktree]'s arguments, shared by the terminal and browser frontends:
    [/worktree] (or [list]) picks among the repository's worktrees,
    [/worktree new BRANCH [BASE]] creates one at the standard location and
    moves the session there, [/worktree open [-n] TARGET] moves the session
    there ([-n]: starts a new session there instead), [/worktree back
    [main]] moves it back where it switched into worktrees from (its
    home; [main]: the repository's main checkout instead), and
    [/worktree rm [-f] TARGET] removes one once confirmed ([-f]: also with
    uncommitted changes). A [TARGET] is a worktree's branch, path or
    directory name, as the list shows them. *)

module Action : sig
  type t =
    | List (** [list_worktrees], then a picker *)
    | New of
        { branch : string
        ; base : string option
        }
    | Open of
        { target : string
        ; new_session : bool
        }
    | Remove of
        { target : string
        ; force : bool
        }
    | Back of { main : bool } (** [Enter_worktree]'s way back *)
    | Error of string
  [@@deriving sexp_of, equal]
end

val usage : string

(** [/worktree back] in a session without a home. *)
val no_home : string

(** What [/worktree back] says once in [cwd]: [back in main (~/repo)]. *)
val back_notice
  :  P.Session_home.t option
  -> cwd:string option
  -> tilde:(string -> string)
  -> string

val parse : string -> Action.t

(** The worktree [target] names: by branch, path, or the end of its path
    (its directory name, say); otherwise an error naming the closest
    ones. *)
val find : P.Worktree.Listing.t -> string -> (P.Worktree.t, string) Result.t

(** The [/worktree open]/[rm] argument that names [w]: its branch, else its
    path. *)
val target : P.Worktree.t -> string

(** [branch · path · 3 changed · ↑1 ↓2 · 2 sessions], for pickers. *)
val describe : P.Worktree.t -> string

(** [*] with uncommitted changes, [+] current, as a short marker. *)
val marker : P.Worktree.t -> string

(** What removing [w] asks before doing it: its changes, the sessions in it
    and that the branch stays. *)
val confirm_remove : P.Worktree.t -> force:bool -> string

(** Why [w] cannot be removed from this session, before asking: the main
    worktree, or the session's own. *)
val cannot_remove : P.Worktree.t -> string option

(** The notices after each action. *)
val created : P.Worktree.Created.t -> string

val removed : P.Worktree.t -> string

(** What [rest] (all after [/worktree ]) completes: the offset in [rest] of
    the word being completed and the candidates [(id, detail)]: the
    subcommands, then the worktrees' targets after [open] and [rm] (and
    their flags), or branches for [new]'s base. *)
val complete
  :  worktrees:P.Worktree.t list
  -> string
  -> (int * (string * string) list) option
