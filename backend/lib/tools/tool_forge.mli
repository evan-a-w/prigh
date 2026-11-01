open! Core
open! Import

(** What the git-host tools ([pull_request], [issue], [repo]) share: their
    repository is that of the working directory (or [cwd]), whose git runs
    on the session's tool host, and the API runs here, as the user's
    sign-in. *)

(** The {!Git_forge} and {!Git_location} pseudo-tools, run on the host. *)
val git_forge : Tool.t

val git_location : Tool.t
val on_host : Tool.Context.t -> cwd:string -> Tool.t -> Json.t -> Tool.Result.t

(** The [cwd] argument, resolved (default: the context's). *)
val cwd : Tool.Context.t -> Json.t -> string

(** Git on the host, in [cwd]. *)
val git : Tool.Context.t -> cwd:string -> Forges.git

(** {!Forges}' errors, reworded for the model (it cannot run slash commands
    or sign in). *)
val for_the_model : string -> string

(** The [number] argument; [what] is the missing number's description. *)
val number : what:string -> Json.t -> int

val int_list : Json.t -> string -> int list
val strings : Json.t -> string -> string list
val required_string : Json.t -> string -> why:string -> string
val repo_name : Forge_repo.t -> string

(** [s] in at most [n] lines, saying how many more there are. *)
val clip_lines : int -> string -> string
