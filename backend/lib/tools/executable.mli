open! Core
open! Import

(** Finding the program a command names, as a shell would, and saying why a
    program ended. *)

(** This process's environment ([KEY=VALUE] entries) with [extra] added,
    replacing variables of the same name. *)
val environment : extra:(string * string) list -> string list

(** A regular file we may execute. *)
val is_executable : string -> bool

module Missing : sig
  type t =
    | Not_executable of string (** a path that is not an executable file *)
    | Not_on_path (** a bare name that no directory on [PATH] has *)
  [@@deriving sexp_of]
end

(** [command] with a [/] is a path, relative to [dir]; a bare name is looked
    up on [environment]'s [PATH] (default [/usr/bin:/bin]). *)
val resolve
  :  environment:string list
  -> dir:string
  -> string
  -> (string, Missing.t) Result.t

(** Where a login shell ([bash -l]) finds [name], for programs installed
    where only shell profiles put them on [PATH] (npm's global bin,
    [~/.local/bin]). [None] when it finds none or takes over 3s. *)
val login_shell_path : env:Env.t -> string -> string option

(** ["exited (code 1)"], ["was killed (sigkill)"]. *)
val describe_status : [ `Exited of int | `Signaled of int ] -> string
