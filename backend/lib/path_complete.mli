open! Core
open! Import

(** Path completion for the frontends' [@path] autocomplete: files and
    directories under [cwd] (up to four levels deep, [_build], [.git],
    [node_modules] and [_opam] skipped) whose relative path contains [prefix]
    case-insensitively. Directories end in [/]. Uses [fd] when available
    (respects [.gitignore]), otherwise a plain directory walk. At most 200
    sorted results. *)
val list
  :  ?use_fd:bool
  -> env:Env.t
  -> cwd:string
  -> prefix:string
  -> unit
  -> string list
