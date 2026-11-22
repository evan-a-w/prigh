open! Core
open! Import

(** A session's project in a word, for lists: its repository's name (the
    main checkout's directory, the same for all of its worktrees), else the
    last part of the directory it works in (["~"] for the home directory
    [host_home]). *)
val name : ?host_home:string -> Session_summary.t -> cwd:string -> string
