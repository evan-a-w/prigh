open! Core
open! Import

(** One permission rule, written like Claude Code's: [TOOL] or
    [TOOL(PATTERN)], e.g. ["bash(git status:*)"], ["edit(src/**)"],
    ["mcp__github__*"].

    [TOOL] is a tool name, case-insensitive, where [*] matches any characters
    (["mcp__github__*"], or ["*"] for every tool). Two names stand for families:
    [edit] also covers [write] and [apply_patch] (every file change) and
    [read] also covers [ls], [grep] and [find] (every file read).

    [PATTERN] depends on the tool:
    - [bash]: [PREFIX:*] matches a command that is [PREFIX] or starts with
      [PREFIX] and a space; anything else is a glob over the whole command
      ([*] matches any characters). A command is split at unquoted [;], [&],
      [&&], [|], [||] and newlines, and each part is matched on its own.
    - file tools: a glob over the paths the call touches ([**] matches across
      directories, [*] and [?] within a name); a relative pattern is under
      the session's cwd, [~/] is the home directory.
    - other tools take no pattern; one given never matches. *)
type t [@@deriving sexp_of, equal, compare]

(** Fails, saying what is wrong, on empty names, unbalanced parentheses and
    empty patterns. *)
val of_string : string -> t Or_error.t

(** Canonical form ([TOOL] lowercased, [TOOL(PATTERN)]); [of_string] reads it
    back. *)
val to_string : t -> string

(** What a rule is checked against. *)
module Call : sig
  type t =
    { tool : string
    ; command : string option (** [bash]'s command *)
    ; paths : string list
      (** absolute paths a file tool touches; empty for other tools *)
    }
  [@@deriving sexp_of]

  (** From a tool call's arguments: [bash]'s [command]; [path] (default the
      cwd for [ls], [grep], [find]) of the file tools; the files an
      [apply_patch] adds, updates, deletes or moves. *)
  val create : cwd:string -> tool:string -> Json.t -> t
end

(** Whether the rules together cover the whole call: a rule without a
    pattern covers every call of its tools; otherwise every part must match
    some rule. For [bash], the parts are the command's segments, so [cd x &&
    rm y] is not covered by [cd:*] alone, but is by [cd:*] and [rm:*];
    comments are left out. Commands with substitutions ([$(...)],
    backquotes, [<(...)]), ANSI-C quotes ([$'...']), here-documents,
    parentheses (subshells, function definitions) or output redirections to
    files ([>], [>&FILE]) are never covered by patterns, so allow rules
    cannot let one through. For file tools, the parts are the paths. *)
val covers : t list -> cwd:string -> home:string -> Call.t -> bool

(** Whether the rule matches any part of the call: for [bash], any part of
    the command (with substitutions too); for file tools, any path. Deny and
    ask rules use this, so [cd x && rm y] is caught by [rm:*]. *)
val touches : t -> cwd:string -> home:string -> Call.t -> bool

(** The rules a "don't ask again" answer would add to cover this call and
    others like it: for [bash], [PREFIX:*] for each part of the command, the
    prefix being the program, plus its subcommand for programs such as
    [git], [npm] and [cargo]; for file edits, [edit]; for other tools, the
    tool's name. Empty when nothing sensible covers it (a command with
    substitutions or redirections). *)
val suggest : Call.t -> t list

(** The parts of a shell command as [covers] and [touches] see them, and
    whether it has substitutions or output redirections. Exposed for tests. *)
val segments : string -> string list * [ `Simple | `Complex ]

(** Glob matching as rules use it, exposed for tests. *)
val glob_matches : pattern:string -> string -> bool
