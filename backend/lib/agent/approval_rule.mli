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
      [apply_patch] adds, updates, deletes or moves, with [~] as [home]
      (default: [$HOME]). *)
  val create : ?home:string -> cwd:string -> tool:string -> Json.t -> t
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

(** Whether the rule matches any part of the call. Deny and ask rules use
    this, so [cd x && rm y] is caught by [rm:*].

    For [bash]: the whole command, each part, and every command it may run
    as far as the text shows: those in substitutions ([$(rm x)], backquotes),
    subshells and braces, after keywords ([if], [then], [!], ...),
    assignments ([A=1 rm x]) and quotes or escapes ([\rm], ['rm']), under
    their base name ([/bin/rm]), behind wrappers ([sudo], [env], [xargs],
    [nice -n 5], ...: any later word that is not an option), and in the
    arguments of shells and [eval] ([bash -c 'rm x']). It cannot see
    commands built at run time ([$CMD x], [eval "$(...)"]), aliases or
    functions, scripts that are run, or files a command reads: a file rule
    such as a [read] rule for [~/.ssh] does not stop [bash] reading them.

    For file tools: any path (lexically, so a symlink to a protected file is
    not caught), and what directory tools show: an entry of [ls]'s
    directory, anything under [grep]'s and [find]'s. *)
val touches : t -> cwd:string -> home:string -> Call.t -> bool

(** The rules a "don't ask again" answer would add to cover this call and
    others like it: for [bash], [PREFIX:*] for each part of the command, the
    prefix being the program, plus its subcommand for programs such as
    [git], [npm] and [cargo]; for file edits, [edit]; for other tools, the
    tool's name. Empty when nothing sensible covers it: a command with
    substitutions or redirections, or one whose program runs other commands
    ([env], [sudo], [xargs], [bash], ...) or is written oddly ([CI=1 make],
    [\rm], [$CMD]). *)
val suggest : Call.t -> t list

(** The parts of a shell command as [covers] and [touches] see them, and
    whether it has substitutions or output redirections. Exposed for tests. *)
val segments : string -> string list * [ `Simple | `Complex ]

(** Glob matching as rules use it, exposed for tests. *)
val glob_matches : pattern:string -> string -> bool
