open! Core

(** Syntax highlighting: a small tokenizer per language, line by line with
    the state that carries across lines (block comments, multi-line strings,
    Markdown fences, an HTML tag's attributes). Total and approximate: any
    text tokenizes, and the tokens' texts concatenate to the input. *)

module Kind : sig
  type t =
    | Plain
    | Keyword
    | Type (** types, modules, constructors, builtins *)
    | String
    | Number
    | Constant (** [true], [None], [FOO] *)
    | Comment
    | Property (** keys: JSON, YAML, TOML, CSS properties, HTML attributes *)
    | Variable (** [$x] in shell, [--x] in CSS, YAML anchors *)
    | Meta (** attributes, decorators, preprocessor, macros, fences *)
    | Tag (** HTML tags, CSS selectors, TOML tables *)
    | Heading
    | Emph
    | Strong
    | Link
    | Added
    | Removed
  [@@deriving sexp_of, equal, enumerate]

  (** Its CSS class, [hl-...] ([""] for [Plain]). *)
  val css : t -> string
end

module Token : sig
  type t =
    { kind : Kind.t
    ; text : string
    }
  [@@deriving sexp_of, equal]
end

module Lang : sig
  type t =
    | Ocaml
    | Javascript (** and TypeScript, JSX *)
    | Python
    | Rust
    | Go
    | C (** and C++ *)
    | Shell
    | Json
    | Yaml
    | Toml
    | Markdown
    | Html (** and XML, SVG *)
    | Css
    | Nix
    | Sql
    | Sexp (** dune, Lisp, Scheme *)
    | Diff
  [@@deriving sexp_of, equal, enumerate]

  (** A fence's info string ([ocaml], [ts], [```{.python}]...). *)
  val of_name : string -> t option

  (** By extension or file name ([main.ml], [dune], [flake.lock]...). *)
  val of_path : string -> t option
end

module State : sig
  type t [@@deriving sexp_of, equal]

  val initial : t
end

(** One line (without its newline), from [state]: its tokens and the state
    for the next line. *)
val line : Lang.t -> State.t -> string -> Token.t list * State.t

(** A whole text, newlines included (in [Plain] tokens), with adjacent
    [Plain] tokens merged. *)
val tokens : Lang.t -> string -> Token.t list
