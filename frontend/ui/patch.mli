open! Core

(** What an [apply_patch] call changes, file by file: parsed from its input (a
    Codex-format patch, perhaps still streaming) and, once it has succeeded,
    the unified diffs its result returns for updated files. *)

module Kind : sig
  type t =
    | Add
    | Update
    | Delete
  [@@deriving sexp_of, equal]

  (** [A], [M] or [D], as the result lists them. *)
  val letter : t -> string
end

module File : sig
  type t =
    { kind : Kind.t
    ; path : string
    ; move_to : string option
    ; input : string list
      (** The patch's lines for this file: an added file's [+] lines, an
        updated file's hunks ([@@ context], [ ], [-], [+]). *)
    ; diff : string list option
      (** An updated file's unified diff from the result, from its first [@@]
        line (with line numbers). *)
    }
  [@@deriving sexp_of]

  (** [path], or [path → move_to]. *)
  val name : t -> string

  (** What to show: [diff] if there is one, else [input]; nothing for a
      deleted file. *)
  val lines : t -> string list

  (** Lines [(added, removed)] in [lines]. *)
  val counts : t -> int * int
end

(** The files [input] touches, in order. [result] is the text of a successful
    result; [None] while the call is pending or when it failed. *)
val parse : input:string -> result:string option -> File.t list

(** Whether [text] is a successful result. *)
val is_success : string -> bool

val counts : File.t list -> int * int
