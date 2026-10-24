open! Core
open! Import

(** Codex's [apply_patch] format: a file-oriented diff that GPT models are
    trained to write.

    {v
*** Begin Patch
*** Add File: path/new.ml
+line
*** Delete File: path/old.ml
*** Update File: path/file.ml
*** Move to: path/renamed.ml
@@ let f x =
 context
-removed
+added
*** End of File
*** End Patch
    v} *)

(** The Lark grammar of the format, for OpenAI custom (freeform) tools. *)
val grammar : string

(** The tool description given to the model. *)
val description : string

(** One hunk of an [Update File]: [context] is the optional text after [@@]
    (a line such as a function header that locates the hunk), [old_lines]
    the context and removed lines, [new_lines] the context and added lines,
    [end_of_file] whether it ended with [*** End of File]. *)
module Chunk : sig
  type t =
    { context : string option
    ; old_lines : string list
    ; new_lines : string list
    ; end_of_file : bool
    }
  [@@deriving sexp_of]
end

module Operation : sig
  type t =
    | Add of
        { path : string
        ; contents : string
        }
    | Delete of { path : string }
    | Update of
        { path : string
        ; move_to : string option
        ; chunks : Chunk.t list
        }
  [@@deriving sexp_of]

  (** The path the operation reads or creates ([Update]'s source path). *)
  val path : t -> string
end

(** Parses a patch. Tolerates surrounding whitespace, a missing final newline
    and a [<<'EOF' ... EOF] heredoc wrapper. Errors give the line number and
    say what was expected. *)
val parse : string -> Operation.t list Or_error.t

(** The paths an unparsed patch touches, from its [*** ... File:] and
    [*** Move to:] headers, best effort (for confirmation prompts and
    display). *)
val paths : string -> string list

module Change : sig
  type t =
    | Added of
        { path : string
        ; contents : string
        }
    | Deleted of
        { path : string
        ; before : string
        }
    | Updated of
        { path : string
        ; move_to : string option
        ; before : string
        ; after : string
        }
  [@@deriving sexp_of]
end

(** Computes the result of every operation without touching the filesystem:
    [read path] gives a file's contents ([None] if it does not exist). Paths
    are passed to [read] exactly as written in the patch. Hunks are located
    in order, as Codex does: exact lines first, then ignoring trailing
    whitespace, then surrounding whitespace, then with typographic
    punctuation (dashes, quotes, non-breaking spaces) normalised; [@@]
    context lines move the search start past the matching line. Fails,
    naming the file and the lines that were not found, if any operation
    cannot be applied (an updated or deleted file that does not exist, a
    hunk that does not match). Adding a file that exists replaces it (an
    [Updated] change, as Codex overwrites it). Several operations on the
    same file are applied in sequence: [file path] names the file a path
    means (default: the path itself), so that e.g. [a.txt] and [./a.txt]
    are one file. *)
val apply
  :  ?file:(string -> string)
  -> Operation.t list
  -> read:(string -> string option)
  -> Change.t list Or_error.t
