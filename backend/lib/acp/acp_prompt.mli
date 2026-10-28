open! Core
open! Import

(** An ACP prompt ([session/prompt]'s content blocks) as a prigh prompt.

    - [text] blocks are the text, joined in order.
    - A [resource_link] to a [file://] URI is mentioned as [@path] and the
      file attached (prigh reads it through the session's tool host, so an
      editor's unsaved contents win); another URI is mentioned as a
      markdown link.
    - An embedded text [resource] is mentioned as [@path] and its contents
      inlined as a [<file path="...">] block (what prigh appends for an
      attachment); an image blob is attached as an image.
    - A URI ending in [#L<start>-L<end>] (or [#L<line>]) is a selection: the
      mention says which lines.
    - [image] blocks are attached as images. *)

type t =
  { text : string
  ; attachments : string list (** absolute paths *)
  ; images : Json.t list (** [{mime_type, data}], as prigh's RPC takes them *)
  }
[@@deriving sexp_of]

(** Fails, saying what is not supported, for audio and unknown blocks. *)
val of_blocks : Json.t list -> t Or_error.t
