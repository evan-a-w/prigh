open! Core
open! Import

(** Markdown as DOM. Links open in a new tab; code blocks carry a copy button
    (see [code_block]). *)
(** [streaming]: [text] is still arriving (see [Markdown.parse]). *)
val render : ?streaming:bool -> string -> Node.t

(** A [.copyable] block whose [button.copy] the page wires to copy the
    [.copy-text] inside it. [closed = false] while it is still streaming. *)
val code_block : lang:string -> text:string -> closed:bool -> Node.t

(** The button [code_block] uses, for other [.copyable] blocks. *)
val copy_button : Node.t
