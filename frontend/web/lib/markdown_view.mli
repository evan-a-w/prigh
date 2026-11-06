open! Core
open! Import

(** Markdown as DOM. Links open in a new tab; code blocks are [.copyable]:
    their [button.copy] copies their [.copy-text] (see the page's listeners).
    [streaming]: [text] is still arriving (see [Markdown.parse]). *)
val render : ?streaming:bool -> string -> Node.t

(** A code block as [render] draws a fenced one, with [title] in its head
    before the language. [closed]: its fence is closed. *)
val code_block : ?title:string -> lang:string -> text:string -> closed:bool -> unit -> Node.t
