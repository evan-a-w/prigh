open! Core
open! Import

(** The editor mode's page: the strip (the tree's ☰, the file, ⋯), the open
    files' tabs (a wide window) or dots (a phone), the file tree (docked, or a
    drawer on a phone), the editor, the agent's change and conflicts, a
    phone's peek at the chat and its key bar over the keyboard. *)

module Input : sig
  (** What the page's editor widget shows, and where it reports. *)
  type t =
    { path : string
    ; lang : Code_editor.Lang.t
    ; text : string
    ; revision : int
    ; hunks : Code_editor.Hunk.t list
    ; marks : (int * int) list
    ; on_change : path:string -> string -> unit Effect.t
    ; on_select : Code_editor.Selection.t -> unit Effect.t
    ; on_scroll : [ `Down | `Up ] -> unit Effect.t
    }

  val sexp_of_t : t -> Sexp.t
end

val view
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> widget:(Input.t -> Node.t)
  -> Node.t

(** The code the next prompt is about, a chip in the composer. *)
val chip : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
