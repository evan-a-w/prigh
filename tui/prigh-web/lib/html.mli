open! Core
open! Import

(** Small DOM helpers shared by the views. *)

val div : ?cls:string -> ?attrs:Attr.t list -> Node.t list -> Node.t
val span : ?cls:string -> ?attrs:Attr.t list -> string -> Node.t
val classes : string list -> (string * bool) list -> Attr.t

val button
  :  ?cls:string
  -> ?title:string
  -> ?disabled:bool
  -> ?attrs:Attr.t list
  -> on_click:unit Effect.t
  -> Node.t list
  -> Node.t

module Icon : sig
  type t =
    | Menu
    | Sidebar
    | Plus
    | Search
    | Trash
    | Pencil
    | Close
    | Send
    | Stop
    | Chevron
    | Arrow_down
    | Brain
    | Folder
    | Branch
    | Cpu
    | Logout
    | Undo
    | External
    | Key
    | Server
    | Help
    | Bot
    | Back
    | Locate
    | User
    | Users
    | User_plus
    | Copy
    | Check

  (** An inline SVG; tests show [<icon name>]. *)
  val view : ?cls:string -> t -> Node.t
end

val icon : ?cls:string -> Icon.t -> Node.t

(** The caret of the [<textarea>] or [<input>] an input event came from. *)
val caret : Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> int option
