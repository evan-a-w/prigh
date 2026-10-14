open! Core
open! Import

(** Small DOM helpers shared by the views. *)

val div : ?cls:string -> ?attrs:Attr.t list -> Node.t list -> Node.t
val span : ?cls:string -> ?attrs:Attr.t list -> string -> Node.t
val classes : string list -> (string * bool) list -> Attr.t

(** [title] is the tooltip: the hint (its key, its command, more words).
    The accessible name is its text, or [label] (what it does) for a button
    with only an icon. *)
val button
  :  ?cls:string
  -> ?title:string
  -> ?label:string
  -> ?disabled:bool
  -> ?attrs:Attr.t list
  -> on_click:unit Effect.t
  -> Node.t list
  -> Node.t

(** The sodium lamp that glows wherever work is in progress (a running
    session, tool, agent or request); [title] says what works. *)
val lamp : ?title:string -> unit -> Node.t

(** A checkbox's box (a radio button's circle with [radio]), for rows that
    say their state with [aria-checked] or [aria-selected]. *)
val check : ?radio:bool -> bool -> Node.t

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
    | Terminal
    | Restart
    | Target
    | Clock
    | Archive
    | Unarchive
    | Bell
    | Bell_off
    | Shield
    | Sliders
    | Cross
    | Dash
    | Square
    | Warning
    | Next
    | Ring
    | Toc

  (** An inline SVG; tests show [<icon name>]. *)
  val view : ?cls:string -> t -> Node.t
end

val icon : ?cls:string -> Icon.t -> Node.t

(** How something stands or ended (done, failed, stopped, ...), in front of
    its line: [icon] in a square box to centre on the line's first row,
    coloured by [cls] ([done], [failed], [stopped], [now]). [label] says it
    in words (to screen readers, and as its tooltip); without one it is
    hidden from screen readers, for lines that say it themselves. *)
val mark : ?cls:string -> ?label:string -> Icon.t -> Node.t

(** The caret of the [<textarea>] or [<input>] an input event came from. *)
val caret : Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> int option
