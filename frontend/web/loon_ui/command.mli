(** Fast, composable, unstyled command menu. Port of shadcn/ui's [Command]
    (cmdk: typing in the input filters the items, ranked by cmdk's
    command-score, groups ordered by their best item; ArrowUp/ArrowDown
    (Ctrl+N/P, Ctrl+J/K), Home/End move the selection; Enter or a click runs
    the item's [on_select]; the pointer selects the item under it).

    Core has a [Command] module too: write [Loon.Command], or open [Loon]
    after Core. In a file that opens Core, alias it first:
    [module Cmdk = Command] before [open! Core].

    {[
      Command.component
        (return
           [ Command.input ~placeholder:"Type a command or search..." ()
           ; Command.list
               [ Command.empty [ Node.text "No results found." ]
               ; Command.group ~heading:"Suggestions"
                   [ Command.item [ Node.text "Calendar" ] ]
               ]
           ])
        graph
    ]} *)

open! Import

module State : sig
  (** The search, and the selected item's value ([None]: the first item). *)
  type t =
    { search : string
    ; value : string option
    }
  [@@deriving sexp_of, equal]

  val empty : t
end

(** A part of the menu: the input, the list, and inside the list groups,
    items, separators and the empty message. *)
type part

(** Controlled. [id] (unique on the page) prefixes the parts' ids; [label]
    names the input for screen readers. [should_filter] (default [true]):
    filter and sort the items by the search (else the caller does).
    [filter] replaces cmdk's ranking (a score in [0, 1], 0 hiding the
    item). [loop] (default [false]): the arrows wrap round. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> ?label:string
  -> ?should_filter:bool
  -> ?filter:(string -> search:string -> keywords:string list -> float)
  -> ?loop:bool
  -> state:State.t
  -> on_change:(State.t -> unit Effect.t)
  -> part list
  -> Node.t

(** Keeps the state, starting empty. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?label:string
  -> ?should_filter:bool
  -> ?filter:(string -> search:string -> keywords:string list -> float)
  -> ?loop:bool
  -> part list Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** The search box, with its icon. [auto_focus]: focus it when it appears
    (React's [autoFocus]). *)
val input
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?placeholder:string
  -> ?disabled:bool
  -> ?auto_focus:bool
  -> unit
  -> part

(** The scrolling list of results. *)
val list : ?attrs:Attr.t list -> ?class_:string -> part list -> part

(** Shown when no item matches the search. *)
val empty : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> part

(** Items under a [heading]; hidden when none of them matches (unless
    [force_mount]). *)
val group
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?heading:string
  -> ?value:string
  -> ?force_mount:bool
  -> part list
  -> part

(** A rule between groups, hidden while searching (unless
    [always_render]). *)
val separator : ?attrs:Attr.t list -> ?class_:string -> ?always_render:bool -> unit -> part

(** An item. [value] is what the search matches (with [keywords]) and what
    the state's [value] names; it defaults to the children's text, but only
    text nodes directly among the children count: give it when the text is
    inside an element ([<span>Calendar</span>]). *)
val item
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?value:string
  -> ?keywords:string list
  -> ?disabled:bool
  -> ?force_mount:bool
  -> ?on_select:unit Effect.t
  -> Node.t list
  -> part

(** Any other content, rendered as is. *)
val static : Node.t -> part

(** A keyboard shortcut at the end of an item, e.g. ["⌘P"]. *)
val shortcut : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** shadcn's [CommandDialog]: the menu in a {!Dialog} (titled [title], for
    screen readers), the search cleared when it closes. [class_] is the
    dialog content's. *)
val dialog
  :  ?id:string
  -> ?title:string
  -> ?description:string
  -> ?class_:string
  -> ?show_close_button:bool
  -> ?label:string
  -> ?should_filter:bool
  -> ?filter:(string -> search:string -> keywords:string list -> float)
  -> ?loop:bool
  -> open_:bool
  -> on_open_change:(bool -> unit Effect.t)
  -> state:State.t
  -> on_change:(State.t -> unit Effect.t)
  -> part list
  -> Node.t
