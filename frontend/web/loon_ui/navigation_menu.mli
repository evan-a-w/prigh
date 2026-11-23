(** A collection of links for navigating websites. Port of shadcn/ui's
    [NavigationMenu] (Radix NavigationMenu's behaviour: a trigger opens its
    content on hover, after a short delay unless another is open, or on
    click; Enter, Space and Arrow Down open it and Arrow Down or Tab moves
    into it; Arrow Left and Right move between the triggers and links of
    the list, arrows between the links of the content; Esc, a click outside
    or a link chosen closes it).

    With [viewport] (the default), the open content is shown in a shared
    viewport under the list that resizes to fit it and slides between
    contents; without, each content drops down under its own item.

    {[
      Navigation_menu.component
        (return (fun nav ->
          [ Navigation_menu.list nav
              [ Navigation_menu.item
                  [ Navigation_menu.trigger nav ~value:"home" [ Node.text "Home" ]
                  ; Navigation_menu.content nav ~value:"home"
                      [ Navigation_menu.link ~href:"/docs" [ Node.text "Docs" ] ]
                  ]
              ; Navigation_menu.item
                  [ Navigation_menu.link
                      ~class_:(Navigation_menu.trigger_style ())
                      ~href:"/docs"
                      [ Node.text "Docs" ]
                  ]
              ]
          ]))
        graph
    ]} *)

open! Import

(** The menu's state for its parts: which content is open, and how to
    change it. *)
type t

(** Controlled: [value] is the open item's ([None]: all closed);
    [previous] the item open before it, from which the content slides
    ([component] keeps it). [id] (unique on the page, ["navigation-menu"]
    by default) prefixes the parts' ids. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?viewport:bool
  -> ?id:string
  -> ?previous:string
  -> value:string option
  -> on_change:(string option -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** Keeps which item is open (none at first, or [default_value]). *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?viewport:bool
  -> ?default_value:string
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** The [<ul>] of items. *)
val list : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t

val item : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** shadcn's [navigationMenuTriggerStyle]: the classes of a trigger, then
    [class_], for a link in the list that should look like one. *)
val trigger_style : ?class_:string -> unit -> string

(** The button opening the content of the same [value], with a chevron. *)
val trigger
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?disabled:bool
  -> t
  -> value:string
  -> Node.t list
  -> Node.t

(** Shown while [value] is open: under its item, or in the viewport. *)
val content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> t
  -> value:string
  -> Node.t list
  -> Node.t

(** An [<a>]; choosing it closes the menu. [active]: the current page
    ([aria-current="page"]). *)
val link
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?active:bool
  -> href:string
  -> Node.t list
  -> Node.t

(** An arrow under the open trigger. Put it last in the [list]'s
    children. *)
val indicator : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t
