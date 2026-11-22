(** A composable, themeable and customizable sidebar component. Port of
    shadcn/ui's [Sidebar].

    A {!provider} holds the parts and the page beside them ({!inset}); a
    {!t} (from {!component}, or {!create} when the caller keeps the state)
    says whether the sidebar is open, and is passed to the parts that need
    it. On desktop the sidebar collapses (off the canvas, or to its icons);
    below {!Use_mobile.breakpoint} it is a sheet over the page instead.
    Ctrl+B (Cmd+B) toggles it. *)

open! Import

module State : sig
  type t =
    | Expanded
    | Collapsed
  [@@deriving enumerate, sexp_of, compare, equal]

  (** [expanded] or [collapsed], as in [data-state]. *)
  val name : t -> string
end

module Side : sig
  type t =
    | Left
    | Right
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

module Variant : sig
  type t =
    | Sidebar
    | Floating
    | Inset
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

module Collapsible : sig
  type t =
    | Offcanvas
    | Icon
    | None
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** The sidebar's state: shadcn's [useSidebar()]. *)
type t

(** Controlled: [open_] on desktop, [open_mobile] for the sheet on phones,
    [is_mobile] which of them shows; [open_tooltip] the id of the menu
    button tooltip shown (none, and never shown, unless the caller keeps it
    with [on_open_tooltip_change]). Setting [open_] also writes it to the
    cookie [sidebar_state], as shadcn does, unless [persist] is [false]. *)
val create
  :  ?persist:bool
  -> open_:bool
  -> on_open_change:(bool -> unit Effect.t)
  -> open_mobile:bool
  -> on_open_mobile_change:(bool -> unit Effect.t)
  -> is_mobile:bool
  -> ?open_tooltip:string
  -> ?on_open_tooltip_change:(string option -> unit Effect.t)
  -> unit
  -> t

(** Keeps the state: open at first as the cookie [sidebar_state] says,
    else [default_open] (default [true]); the sheet closed; [is_mobile]
    from {!Use_mobile}; the menu buttons' tooltips. With [persist] ([true]) it reads and writes the
    cookie. *)
val component
  :  ?default_open:bool
  -> ?persist:bool
  -> local_ Bonsai.graph
  -> t Bonsai.t

val state : t -> State.t
val open_ : t -> bool
val set_open : t -> bool -> unit Effect.t
val open_mobile : t -> bool
val set_open_mobile : t -> bool -> unit Effect.t
val is_mobile : t -> bool

(** Toggles the sheet on phones, the sidebar elsewhere. *)
val toggle : t -> unit Effect.t

(** [SidebarProvider]: the wrapper of the sidebar and the page, setting
    [--sidebar-width] and [--sidebar-width-icon]. [keyboard_shortcut]
    (default [true]) listens for Ctrl+B / Cmd+B on the window. *)
val provider
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?keyboard_shortcut:bool
  -> t
  -> Node.t list
  -> Node.t

(** [Sidebar]. On phones a {!Sheet} on [side] ([attrs] go on its content);
    elsewhere [class_] goes on the fixed container, as shadcn's
    [className]; [side] defaults to [Left], [variant] to
    [Sidebar], [collapsible] to [Offcanvas]. *)
val root
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?side:Side.t
  -> ?variant:Variant.t
  -> ?collapsible:Collapsible.t
  -> t
  -> Node.t list
  -> Node.t

(** [SidebarTrigger]: a ghost icon button that toggles the sidebar, after
    [on_click]. *)
val trigger
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?on_click:unit Effect.t
  -> t
  -> Node.t

(** [SidebarRail]: the strip along the sidebar's edge that toggles it. *)
val rail : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t

(** [SidebarInset]: the [<main>] beside an [Inset] sidebar. *)
val inset : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** [SidebarInput]: an {!Input} sized for the sidebar (controlled with
    [value] and [on_input], else the browser keeps what is typed). *)
val input
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?type_:Input.Type.t
  -> ?id:string
  -> ?placeholder:string
  -> ?value:string
  -> ?on_input:(string -> unit Effect.t)
  -> unit
  -> Node.t

val header : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val footer : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** [SidebarSeparator]: a {!Separator}. *)
val separator : ?attrs:Attr.t list -> ?class_:string -> unit -> Node.t

(** [SidebarContent]: the scrolling middle. *)
val content : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

val group : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** The classes of [SidebarGroupLabel], for another element in its place
    (shadcn's [asChild]). *)
val group_label_classes : ?class_:string -> unit -> string

val group_label : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val group_action_classes : ?class_:string -> unit -> string

(** A [<button>] at the top right of a group. *)
val group_action
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?on_click:unit Effect.t
  -> Node.t list
  -> Node.t

val group_content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> Node.t list
  -> Node.t

(** [SidebarMenu], a [<ul>] of {!menu_item}s. *)
val menu : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

val menu_item : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

module Menu_button_variant : sig
  type t =
    | Default
    | Outline
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

module Menu_button_size : sig
  type t =
    | Default
    | Sm
    | Lg
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** shadcn's [sidebarMenuButtonVariants]. *)
val menu_button_classes
  :  ?variant:Menu_button_variant.t
  -> ?size:Menu_button_size.t
  -> ?class_:string
  -> unit
  -> string

(** A menu button's tooltip (shadcn's [tooltip] prop): a {!Tooltip} on
    the button's right. *)
module Menu_tooltip : sig
  type t
end

(** [menu_tooltip sidebar text]: shown while the sidebar is collapsed to
    its icons (on desktop), or as [hidden] says ([~hidden:false]: always,
    shadcn's [tooltip={{ hidden: false }}]). [id] (unique on the page;
    by default from [text]) prefixes the tooltip's ids. *)
val menu_tooltip : ?id:string -> ?hidden:bool -> t -> string -> Menu_tooltip.t

(** [SidebarMenuButton]: a [<button>], or an [<a>] with [href] (shadcn's
    [asChild] over a link), with its [tooltip] (from {!menu_tooltip}). *)
val menu_button
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Menu_button_variant.t
  -> ?size:Menu_button_size.t
  -> ?is_active:bool
  -> ?tooltip:Menu_tooltip.t
  -> ?href:string
  -> ?on_click:unit Effect.t
  -> Node.t list
  -> Node.t

(** [SidebarMenuAction]: a [<button>] at the right of a menu item; with
    [show_on_hover], only while the item is hovered or focused (on
    desktop). *)
val menu_action
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?show_on_hover:bool
  -> ?on_click:unit Effect.t
  -> Node.t list
  -> Node.t

val menu_badge : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** [SidebarMenuSkeleton]: a loading menu item. Its text is [width] wide
    (a CSS length; shadcn picks between 50% and 90% at random). *)
val menu_skeleton
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?show_icon:bool
  -> ?width:string
  -> unit
  -> Node.t

val menu_sub : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

val menu_sub_item
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> Node.t list
  -> Node.t

module Menu_sub_button_size : sig
  type t =
    | Sm
    | Md
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** [SidebarMenuSubButton]: an [<a>]. *)
val menu_sub_button
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?size:Menu_sub_button_size.t
  -> ?is_active:bool
  -> ?href:string
  -> Node.t list
  -> Node.t
