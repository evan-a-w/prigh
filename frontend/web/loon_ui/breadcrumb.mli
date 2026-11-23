(** Displays the path to the current resource using a hierarchy of links.
    Port of shadcn/ui's [Breadcrumb]: [root] holds a [list] of [item]s
    (each a [link] or the current [page]) with [separator]s between, and
    [ellipsis] for collapsed parts.

    {[
      Breadcrumb.root
        [ Breadcrumb.list
            [ Breadcrumb.item [ Breadcrumb.link ~href:"/" [ Node.text "Home" ] ]
            ; Breadcrumb.separator ()
            ; Breadcrumb.item [ Breadcrumb.page [ Node.text "Breadcrumb" ] ]
            ]
        ]
    ]} *)

open! Import

(** A [<nav aria-label="breadcrumb">]. *)
val root : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** The [<ol>] of items and separators. *)
val list : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

val item : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** An [<a>] to an ancestor page. *)
val link
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> href:string
  -> Node.t list
  -> Node.t

(** shadcn's [BreadcrumbLink]'s classes, for another element acting as a
    link ([<BreadcrumbLink asChild>]). *)
val link_classes : ?class_:string -> unit -> string

(** The current page: not a link ([aria-current="page"]). *)
val page : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** Between items: a chevron, or [children]. *)
val separator
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?children:Node.t list
  -> unit
  -> Node.t

(** "…" standing for collapsed items. *)
val ellipsis : ?attrs:Attr.t list -> ?class_:string -> unit -> Node.t
