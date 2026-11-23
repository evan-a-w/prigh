(** Pagination with page navigation, next and previous links. Port of
    shadcn/ui's [Pagination]: [root] holds [content], a row of [item]s each
    holding a [link] (a page), [previous], [next] or [ellipsis].

    {[
      Pagination.root
        [ Pagination.content
            [ Pagination.item [ Pagination.previous ~href:"#" () ]
            ; Pagination.item [ Pagination.link ~href:"#" [ Node.text "1" ] ]
            ; Pagination.item [ Pagination.ellipsis () ]
            ; Pagination.item [ Pagination.next ~href:"#" () ]
            ]
        ]
    ]}

    [entries] works out which page numbers to show. *)

open! Import

(** A [<nav role="navigation" aria-label="pagination">]. *)
val root : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** The [<ul>] of items. *)
val content : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

val item : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** An [<a>] that looks like a button: outlined when [is_active] (the
    current page, [aria-current="page"]), ghost otherwise; [size] defaults
    to [Icon]. [on_click] runs instead of following [href]. *)
val link
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?is_active:bool
  -> ?size:Button.Size.t
  -> ?href:string
  -> ?on_click:unit Effect.t
  -> Node.t list
  -> Node.t

(** "Previous" with a chevron (the text hidden on phones). *)
val previous
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?text:string
  -> ?href:string
  -> ?on_click:unit Effect.t
  -> unit
  -> Node.t

(** "Next" with a chevron (the text hidden on phones). *)
val next
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?text:string
  -> ?href:string
  -> ?on_click:unit Effect.t
  -> unit
  -> Node.t

(** "…" standing for pages not shown. *)
val ellipsis : ?attrs:Attr.t list -> ?class_:string -> unit -> Node.t

module Entry : sig
  type t =
    | Page of int
    | Ellipsis
  [@@deriving sexp_of, compare, equal]
end

(** The entries to show for [page] of [pages] (counting from 1): always the
    first and last, [siblings] (default 1) either side of [page], and
    ellipses for gaps of more than one page; the count stays the same as
    [page] moves. *)
val entries : ?siblings:int -> page:int -> pages:int -> unit -> Entry.t list
