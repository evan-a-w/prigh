open! Core
open! Import

(** A phone's sheets (what opens from a button comes up from the bottom),
    drawn as loon's Drawer: phone.css makes [chrome-menu], [chip-popover],
    [context-menu], [account-menu], [filter-menu] and [review-popover]
    sheets on a phone; these are their parts. *)

(** The handle at a sheet's top ([data-slot=drawer-handle]). *)
val grip : unit -> Node.t

(** A labelled row of a sheet: [glyph], [name] with [detail] under it,
    [badge] at the right; [on] pressed (a panel that is open). *)
val row
  :  ?on:bool
  -> ?id:string
  -> ?detail:string
  -> ?badge:string
  -> cls:string
  -> name:string
  -> glyph:Html.Icon.t
  -> on_click:unit Effect.t
  -> unit
  -> Node.t
