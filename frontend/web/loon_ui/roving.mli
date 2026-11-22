(** Roving focus in a group of items (radios, toggle groups): which item an
    arrow key, Home or End moves to, skipping disabled ones. *)

open! Import
open Js_of_ocaml

module Orientation : sig
  type t =
    | Horizontal
    | Vertical
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** [move ~enabled ~current key]: the index (into the list of
    items, whose [enabled] flags are given) that [key] moves to from
    [current]; arrows wrap. Both arrow axes move, as in a Radix group
    without an orientation. *)
val move
  :  enabled:bool list
  -> current:int
  -> Dom_html.Keyboard_code.t
  -> int option

(** For a keydown on an item of a group: the [value] of the item [key]
    moves to, and an effect focusing it. In a browser the items are the
    elements matching [item] inside the event's item's closest [group], in
    document order (their [value] attribute; disabled when they have
    [disabled]); elsewhere (tests), [fallback]: values and whether
    enabled, with [current] the item's value. *)
val next
  :  Dom_html.keyboardEvent Js.t
  -> group:string
  -> item:string
  -> fallback:(string * bool) list
  -> current:string
  -> (string * unit Effect.t) option

(** Radix's RovingFocusGroup tab stop: the group is focusable (Tab into it
    focuses its [active] item, else the last focused, else the first) and
    the items have [tabindex="-1"] but the last focused (set in the DOM).
    Shift+Tab from an item leaves the group. [group] and [item] are
    selectors; [focusable]: whether any item is enabled. *)
module Tab_stop : sig
  val group_attrs : item:string -> active:string -> focusable:bool -> Attr.t list
  val item_attrs : group:string -> item:string -> Attr.t list
end
