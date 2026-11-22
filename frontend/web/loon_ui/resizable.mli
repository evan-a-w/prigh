(** Accessible resizable panel groups and layouts with keyboard support.
    Port of shadcn/ui's [Resizable] (react-resizable-panels' [Group],
    [Panel] and [Separator]: dragging a handle, or focusing it and using
    the arrow keys (5% a step), Home and End, resizes the panels on either
    side within their minimum and maximum sizes).

    {[
      Resizable.component
        ~orientation:Horizontal
        ~class_:"max-w-md rounded-lg border"
        (return
           [ Resizable.panel ~default_size:50. [ Node.text "One" ]
           ; Resizable.handle ()
           ; Resizable.panel ~default_size:50. [ Node.text "Two" ]
           ])
        graph
    ]} *)

open! Import

module Orientation : sig
  type t =
    | Horizontal
    | Vertical
  [@@deriving enumerate, sexp_of, compare, equal]

  (** ["horizontal"] or ["vertical"]. *)
  val name : t -> string
end

(** A panel or a handle, in the order they show. *)
type child

(** A panel ([ResizablePanel]). Sizes are percentages of the group:
    [default_size] (by default, an equal share of what the other panels
    leave), [min_size] (0) and [max_size] (100). [class_] goes on the
    panel's inner, scrolling element, as react-resizable-panels does. *)
val panel
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> ?default_size:float
  -> ?min_size:float
  -> ?max_size:float
  -> Node.t list
  -> child

(** The handle between two panels ([ResizableHandle]), a focusable
    [role="separator"]; [with_handle] shows a grip. *)
val handle
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?with_handle:bool
  -> ?disabled:bool
  -> unit
  -> child

(** The panels' sizes when nothing has moved yet: [default_size]s, the rest
    shared out, fitted within the limits. *)
val default_layout : child list -> float list

(** Controlled ([ResizablePanelGroup]): [layout] has a size for each panel
    (when its length does not match, [default_layout]); [on_layout_change]
    gets the new sizes while a handle moves. [id] (unique on the page,
    ["resizable"] by default) prefixes the panels' ids. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> orientation:Orientation.t
  -> layout:float list
  -> on_layout_change:(float list -> unit Effect.t)
  -> child list
  -> Node.t

(** Keeps the layout ([default_layout] at first). *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> orientation:Orientation.t
  -> child list Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** The layout after moving the handle between panels [pivot] and
    [pivot + 1] by [delta] percent (positive: towards the end), as
    react-resizable-panels: the panel growing stops at its maximum, those
    shrinking give way one after another down to their minimums. *)
val adjust
  :  child list
  -> layout:float list
  -> pivot:int
  -> delta:float
  -> float list
