(** An interactive component which expands/collapses a panel. Port of
    shadcn/ui's [Collapsible] (Radix Collapsible: the trigger's
    [aria-expanded] and [aria-controls], [data-state] on every part, the
    content [hidden] while closed).

    {[
      Collapsible.component
        (return (fun c ->
          [ Collapsible.trigger c [ Node.text "Can I use this in my project?" ]
          ; Collapsible.content c [ Node.text "Yes. Free to use." ]
          ]))
        graph
    ]} *)

open! Import

(** The collapsible's state, for its parts. *)
type t

val is_open : t -> bool

(** Controlled: [open_] and [on_change] (Radix's [open] and [onOpenChange]);
    [id] (unique on the page, ["collapsible"] by default) names the
    content for the trigger's [aria-controls]. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?disabled:bool
  -> ?id:string
  -> open_:bool
  -> on_change:(bool -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** The state for parts without the root's [<div>]: shadcn's
    [<Collapsible asChild>] over another element, which takes
    {!root_attrs}. *)
val create
  :  ?disabled:bool
  -> ?id:string
  -> open_:bool
  -> on_change:(bool -> unit Effect.t)
  -> unit
  -> t

(** The root's [data-state] (and [data-disabled]), for the element in its
    place (without [data-slot]: the element keeps its own). *)
val root_attrs : t -> Attr.t list

(** Keeps whether it is open: closed at first unless [default_open]. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?disabled:bool
  -> ?default_open:bool
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** A [<button>] that opens and closes it. *)
val trigger : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t

(** Makes [node] the trigger: shadcn's [<CollapsibleTrigger asChild>], as
    in [Collapsible.trigger_child c (Button.view ...)]. Its attributes merge
    as Radix's [Slot] (see {!Slot.merge}): [data-slot="collapsible-trigger"]
    replaces the element's own. *)
val trigger_child : ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** The trigger's attributes without [data-slot], for an element that keeps
    its own: a part over [<CollapsibleTrigger>] with [asChild] (shadcn's
    [<SidebarGroupLabel asChild><CollapsibleTrigger>]). *)
val trigger_attrs : t -> Attr.t list

(** The panel: [hidden] while closed (its children kept, so their state
    survives). Its open size is measured into
    [--radix-collapsible-content-height] and [-width], for
    [animate-collapsible-down]/[-up] classes, and it stays visible until
    such a closing animation ends. *)
val content : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t

(** The measuring and the delayed hiding of {!content}, for an element whose
    [hidden] attribute follows [open_] (Accordion's content uses it, with
    [accordion]: the size goes into [--radix-accordion-content-height] and
    [-width] too). *)
val presence : ?accordion:bool -> open_:bool -> unit -> Attr.t
