(** A set of checkable buttons, known as radio buttons, where no more than
    one of the buttons can be checked at a time. Port of shadcn/ui's
    [RadioGroup] (Radix Radio Group: Tab reaches the checked item; arrow
    keys, Home and End move to another and check it; disabled items are
    skipped).

    {[
      Radio_group.component ~default_value:"comfortable"
        (return (fun group ->
          [ Node.div ~attrs:[ Attr.class_ "flex items-center gap-3" ]
              [ Radio_group.item group ~value:"default" ~id:"r1"
              ; Label.view ~for_:"r1" [ Node.text "Default" ]
              ]
          ; ...
          ]))
        graph
    ]} *)

open! Import

(** The group's state, for its items. *)
type t

(** Controlled; [value] is the checked item's, [None] before one is. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?orientation:Roving.Orientation.t
  -> ?disabled:bool
  -> ?required:bool
  -> value:string option
  -> on_change:(string -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** Keeps the checked value, [default_value] at first. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?orientation:Roving.Orientation.t
  -> ?disabled:bool
  -> ?required:bool
  -> ?default_value:string
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> string option Control.t Bonsai.t

(** A radio button ([role="radio"]); give it an [id] for a {!Label}'s
    [for_]. *)
val item
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> ?disabled:bool
  -> ?invalid:bool
  -> t
  -> value:string
  -> Node.t
