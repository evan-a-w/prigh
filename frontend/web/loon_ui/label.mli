(** Renders an accessible label associated with controls. Port of shadcn/ui's
    [Label] (Radix Label: a [<label>]; a double click on it does not select
    its text).

    {[
      Label.view ~for_:"terms" [ Node.text "Accept terms and conditions" ]
    ]} *)

open! Import

(** A [<label>]; [for_] is the id of the control it labels. *)
val view : ?attrs:Attr.t list -> ?class_:string -> ?for_:string -> Node.t list -> Node.t
