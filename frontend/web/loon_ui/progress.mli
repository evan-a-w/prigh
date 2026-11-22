(** Displays an indicator showing the completion progress of a task,
    typically displayed as a progress bar. Port of shadcn/ui's [Progress]
    (Radix Progress's [role="progressbar"], [aria-value*] and
    [data-state="loading|complete|indeterminate"]). *)

open! Import

(** [value] out of [max] (default 100); without [value], indeterminate. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?max:float
  -> ?value:float
  -> unit
  -> Node.t
