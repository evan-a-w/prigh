(** Displays a form textarea or a component that looks like a textarea. Port
    of shadcn/ui's [Textarea]: a [<textarea>] that grows with its content
    ([field-sizing: content]).

    {[
      Textarea.view ~placeholder:"Type your message here." ()
    ]} *)

open! Import

(** shadcn's classes for a textarea, then [class_]. *)
val classes : ?class_:string -> unit -> string

(** With [value] it is controlled (give [on_input] too); without, the
    browser keeps what is typed. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> ?name:string
  -> ?placeholder:string
  -> ?rows:int
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> ?value:string
  -> ?on_input:(string -> unit Effect.t)
  -> unit
  -> Node.t

(** Keeps the text, [default_value] (or empty) at first. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> ?name:string
  -> ?placeholder:string
  -> ?rows:int
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> ?default_value:string
  -> local_ Bonsai.graph
  -> string Control.t Bonsai.t
