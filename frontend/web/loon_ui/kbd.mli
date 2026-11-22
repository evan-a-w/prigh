(** Used to display textual user input from keyboard. Port of shadcn/ui's
    [Kbd] and [KbdGroup].

    {[
      Kbd.group [ Kbd.view [ Node.text "Ctrl" ]; Kbd.view [ Node.text "B" ] ]
    ]} *)

open! Import

(** A [<kbd>]: one key. *)
val view : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** Keys shown together (a [<kbd>] too). *)
val group : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
