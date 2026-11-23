(** Class names, joined as shadcn/ui's [cn] joins them (clsx, then
    tailwind-merge): a later Tailwind class overrides an earlier one it
    conflicts with ([px-2 ... p-4] keeps [p-4]; [h-9 ... h-auto] keeps
    [h-auto]), under the same variants ([hover:], [dark:], [data-...:]).

    The merge knows the utilities loon's components use and the common ones
    a caller passes as [class_]: spacing, sizes, layout, flex and grid,
    typography, colours, borders, radii, rings, outlines, shadows, opacity,
    transitions. Classes it does not know are kept as they are (duplicates
    dropped). *)

open! Import

(** [cn [ base; variant; class_ ]]: the classes of every string, merged. *)
val cn : string list -> string

(** [Attr.classes] of {!cn}. *)
val attr : string list -> Attr.t

(** A component's [class_] argument, last: [attr ([ ours ] @ class_ opt)]. *)
val with_class : ?class_:string -> string list -> Attr.t
