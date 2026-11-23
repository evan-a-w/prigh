(** Use to show a placeholder while content is loading. Port of shadcn/ui's
    [Skeleton]: size and shape it with [class_] ([h-4 w-[250px]],
    [rounded-full]). *)

open! Import

val view : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
