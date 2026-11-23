(** An indicator that can be used to show a loading state. Port of
    shadcn/ui's [Spinner]: Lucide's [loader-circle], spinning; size and
    colour it with [class_] ([size-6 text-blue-500]). *)

open! Import

(** [role="status"], [aria-label="Loading"]. *)
val view : ?attrs:Attr.t list -> ?class_:string -> unit -> Node.t
