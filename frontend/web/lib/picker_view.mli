open! Core
open! Import

(** A picker ([Picker.t]) drawn as loon's Command (its [data-slot]s;
    style.css draws the hook classes [search], [picker-items],
    [picker-item], [picker-label], [picker-detail], [picker-empty]): the
    filter, then the items as a listbox, or what to say when none match.
    Pure; the query and the item under the keys are App's. Ids:
    [picker-input], [picker-list], [picker-option-N]. *)
val view
  :  ?noun:string (** "models": the filter's label and placeholder *)
  -> ?empty:Node.t (** instead of "Nothing matches …" *)
  -> ?leading:(Picker.Item.t -> Node.t list) (** before its label *)
  -> ?trailing:(Picker.Item.t -> Node.t list) (** after its tick *)
  -> Picker.t
  -> on_query:(string -> unit Effect.t)
  -> on_choose:(string -> unit Effect.t)
  -> Node.t list
