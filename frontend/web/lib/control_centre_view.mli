open! Core
open! Import

(** The control centre (Ctrl+K, the sidebar's Search): one filter, the rows
    in their groups (a setting's current value and shortcut on the right),
    or a setting's values a level down under its breadcrumb; the keys it
    takes along its foot. A centred box; on a phone, the whole screen. *)
val view
  :  App.Model.t
  -> root:Control_centre.t
  -> options:Dialog.Control_options.t option
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
