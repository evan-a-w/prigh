open! Core
open! Async_kernel

(** The browser side of [Page_notify]: the Notification API, the page's
    visibility, the title and the favicon. *)

(** [Notification.permission], or [unsupported]. *)
val permission : unit -> string

(** Asks the browser (it must follow a key press or a click); the answer is
    [Notification.permission]'s value. *)
val request_permission : unit -> string Deferred.t

(** [/notify on|off], kept in localStorage ([prigh-web.notify]); on unless
    turned off. *)
val saved : unit -> bool

val save : bool -> unit
val show : Prigh_ui.Notify.t -> unit

(** Called with whether the page is hidden, now and on every change. *)
val on_visibility_change : (bool -> unit) -> unit

(** Sets the title, and the favicon's dot when it counts something. *)
val set_title : string -> unit
