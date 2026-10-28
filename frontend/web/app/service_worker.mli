open! Core

(** The page's side of [bin/sw.template.js], the service worker that keeps the
    app shell for an installed app's offline launches. Browsers only allow one
    in a secure context (https, or localhost): elsewhere these do nothing. *)

(** Registers [sw.js] (or picks up the registration there is), and checks for
    a new build hourly and when the page is shown again. *)
val register : unit -> unit

(** Calls [f] once a new build's worker is installed and waiting for the page
    to reload into it (now, if one is). *)
val on_update_ready : (unit -> unit) -> unit

(** Lets the waiting worker take over and reloads the page once it has (at
    once when none is waiting). *)
val apply_update : unit -> unit

(** Shows a notification through the worker, as Android's Chrome and iOS's
    installed apps require; [false] when the page has none, for the page to
    make its own. *)
val show_notification : title:string -> Js_of_ocaml.Js.Unsafe.any -> bool
