open! Core
open! Import

(** Telling the user what happened while the tab was hidden: a browser
    notification (when enabled and the browser allows it) for each thing
    [Prigh_ui.Notify] finds worth it, and a count in the title and the
    favicon until the tab is seen again. *)

module Permission : sig
  type t =
    | Default (** not asked yet *)
    | Granted
    | Denied
    | Unsupported (** no Notification API, e.g. a plain http page *)
  [@@deriving sexp_of, equal]

  (** [Notification.permission]'s value; [unsupported] for anything else. *)
  val of_string : string -> t
end

type t =
  { enabled : bool (** [/notify on|off], kept in the browser *)
  ; permission : Permission.t
  ; hidden : bool
  ; unseen : int (** what happened while hidden *)
  }
[@@deriving sexp_of, equal]

val init : t

(** The tab became hidden or visible again (which clears [unseen]). *)
val set_hidden : t -> bool -> t

(** An event of the main agent: counted while hidden, with the notification
    to raise if enabled and allowed. *)
val on_event : t -> Event.t -> where:string -> t * Prigh_ui.Notify.t option

(** What [/notify] says. *)
val describe : t -> string

(** The page's title with the count, e.g. [(2) prigh]. *)
val title : t -> string
