open! Core
open! Import

(** Session tabs ([/tabs on], off by default): a strip of open sessions over
    the chat, the current one active. Opening a session opens or focuses its
    tab; deleting or archiving one closes it. Whether tabs are on, and which
    are open in order, are the user's preferences (on the backend). *)

(** How a tab's session stands, at a glance. *)
module Status : sig
  type t =
    | Idle
    | Running (** its turn, or background work *)
    | Waiting (** a confirmation or question waits for the user *)
    | Failed (** its last reply ended in an error *)
    | Finished (** its work ended since the user last looked *)
  [@@deriving sexp_of, equal]

  (** In words, for its tooltip and screen readers. *)
  val label : t -> string
end

(** A current session kept out of the strip. *)
module Out : sig
  type t =
    | Nothing
    | Leaving of string (** its tab was closed: until the switch away lands *)
    | Last of string
    (** the last tab was closed: this one until the switch lands, then the
        new session opened instead (the empty state) *)
    | Blank of string (** that new session, until it is listed *)
  [@@deriving sexp_of, equal]
end

type t =
  { enabled : bool
  ; ids : string list (** the open tabs' sessions, in order *)
  ; loaded : bool
    (** the backend's preferences arrived, so [ids] are saved when they
        change *)
  ; saved : string list (** [ids] as the preferences have them *)
  ; known : String.Set.t option
    (** the listed sessions, not archived; [None] before the first list *)
  ; out : Out.t
  ; running : String.Set.t (** listed running *)
  ; finished : String.Set.t (** tabs whose work ended out of sight *)
  ; dragging : string option
  }
[@@deriving sexp_of, equal]

val init : t

(** Another user's sessions: the tabs go, whether they are on stays. *)
val reset : t -> t

val set_enabled : t -> bool -> t

(** The open tabs from the preferences (the browser's cache, then the
    backend's), but those of sessions known to be gone or archived. *)
val restore : t -> string list -> t

(** The backend's preferences arrived (or it keeps none): from now on the
    tabs are saved. *)
val load : t -> t

(** [ids], when they differ from what was [saved]: to save. *)
val to_save : t -> string list option

val saved : t -> t

(** The current session (which may have just changed from [previous]) is
    seen, and has a tab once listed, unless it is [out]. *)
val follow : t -> previous:string option -> current:string option -> t

(** The session list arrived: tabs of sessions gone or archived close
    (unless current), those whose work ended out of sight are [finished],
    and the current session, listed, gets its tab. *)
val listed : t -> Session_summary.t list -> current:string option -> t

(** The tabs shown, in order: those of listed sessions, not archived, with
    their summaries, and the current one (at the end until it is listed,
    unless it is [out]). *)
val shown
  :  t
  -> Session_summary.t list
  -> current:string option
  -> (string * Session_summary.t option) list

(** Closes [id]'s tab; closing the [current] one keeps it out until the
    switch away lands ([Leaving], or [Last] when there is no other tab). *)
val close : t -> string -> current:string option -> t

(** Moves [id]'s tab before [before]'s, or to the end. *)
val move : t -> string -> before:string option -> t

(** The tab [by] tabs from [from] (wrapping). *)
val step : string list -> from:string option -> by:int -> string option

(** The [n]th tab (1-based). *)
val nth : string list -> int -> string option

(** Where closing [id] moves to: the tab after it, else the one before. *)
val neighbour : string list -> string -> string option

val status : t -> Session_summary.t option -> current:bool -> Status.t

(** [ids] for the preferences, and back. *)
val to_json : string list -> Json.t

val of_json : Json.t option -> string list
