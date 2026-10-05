open! Core
open Prigh_web

(** Transcripts built from backend events, as the page renders them. *)

(** Applies the events (JSON, as on the wire) between an agent start and,
    unless [running], its end. A tool call's events first add an assistant
    message holding the call, so its card shows without spelling it out. *)
val chat : ?running:bool -> string list -> Chat.t

val apply : Chat.t -> string -> Chat.t

(** Times are shown as on Monday 5 October 2026 at 15:00 in UTC+2. *)
val times : Message_time.t

(** The rendered transcript as indented HTML; [selector] picks part of it. *)
val show : ?selector:string -> Chat.t -> unit

(** Its visible text. *)
val text : ?selector:string -> Chat.t -> unit
