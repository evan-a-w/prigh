open! Core
open Js_of_ocaml

(** The file browser's page side: the browser's [File]s by the ids
    [Prigh_web.Files] knows them by, their uploads ([PUT] with progress),
    downloads (a link the browser saves), the file pickers and where dropped
    files go. *)

(** Remembers [schedule], tells the app the backend's web address (from the
    WebSocket's, [backend]) and listens for the pickers' choices and for
    files dragged over the page. *)
val install : schedule:(Prigh_web.App.Action.t -> unit) -> backend:string -> unit

val perform : Prigh_web.Files.Page.t -> unit

(** Files to [dest], kept until uploaded or forgotten. *)
val choose : Prigh_web.Files.Dest.t -> File.file Js.t list -> unit

val files_of : File.fileList Js.t -> File.file Js.t list

(** Whether an event happened in the file browser's panel. *)
val in_panel : Dom_html.element Js.t -> bool

(** A drag ended (dropped, or left the window). *)
val drag_end : unit -> unit
