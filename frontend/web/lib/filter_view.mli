open! Core
open! Import

(** The transcript filter ([/filter], {!Filter_menu}) on the page. *)

(** The kinds the filter hides, lowercase and comma separated. *)
val hidden_names : Transcript_filter.t -> string

(** The menu, under the top bar's toggles ([/filter], Ctrl+K, the phone's
    sheet). *)
val menu
  :  App.Model.t
  -> Filter_menu.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** At the top of the transcript while a filter is on: what it hides, how
    much of what is loaded, and "Show all". *)
val note : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t list
