open! Core
open! Import

(** The review's since-view controls, shared by the review page and the file
    browser's preview: the since segment ending the Changes / Whole file
    switch ("Since latest (comment · 20:59)", or the earlier action picked)
    with the menu of the file's actions under its chevron (Latest, which
    follows the newest action, then the earlier ones: {!Review_action}),
    and the note over the since-view (nothing changed, not comparable,
    still reading). *)

(** For a file with actions: the since segment and its menu's button, to
    end a [.segmented] group; [place] tells the page's (with the menu,
    [Review_pick (Since path)], open under it) from the file browser's. *)
val segment
  :  App.Model.t
  -> string
  -> place:[ `Page | `Files ]
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t option

(** Over the since-view of [path]: what it compares with and what came of
    it. *)
val note : App.Model.t -> string -> Node.t
