open! Core
open! Import

(** The review's since-view controls, shared by the review page and the file
    browser's preview: the "Since you approved · 14:32" segment next to
    Changes / Whole file, the menu of a file's older actions, and the note
    over the since-view (nothing changed, not comparable, still reading). *)

(** ["you approved · 14:32"]: who acted (you, or another user), the action,
    when (in the browser's time zone). *)
val action_label : App.Model.t -> Review.Snapshot.t -> string

(** The segment showing [path] as what changed since its last action (or
    the one picked): [(text, title, on, action)] as the segmented controls
    take them; [None] for a file with no actions. *)
val segment
  :  App.Model.t
  -> string
  -> inject:(App.Action.t -> unit Effect.t)
  -> (string * string * bool * unit Effect.t) option

(** While [path] shows its since-view and has older actions: a menu to
    compare with one of them. *)
val menu : App.Model.t -> string -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** Over the since-view of [path]: what it compares with and what came of
    it. *)
val note : App.Model.t -> string -> Node.t
