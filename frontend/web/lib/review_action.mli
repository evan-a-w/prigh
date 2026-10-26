open! Core
open! Import

(** The user's review actions on a file ({!Review.Snapshot}), as the
    since-view names them: what was done and where ("Comment on line 6",
    "Reply on lines 3–5", "Approved", "Edited"), the comment found by the
    time of its post. *)

(** ["Comment on line 6"], ["Accepted change on line 6"], ["Approved"]. *)
val describe : Review_comment.t list -> Review.Snapshot.t -> string

(** For "since …": ["your comment on line 6 · 20:59"], ["you accepted the
    change on line 6 · 21:05"], ["ana's approval · 20:41"]. *)
val phrase
  :  Review_comment.t list
  -> me:string option
  -> time:Message_time.t
  -> Review.Snapshot.t
  -> string

(** One word or two, for the since switch: ["comment"], ["approval"]. *)
val short : Review.Snapshot.t -> string

(** [describe]'s, then when (in the reader's time zone) and who, unless
    [me]: ["Comment on line 6 · 20:59"], ["Approved · 20:41 · by ana"]. *)
val label
  :  Review_comment.t list
  -> me:string option
  -> time:Message_time.t
  -> Review.Snapshot.t
  -> string

(** ["comment · 20:59"]: [short]'s and when. *)
val brief : time:Message_time.t -> Review.Snapshot.t -> string

(** The menu of the since-view of [path] (in [t]'s review): Latest, which
    follows the newest action as there are more, then the earlier ones,
    newest first; the one compared with marked. *)
val picker
  :  Code_review.t
  -> path:string
  -> me:string option
  -> time:Message_time.t
  -> Review_picker.t
