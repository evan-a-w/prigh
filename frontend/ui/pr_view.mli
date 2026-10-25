open! Core
open! Import

(** How pull requests look: [/pr]'s picker rows and [/pr view]'s page. *)

(** [#24 Git hosting], with [lloyd · feat/x → main · draft] as the detail;
    the id is the number. *)
val item : P.Pull_request.t -> Picker.Item.t

(** [Pull requests in owner/name (open)]. *)
val picker_title : P.Pr_list.t -> state:string option -> string

(** Title, state, author, branches and URL; the body as markdown; each
    check with its state; then the diff, as [/diff] shows one. *)
val render : P.Pr_detail.t -> Content.t
