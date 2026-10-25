open! Core
open! Import

(** [reply_to_comment {id, text, edits?, resolve?}]: the agent's reply to one
    of the user's review comments ({!Review_state}), threaded under it;
    [edits] ([{old_text, new_text}], like [edit]'s, on the comment's file)
    propose a change the user accepts or rejects. [reply] records it. Only
    the main agent has it, and only while the session has comments. *)
val create
  :  reply:
       (id:string
        -> text:string
        -> edits:Review_comment.Edit.t list
        -> resolve:bool
        -> Review_comment.t Or_error.t)
  -> Tool.t

val name : string
