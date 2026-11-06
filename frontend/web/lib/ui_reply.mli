open! Core
open! Import

(** The components that answer the model: a [form], [choices] and a
    [checklist] with a submit button. Sending tells the page
    ([Ui_event.Send]), which sends a [prompt] (or [follow_up] while the
    agent works) with [ui_action]; once sent (on its way, or the reply found
    in the transcript, so after a reload too) the component shows what was
    sent and takes no more input. A checklist without [submit] is the
    user's own list, ticked in the page. *)

val form
  :  Ui_context.t
  -> path:string
  -> component:string
  -> title:string
  -> description:string option
  -> Ui_spec.Field.t list
  -> submit:string
  -> heading:string option
  -> Node.t

val choices
  :  Ui_context.t
  -> path:string
  -> component:string
  -> title:string
  -> prompt:string option
  -> Ui_spec.Choice.t list
  -> multiple:bool
  -> Node.t

val checklist
  :  Ui_context.t
  -> path:string
  -> component:string option
  -> title:string
  -> heading:string option
  -> (string * bool) list
  -> submit:string option
  -> Node.t

(** A form's values as it sends them, from the fields' state; or the first
    required field left empty. *)
val form_values
  :  Ui_spec.Field.t list
  -> value:(string -> string option)
  -> (Json.t, string) Result.t
