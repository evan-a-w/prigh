(** Lucide icons that loon_ui's [Icon] lacks. *)
open! Core

type t =
  | Paperclip
  | Mic
  | Square_pen
  | Folder_plus
  | Panel_right
  | Panel_bottom
  | Git_pull_request
  | Wrench
  | File_diff
  | History
  | Cpu
  | At_sign
  | Hammer
  | File_search
  | Eye
  | Git_compare
  | Arrow_up_right
  | Circle_dot
  | Pencil_line
  | List_todo
  | Square_terminal
  | Bot_message_square
  | Chevrons_down_up

val view
  :  ?attrs:Virtual_dom.Vdom.Attr.t list
  -> ?class_:string
  -> t
  -> Virtual_dom.Vdom.Node.t
