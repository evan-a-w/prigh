open! Core

type t =
  { key : string
  ; shift : bool
  ; alt : bool
  ; ctrl : bool
  ; meta : bool
  }

let editor_action t ~running : App.Action.t option =
  match t.key with
  | "Enter" when t.shift || t.ctrl || t.meta -> None
  | "Enter" when t.alt -> Some Send_follow_up
  | "Enter" -> Some Send
  | "Escape" when running -> Some Abort
  | _ -> None
;;
