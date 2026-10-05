open! Core

module Target = struct
  type t =
    | Editor of { cursor : int }
    | Field
    | Page
  [@@deriving sexp_of]
end

type t =
  { key : string
  ; shift : bool
  ; alt : bool
  ; ctrl : bool
  ; meta : bool
  ; target : Target.t
  }
[@@deriving sexp_of]

let help =
  [ "Enter", "send; while running, steer the agent"
  ; "Alt+Enter", "queue a follow-up for after the run"
  ; "Shift+Enter", "new line"
  ; "Esc", "stop the run; close a dialog or popup"
  ; "↑ ↓", "earlier prompts (in an empty editor or its first line)"
  ; "Tab", "complete a /command or @path"
  ; ( "!cmd"
    , "run a shell command (!!cmd: not added to the context; !&cmd: as a \
       background job)" )
  ; "Ctrl+L", "switch model"
  ; "Ctrl+K", "search sessions"
  ; "Ctrl+B", "show or hide the sidebar"
  ]
;;

let plain t = not (t.shift || t.alt || t.ctrl || t.meta)
let command t = (t.ctrl || t.meta) && not (t.alt || t.shift)

let dialog_key (dialog : Dialog.t) t : App.Action.t option =
  match t.key, dialog with
  | "Escape", _ -> Some Close_dialog
  | "ArrowUp", (Picker _ | Login _) -> Some (Dialog_move (-1))
  | "ArrowDown", (Picker _ | Login _) -> Some (Dialog_move 1)
  | "PageUp", Picker _ -> Some (Dialog_move (-8))
  | "PageDown", Picker _ -> Some (Dialog_move 8)
  | "Enter", _ when not t.shift -> Some Dialog_accept
  | _ -> None
;;

let first_line text ~cursor = not (String.mem (String.prefix text cursor) '\n')

let last_line text ~cursor =
  not (String.mem (String.drop_prefix text cursor) '\n')
;;

let editor_key (m : App.Model.t) t ~cursor : App.Action.t option =
  match t.key, App.Model.popup m with
  | "ArrowUp", Some _ when plain t -> Some (Complete_move (-1))
  | "ArrowDown", Some _ when plain t -> Some (Complete_move 1)
  | "Tab", Some _ when plain t -> Some (Complete_accept { run = false })
  | "Enter", Some _ when plain t -> Some (Complete_accept { run = true })
  | "Escape", Some _ -> Some Complete_close
  | "Enter", _ when t.shift || t.ctrl || t.meta -> None
  | "Enter", _ when t.alt -> Some Send_follow_up
  | "Enter", _ -> Some Send
  | "Escape", _ when App.Model.running m -> Some Abort
  | "ArrowUp", None
    when plain t
         && first_line m.draft ~cursor
         && Option.is_some (History.older m.history ~draft:m.draft) ->
    Some History_older
  | "ArrowDown", None
    when plain t && History.browsing m.history && last_line m.draft ~cursor ->
    Some History_newer
  | _ -> None
;;

let handle (m : App.Model.t) t : App.Action.t option =
  match m.confirms, m.dialog with
  | c :: _, _ ->
    (match t.key with
     | "Enter" -> Some (Respond_confirm { call_id = c.call_id; allow = true })
     | "Escape" -> Some (Respond_confirm { call_id = c.call_id; allow = false })
     | _ -> None)
  | [], Some dialog -> dialog_key dialog t
  | [], None ->
    (match t.key with
     | ("l" | "L") when command t -> Some Open_model_picker
     | ("k" | "K") when command t -> Some Open_sessions
     | ("b" | "B") when command t -> Some Toggle_sidebar
     | "Escape" when m.narrow && m.sidebar_open -> Some Toggle_sidebar
     | _ ->
       (match t.target with
        | Editor { cursor } -> editor_key m t ~cursor
        | Field | Page -> None))
;;
