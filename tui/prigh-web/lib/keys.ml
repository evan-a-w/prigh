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
  ; code : string
  ; shift : bool
  ; alt : bool
  ; ctrl : bool
  ; meta : bool
  ; selection : bool
  ; target : Target.t
  }
[@@deriving sexp_of]

let help =
  [ "Enter", "send; while running, steer the agent"
  ; "Alt+Enter", "queue a follow-up for after the run"
  ; "Shift+Enter", "new line"
  ; "Esc", "stop the run; close a dialog, popup or side answer"
  ; "↑ ↓", "earlier prompts (in an empty editor or its first line)"
  ; "Alt+↑", "take the last queued message back into the editor"
  ; "Tab", "complete a /command, its argument or an @path"
  ; ( "!cmd"
    , "run a shell command (!!cmd: not added to the context; !&cmd: as a \
       background job)" )
  ; "Ctrl+L", "switch model"
  ; "Ctrl+P / Alt+P", "next / previous scoped model (/scoped-models)"
  ; "Alt+T", "cycle the thinking level"
  ; "Ctrl+O", "cycle the transcript verbosity"
  ; "Ctrl+X", "copy the last reply (when nothing is selected)"
  ; "Ctrl+↑ / Ctrl+↓", "previous / next of your messages in the transcript"
  ; "PageUp / PageDown", "scroll the transcript"
  ; "Ctrl+K", "search sessions"
  ; "Ctrl+B", "show or hide the sidebar"
  ; "Tab (in /scoped-models)", "check or uncheck the highlighted model"
  ; "Delete (in /jobs)", "kill the highlighted job"
  ]
;;

let browser =
  [ ( "Ctrl+C / Ctrl+V / Ctrl+Z"
    , "copy, paste, undo: the browser's (Esc stops a run)" )
  ; "Ctrl+F", "find in the page, which has the whole transcript"
  ; ( "Ctrl+T / Ctrl+N / Ctrl+W"
    , "the browser's tabs and windows: Alt+T cycles thinking" )
  ; "Ctrl+R", "reload: the session comes back (?session=); Tab completes @paths"
  ; "Ctrl+G", "no $EDITOR in a browser: edit here (Shift+Enter for new lines)"
  ]
;;

let plain t = not (t.shift || t.alt || t.ctrl || t.meta)
let command t = (t.ctrl || t.meta) && not (t.alt || t.shift)
let ctrl_only t = t.ctrl && not (t.alt || t.shift || t.meta)
let alt_only t = t.alt && not (t.ctrl || t.shift || t.meta)

(* Option+letter types a symbol on a Mac: match the physical key. *)
let alt_letter t letter =
  alt_only t && String.equal t.code ("Key" ^ String.uppercase letter)
;;

let dialog_key (dialog : Dialog.t) t : App.Action.t option =
  match t.key, dialog with
  | "Escape", _ -> Some Close_dialog
  | "ArrowUp", (Picker _ | Login _ | Scoped_models _ | Prompt _ | Jobs _) ->
    Some (Dialog_move (-1))
  | "ArrowDown", (Picker _ | Login _ | Scoped_models _ | Prompt _ | Jobs _) ->
    Some (Dialog_move 1)
  | "PageUp", (Picker _ | Scoped_models _) -> Some (Dialog_move (-8))
  | "PageDown", (Picker _ | Scoped_models _) -> Some (Dialog_move 8)
  | "Tab", Scoped_models _ when not t.shift -> Some Dialog_toggle
  | "Tab", Prompt { suggestions = _ :: _; _ } when not t.shift ->
    Some Dialog_complete
  | "Delete", Jobs _ -> Some Dialog_delete
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
  | "Escape", _ when Option.is_some m.btw -> Some Close_btw
  | "Escape", _ when App.Model.running m -> Some Abort
  | "ArrowUp", None
    when plain t
         && first_line m.draft ~cursor
         && Option.is_some (History.older m.history ~draft:m.draft) ->
    Some History_older
  | "ArrowDown", None
    when plain t && History.browsing m.history && last_line m.draft ~cursor ->
    Some History_newer
  | "PageUp", None when plain t -> Some (Scroll_chat (-1))
  | "PageDown", None when plain t -> Some (Scroll_chat 1)
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
     | ("o" | "O") when ctrl_only t -> Some Cycle_verbosity
     | ("p" | "P") when ctrl_only t -> Some (Cycle_model 1)
     | _ when alt_letter t "p" -> Some (Cycle_model (-1))
     | _ when alt_letter t "t" -> Some Cycle_thinking
     | ("x" | "X") when ctrl_only t && not t.selection -> Some Copy_last
     | ("g" | "G") when ctrl_only t ->
       Some
         (Show_toast
            { text =
                "There is no $EDITOR in a browser: edit here (Shift+Enter for \
                 new lines)."
            ; error = false
            })
     | "ArrowUp" when alt_only t -> Some Dequeue
     | "ArrowUp" when ctrl_only t -> Some (Jump_to_user_message (-1))
     | "ArrowDown" when ctrl_only t -> Some (Jump_to_user_message 1)
     | "Escape" when m.narrow && m.sidebar_open -> Some Toggle_sidebar
     | _ ->
       (match t.target with
        | Editor { cursor } -> editor_key m t ~cursor
        | Field | Page -> None))
;;
