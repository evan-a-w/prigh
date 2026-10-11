open! Core

module Target = struct
  type t =
    | Editor of { cursor : int }
    | Field
    | Control
    | Session_search
    | Session of string
    | Terminal
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
  ; "Alt+M", "cycle the mode: ask → auto-edit → auto → plan (/mode, /plan)"
  ; "Ctrl+O", "cycle the transcript verbosity"
  ; "Ctrl+X", "copy the last reply (when nothing is selected)"
  ; "Ctrl+↑ / Ctrl+↓", "previous / next of your messages in the transcript"
  ; "PageUp / PageDown", "scroll the transcript"
  ; "Ctrl+K", "search sessions (↓ ↑ through them, Enter opens, Delete deletes)"
  ; "Ctrl+B", "show or hide the sidebar"
  ; "Alt+1…9", "follow subagent or job N in the agents panel"
  ; "Alt+] Alt+[", "the next or previous subagent or job"
  ; "Alt+0", "close the agents panel"
  ; ( "Ctrl+`"
    , "open or close the terminal (/terminal); in it, every other key goes to \
       the shell" )
  ; "Tab (in /scoped-models)", "check or uncheck the highlighted model"
  ; ( "y / a / n / r (confirmation)"
    , "allow once, always allow (its rules, this session), deny, deny saying \
       why; Enter allows, Esc denies" )
  ; ( "↑↓ Space ←→ (question)"
    , "choose, pick, previous / next question; type for your own answer; Enter \
       accepts, Esc declines" )
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

let on_control t =
  match t.target with
  | Control -> true
  | Editor _ | Field | Session_search | Session _ | Terminal | Page -> false
;;

(* Ctrl+` by the key's position, whatever the layout types there. *)
let terminal_toggle t = ctrl_only t && String.equal t.code "Backquote"

let dialog_key (dialog : Dialog.t) t : App.Action.t option =
  match t.key, dialog with
  | "Escape", _ -> Some Close_dialog
  | "ArrowUp", (Picker _ | Login _ | Scoped_models _ | Prompt _) ->
    Some (Dialog_move (-1))
  | "ArrowDown", (Picker _ | Login _ | Scoped_models _ | Prompt _) ->
    Some (Dialog_move 1)
  | "PageUp", (Picker _ | Scoped_models _) -> Some (Dialog_move (-8))
  | "PageDown", (Picker _ | Scoped_models _) -> Some (Dialog_move 8)
  | "Tab", Scoped_models _ when not t.shift -> Some Dialog_toggle
  | "Tab", Prompt { suggestions = _ :: _; _ } when not t.shift ->
    Some Dialog_complete
  | "Enter", _ when (not t.shift) && not (on_control t) -> Some Dialog_accept
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

(* The agents panel's keys: the TUI's Alt+digit, but not its Shift+Tab (the
   browser's way to move the focus back). Only Alt alone: AltGr is Ctrl+Alt
   on Windows, and macOS's Option types characters, which are left to it. *)
let agents_key (m : App.Model.t) t : App.Action.t option =
  let alt = t.alt && not (t.ctrl || t.meta || t.shift) in
  let listed = List.length (Agents.listed m.agents) in
  match t.key with
  | "0" when alt && m.agents.open_ -> Some Toggle_subagents
  | ("1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") when alt ->
    let n = Int.of_string t.key in
    Option.some_if (n <= listed) (App.Action.Focus_agent n)
  | "]" when alt && listed > 0 -> Some (Cycle_agent 1)
  | "[" when alt && listed > 0 -> Some (Cycle_agent (-1))
  | "Escape"
    when m.agents.open_
         && (m.narrow
             ||
             match t.target with
             | Editor _ | Terminal -> false
             | Field | Control | Session_search | Session _ | Page -> true) ->
    Some Agents_back
  | _ -> None
;;

(* The sidebar is a picker: ↓ ↑ move from the search through the sessions,
   Enter opens one (from the search, the best match), Esc goes back to the
   editor. *)
let session_search_key t : App.Action.t option =
  match t.key with
  | "ArrowDown" when plain t -> Some (Session_nav { from = None; by = 1 })
  | "Enter" when plain t -> Some Open_first_session
  | "Escape" -> Some Leave_sidebar
  | _ -> None
;;

let session_key (m : App.Model.t) t id : App.Action.t option =
  match t.key with
  | "ArrowDown" when plain t -> Some (Session_nav { from = Some id; by = 1 })
  | "ArrowUp" when plain t -> Some (Session_nav { from = Some id; by = -1 })
  | ("Enter" | " ") when plain t ->
    List.find m.sessions ~f:(fun s -> String.equal s.id id)
    |> Option.map ~f:(fun (s : Prigh_protocol.Session_summary.t) ->
      App.Action.Switch_session s.path)
  | "Delete" when plain t ->
    List.find m.sessions ~f:(fun s -> String.equal s.id id)
    |> Option.map ~f:(fun (s : Prigh_protocol.Session_summary.t) ->
      App.Action.Ask_delete s.path)
  | "Escape" -> Some Leave_sidebar
  | _ -> None
;;

(* y/Enter allow, a always (when offered), n/Esc deny, r a reason; while
   typing the reason, Enter sends it and Esc goes back. *)
let confirm_key (m : App.Model.t) (c : Prigh_protocol.Pending.Confirm.t) t
  : App.Action.t option
  =
  let answer answer =
    Some
      (App.Action.Answer_confirm
         { call_id = c.call_id; agent_id = c.agent_id; answer })
  in
  let letter = not (t.ctrl || t.meta || t.alt) in
  let offered answer =
    List.exists (Prigh_ui.Confirm_prompt.answers c) ~f:(fun (a, _, _) ->
      Prigh_ui.Confirm_prompt.Answer.equal a answer)
  in
  match m.confirm_reason, t.key with
  | Some _, "Enter" when plain t && not (on_control t) ->
    Some Send_confirm_reason
  | Some _, "Escape" -> Some Cancel_confirm_reason
  | Some _, _ -> None
  | None, "Enter" when plain t && not (on_control t) -> answer Allow
  | None, "Escape" -> answer Deny
  | None, ("y" | "Y") when letter -> answer Allow
  | None, ("a" | "A") when letter && offered Always -> answer Always
  | None, ("n" | "N") when letter -> answer Deny
  | None, ("r" | "R") when letter -> answer Deny_with_reason
  | None, _ -> None
;;

(* A character, not a named key like [ArrowUp]. *)
let typed key = String.length key = 1 || Char.to_int key.[0] >= 0x80

(* ↑↓ rows, ←→ questions, Space picks, typing goes to the own answer (its
   field, once focused, types itself), Enter accepts, Esc declines. *)
let question_key t : Question_dialog.Action.t option =
  let field =
    match t.target with
    | Field -> true
    | Editor _ | Control | Session_search | Session _ | Terminal | Page -> false
  in
  match t.key with
  | "Escape" -> Some Decline
  | "Enter" when plain t && not (on_control t) -> Some Accept
  | "ArrowUp" when plain t -> Some Up
  | "ArrowDown" when plain t -> Some Down
  | "ArrowLeft" when plain t && not field -> Some Prev
  | "ArrowRight" when plain t && not field -> Some Next
  | _ when field || on_control t || t.ctrl || t.meta || t.alt -> None
  | " " -> Some Toggle
  | "Backspace" -> Some Backspace
  | key when typed key -> Some (Insert key)
  | _ -> None
;;

let handle (m : App.Model.t) t : App.Action.t option =
  match t.target, m.confirms, m.questions, m.dialog with
  (* Inside the terminal every key but Ctrl+` is the shell's, Esc and Enter
     included, even with a dialog or a confirmation showing. *)
  | Terminal, _, _, _ ->
    Option.some_if (terminal_toggle t) App.Action.Toggle_terminal
  | _, c :: _, _, _ -> confirm_key m c t
  | _, [], _ :: _, _ ->
    Option.map (question_key t) ~f:(fun a -> App.Action.Question a)
  | _, [], [], Some dialog -> dialog_key dialog t
  | _, [], [], None ->
    (match t.key with
     | _ when terminal_toggle t -> Some Toggle_terminal
     | ("l" | "L") when command t -> Some Open_model_picker
     | ("k" | "K") when command t -> Some Open_sessions
     | ("b" | "B") when command t -> Some Toggle_sidebar
     | ("o" | "O") when ctrl_only t -> Some Cycle_verbosity
     | ("p" | "P") when ctrl_only t -> Some (Cycle_model 1)
     | _ when alt_letter t "p" -> Some (Cycle_model (-1))
     | _ when alt_letter t "t" -> Some Cycle_thinking
     | _ when alt_letter t "m" -> Some Cycle_mode
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
       (match agents_key m t, t.target with
        | Some action, _ -> Some action
        | None, Editor { cursor } -> editor_key m t ~cursor
        | None, Session_search -> session_search_key t
        | None, Session id -> session_key m t id
        | None, (Field | Control | Terminal | Page) -> None))
;;
