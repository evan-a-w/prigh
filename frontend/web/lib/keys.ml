open! Core

module Target = struct
  type t =
    | Editor of { cursor : int }
    | Field
    | Multiline
    | Control
    | Session_search
    | Session of string
    | Worktree of
        { session : string
        ; path : string
        }
    | Terminal
    | Files
    | Review
    | Resize of Resize.Handle.t
    | Message of int
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
  ; ( "Ctrl+Enter / ⌘+Enter"
    , "send exactly what is typed, ignoring the completion popup" )
  ; "Alt+Enter", "queue a follow-up for after the run"
  ; "Shift+Enter", "new line"
  ; "Esc", "stop the run; close a dialog, popup or side answer"
  ; "↑ ↓", "earlier prompts (in an empty editor or its first line)"
  ; "Alt+↑", "take the last queued message back into the editor"
  ; "Tab", "complete a /command, its argument, an @path or a #snippet"
  ; ( "!cmd"
    , "run a shell command (!!cmd: not added to the context; !&cmd: as a \
       background job)" )
  ; "Ctrl+L", "switch model: the model chip's menu"
  ; ( "↑ ↓ / Enter / Esc (a chip's menu)"
    , "move / choose / close it; type to filter the models" )
  ; "← → / Tab (a chip's menu)", "the next chip's menu (model, thinking, mode)"
  ; "Ctrl+P / Alt+P", "next / previous scoped model (/scoped-models)"
  ; "Alt+T", "cycle the thinking level"
  ; "Alt+M", "cycle the mode: ask → auto-edit → auto → plan (/mode, /plan)"
  ; "Alt+N", "insert a snippet at the caret: a list to filter (/snippet)"
  ; "Ctrl+O", "cycle the transcript verbosity"
  ; ( "↑ ↓ / Space / Enter / Esc (transcript filter)"
    , "the filter's menu (/filter, its button left of the files'): choose a \
       kind, show or hide it, keep the filter, or put back the one it opened \
       with" )
  ; "Ctrl+X", "copy the last reply (when nothing is selected)"
  ; ( "Ctrl+↑ / Ctrl+↓"
    , "previous / next of your messages in the transcript (loading earlier \
       ones)" )
  ; ( "Ctrl+J"
    , "jump to one of your messages: a list to filter (or the marks at the \
       transcript's right edge)" )
  ; ( "Delete (in the jump list)"
    , "trim the model's context before the highlighted message (confirmed \
       first; /trim undo brings it back)" )
  ; "PageUp / PageDown", "scroll the transcript"
  ; "Ctrl+K", "search sessions (↓ ↑ through them, Enter opens, Delete deletes)"
  ; ( "Alt+Enter / Alt+R / Delete (/worktree)"
    , "start a new session in the highlighted worktree / review its changes / \
       remove it" )
  ; ( "A (on a session)"
    , "archive or unarchive it (archived sessions are listed last, muted)" )
  ; ( "Shift+F10 / ContextMenu (on a session or worktree)"
    , "its menu: open, rename, fork, new worktree, review, files, terminal, \
       pull request, copy, delete or remove (also right-click, a long press or \
       its ⋯)" )
  ; ( "Shift+F10 / ContextMenu (on a message)"
    , "its menu: copy, trim everything before or after it, delete it (also \
       right-click, a long press or its ⋯; with text selected, right-click is \
       the browser's, to copy it)" )
  ; ( "Space (selecting)"
    , "mark a session (Shift: the range from the last one); Delete deletes the \
       marked ones, Esc stops selecting" )
  ; "Ctrl+B", "show or hide the sidebar"
  ; ( "Ctrl+,"
    , "settings: notifications, transcript detail, your account (/settings)" )
  ; ( "← → / ↑ ↓ (on a panel's edge)"
    , "resize the panel (Shift: faster; Home / End: smallest / largest; Enter: \
       the default size); or drag the edge, double-click to reset" )
  ; "Alt+1…9", "follow subagent or job N in the agents panel"
  ; "Alt+] Alt+[", "the next or previous subagent or job"
  ; "Alt+0", "close the agents panel"
  ; ( "Enter / Alt+Enter (steering a subagent)"
    , "send it a message: at its next step / when it ends its turn (its Steer \
       button in the agents panel; /agents steer N TEXT sends one)" )
  ; "Alt+↑ (steering a subagent)", "take its last queued message back"
  ; "Esc (steering a subagent)", "write to the main agent again"
  ; ( "Ctrl+Shift+F"
    , "open or close the file browser (/files): ↑↓ select, Enter opens, \
       Backspace goes up, F2 renames, Delete deletes, Esc closes" )
  ; ( "Alt+R"
    , "open or close the review page (/review): the changes file by file, \
       beside the chat" )
  ; "] / [ (reviewing; also n p, j k)", "the next / previous changed file"
  ; ( "a / c / e / v / s (reviewing)"
    , "approve the file; comment on the code selected (drag across it, in the \
       review or a changed file's preview); edit or comment; the whole file or \
       the changes; send the new comments to prigh" )
  ; ( "r / b / d (reviewing)"
    , "choose what to review (a worktree, or a branch read-only); what to \
       compare it with (a branch, tag, commit or any revision typed); since \
       the merge base or directly" )
  ; ( "l / Shift+L (reviewing)"
    , "what changed since your latest action on the file (or the hunks again); \
       choose the action it compares with: Latest follows your newest" )
  ; "Esc (reviewing)", "drop the comment or selection, then close the review"
  ; ( "Enter / Ctrl+S (reviewing)"
    , "add the comment or reply (Shift+Enter: a new line); save the file being \
       edited" )
  ; ( "Ctrl+`"
    , "open or close the terminal (/terminal); in it, every other key goes to \
       the shell" )
  ; ( "Esc (in a pull request)"
    , "back to the list it was opened from (/pr); Enter closes it" )
  ; ( "Alt+G (a pull request, commit message, branch or session name)"
    , "✦ Generate: the model drafts it from the changes, the commits and the \
       session (Esc stops, keeping what came; Undo puts the text back)" )
  ; ( "Ctrl+Enter (the review's commit message)"
    , "commit the ticked files (approved files come ticked; untick to leave \
       one out)" )
  ; ( "Ctrl+Enter (in a dialog's text box)"
    , "accept the dialog, e.g. create the pull request (Enter is a new line)" )
  ; "Tab (in /scoped-models)", "check or uncheck the highlighted model"
  ; "Delete (in /remind's list)", "remove the highlighted reminder"
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
  | Editor _
  | Field
  | Multiline
  | Session_search
  | Session _
  | Worktree _
  | Terminal
  | Files
  | Review
  | Resize _
  | Message _
  | Page -> false
;;

(* Ctrl+` by the key's position, whatever the layout types there. *)
let terminal_toggle t = ctrl_only t && String.equal t.code "Backquote"

(* The review page's keys; [r], [b] and [d] choose what it compares, [L]
   what its file's since-view does. *)
let review_key (m : App.Model.t) t : App.Action.t option =
  match t.key with
  | "r" when plain t -> Some (Review_pick Target)
  | "b" when plain t -> Some (Review_pick Base)
  | "L" when t.shift && not (t.alt || t.ctrl || t.meta) ->
    Option.map m.review.current ~f:(fun path ->
      App.Action.Review_pick (Since path))
  | "d" when plain t ->
    Option.map m.review.diff ~f:(fun d ->
      App.Action.Review_direct (not d.direct))
  | _ ->
    Code_review.key m.review ~key:t.key ~shift:t.shift ~ctrl:t.ctrl ~alt:t.alt
    |> Option.map ~f:(fun a -> App.Action.Review a)
;;

let dialog_key (dialog : Dialog.t) t : App.Action.t option =
  match t.key, dialog with
  | "Escape", _ -> Some Close_dialog
  | " ", Filter _ when plain t -> Some (Filter_press None)
  | ( "ArrowUp"
    , ( Picker _
      | Filter _
      | Popover _
      | Review_pick _
      | Login _
      | Scoped_models _
      | Prompt _
      | Pulls _ ) ) -> Some (Dialog_move (-1))
  | ( "ArrowDown"
    , ( Picker _
      | Filter _
      | Popover _
      | Review_pick _
      | Login _
      | Scoped_models _
      | Prompt _
      | Pulls _ ) ) -> Some (Dialog_move 1)
  | "PageUp", (Picker _ | Popover _ | Review_pick _ | Scoped_models _ | Pulls _)
    -> Some (Dialog_move (-8))
  | ( "PageDown"
    , (Picker _ | Popover _ | Review_pick _ | Scoped_models _ | Pulls _) ) ->
    Some (Dialog_move 8)
  (* In the model's filter ← → move the caret once something is typed. *)
  | ("ArrowLeft" | "ArrowRight"), Popover { picker; _ }
    when plain t && String.is_empty (Picker.query picker) ->
    Some (Chip_step (if String.equal t.key "ArrowLeft" then -1 else 1))
  | ("l" | "L"), Popover _ when command t -> Some (Open_chip Model)
  | "Tab", Popover _ when not (t.alt || t.ctrl || t.meta) ->
    Some (Chip_step (if t.shift then -1 else 1))
  | "Delete", Picker { kind = Reminders; picker } when plain t ->
    Option.map (Picker.selected_item picker) ~f:(fun (item : Picker.Item.t) ->
      App.Action.Remove_reminder item.id)
  | "Delete", Picker { kind = Jump; picker } when plain t ->
    Option.bind (Picker.selected_item picker) ~f:(fun (item : Picker.Item.t) ->
      match Jump.Target.of_string item.id with
      | Some ((Entry _ | Message _) as target) ->
        Some (App.Action.Ask_trim target)
      | Some Trimmed | None -> None)
  | "Delete", Picker { kind = Worktrees _; picker } when plain t ->
    Option.map (Picker.selected_item picker) ~f:(fun (item : Picker.Item.t) ->
      App.Action.Ask_remove_worktree item.id)
  | _, Picker { kind = Worktrees _; picker } when alt_letter t "r" ->
    Option.map (Picker.selected_item picker) ~f:(fun (item : Picker.Item.t) ->
      App.Action.Review_worktree { path = item.id; base = None })
  | "Enter", Picker { kind = Worktrees _; picker } when alt_only t ->
    Option.map (Picker.selected_item picker) ~f:(fun (item : Picker.Item.t) ->
      App.Action.Open_worktree { path = item.id; new_session = true })
  | _, Pr_create _ when alt_letter t "g" -> Some (Generate Pr)
  | _, New_worktree _ when alt_letter t "g" -> Some (Generate Worktree_branch)
  | _, Rename _ when alt_letter t "g" -> Some (Generate Session_title)
  | "Tab", Scoped_models _ when not t.shift -> Some Dialog_toggle
  | "Tab", Prompt { suggestions = _ :: _; _ } when not t.shift ->
    Some Dialog_complete
  | "Enter", _
    when (match t.target with
          | Multiline -> t.ctrl || t.meta
          | _ -> not t.shift)
         && not (on_control t) -> Some Dialog_accept
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
  | "Enter", _ when command t -> Some Send
  | "Enter", _ when t.shift || t.ctrl || t.meta -> None
  | "Enter", _ when t.alt -> Some Send_follow_up
  | "Enter", _ -> Some Send
  | "Escape", _ when Option.is_some m.btw -> Some Close_btw
  | "Escape", _ when not (Recipient.equal m.recipient Recipient.Main) ->
    Some (Set_recipient Main)
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
             | Editor _ | Terminal | Files | Review -> false
             | Field
             | Multiline
             | Control
             | Session_search
             | Session _
             | Worktree _
             | Resize _
             | Message _
             | Page -> true) -> Some Agents_back
  | _ -> None
;;

(* On a panel's edge: the arrows along it move it (Shift: four times as
   far). *)
let resize_key h t : Resize.Action.t option =
  let module H = Resize.Handle in
  let steps = if t.shift then 4 else 1 in
  let move by = Some (Resize.Action.Nudge (h, by * steps * H.grows h)) in
  if t.ctrl || t.alt || t.meta
  then None
  else (
    match t.key, H.vertical h with
    | "ArrowLeft", true | "ArrowUp", false -> move (-1)
    | "ArrowRight", true | "ArrowDown", false -> move 1
    | "Home", _ -> Some (Smallest h)
    | "End", _ -> Some (Largest h)
    | "Enter", _ -> Some (Reset h)
    | _ -> None)
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

let menu_key t =
  String.equal t.key "ContextMenu"
  || (String.equal t.key "F10" && t.shift && not (t.ctrl || t.alt || t.meta))
;;

let session_key (m : App.Model.t) t id : App.Action.t option =
  let session f =
    List.find m.sessions ~f:(fun s -> String.equal s.id id) |> Option.map ~f
  in
  match t.key with
  | _ when menu_key t ->
    session (fun s ->
      App.Action.Open_menu
        { target = Session s.path; anchor = Element (Session_list.dom_id s.id) })
  | "ArrowDown" when plain t -> Some (Session_nav { from = Some id; by = 1 })
  | "ArrowUp" when plain t -> Some (Session_nav { from = Some id; by = -1 })
  (* Space clicks the row's button, as Enter would. *)
  | "Enter" when m.marks.selecting && not (t.ctrl || t.alt || t.meta) ->
    session (fun s ->
      App.Action.Mark_session { path = s.path; range = t.shift })
  | "Enter" when plain t -> session (fun s -> App.Action.Switch_session s.path)
  | ("Delete" | "Backspace") when plain t ->
    session (fun s -> App.Action.Ask_delete s.path)
  | ("a" | "A") when not (t.ctrl || t.alt || t.meta) ->
    session (fun s ->
      App.Action.Set_archived { path = s.path; archived = not s.archived })
  | "Escape" when m.marks.selecting -> Some (Select_sessions false)
  | "Escape" -> Some Leave_sidebar
  | _ -> None
;;

(* A worktree's row: its menu; ↑↓ go on through the sessions. *)
let worktree_key (m : App.Model.t) t ~session ~path : App.Action.t option =
  match List.find m.sessions ~f:(fun s -> String.equal s.id session) with
  | None -> None
  | Some s ->
    (match t.key with
     | _ when menu_key t ->
       Some
         (Open_menu
            { target = Worktree { session = s.path; path }
            ; anchor = Element (Context_menu.worktree_dom_id ~session path)
            })
     | "ArrowDown" when plain t ->
       Some (Session_nav { from = Some session; by = 1 })
     | "ArrowUp" when plain t ->
       Some (Session_nav { from = Some session; by = 0 })
     | ("Delete" | "Backspace") when plain t -> Some (Ask_remove_worktree path)
     | "Escape" -> Some Leave_sidebar
     | _ -> None)
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
    | Field | Multiline -> true
    | Editor _
    | Control
    | Session_search
    | Session _
    | Worktree _
    | Terminal
    | Files
    | Review
    | Resize _
    | Message _
    | Page -> false
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

(* Code is selected and no field has the keys. *)
let commenting (m : App.Model.t) t =
  Option.is_some m.review.selection
  && Option.is_none m.review.draft
  &&
  match t.target with
  | Page | Message _ | Files | Review -> true
  | Editor _
  | Field
  | Multiline
  | Control
  | Session_search
  | Session _
  | Worktree _
  | Terminal
  | Resize _ -> false
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
  | _, [], [], None when Files.asking m.files ->
    Files.key m.files ~key:t.key ~shift:t.shift ~ctrl:t.ctrl ~alt:t.alt
    |> Option.map ~f:(fun a -> App.Action.Files a)
  | _, [], [], None ->
    (match t.key with
     | _ when terminal_toggle t -> Some Toggle_terminal
     | _
       when Files.toggle_key
              ~code:t.code
              ~shift:t.shift
              ~ctrl:t.ctrl
              ~alt:t.alt
              ~meta:t.meta -> Some (Files Toggle)
     | _
       when Code_review.toggle_key
              ~code:t.code
              ~shift:t.shift
              ~ctrl:t.ctrl
              ~alt:t.alt
              ~meta:t.meta -> Some (Review Toggle)
     | ("l" | "L") when command t -> Some (Open_chip Model)
     | ("k" | "K") when command t -> Some Open_sessions
     | ("b" | "B") when command t -> Some Toggle_sidebar
     | "," when ctrl_only t -> Some Open_settings
     | ("j" | "J") when ctrl_only t -> Some Open_jump_list
     | ("o" | "O") when ctrl_only t -> Some Cycle_verbosity
     | ("p" | "P") when ctrl_only t -> Some (Cycle_model 1)
     | _ when alt_letter t "p" -> Some (Cycle_model (-1))
     | _ when alt_letter t "t" -> Some Cycle_thinking
     | _ when alt_letter t "m" -> Some Cycle_mode
     | _ when alt_letter t "n" ->
       Some
         (Open_snippets
            (match t.target with
             | Editor { cursor } -> Some cursor
             | _ -> None))
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
     (* On a phone the file browser is a sheet: Esc closes it from anywhere. *)
     | "Escape" when m.narrow && m.files.open_ && not (Files.asking m.files) ->
       Some (Files Close)
     (* Code selected in the review or a preview: comment on it. *)
     | "c" when plain t && commenting m t -> Some (Review Start_comment)
     | _ ->
       (match agents_key m t, t.target with
        | Some action, _ -> Some action
        | None, Editor { cursor } -> editor_key m t ~cursor
        | None, Session_search -> session_search_key t
        | None, Session id -> session_key m t id
        | None, Worktree { session; path } -> worktree_key m t ~session ~path
        | None, Files ->
          Files.key m.files ~key:t.key ~shift:t.shift ~ctrl:t.ctrl ~alt:t.alt
          |> Option.map ~f:(fun a -> App.Action.Files a)
        | None, Message n when menu_key t ->
          Some
            (Open_menu
               { target = Message n; anchor = Element (Chat_view.more_id n) })
        | None, (Page | Message _) when m.review.open_ -> review_key m t
        | None, Review -> review_key m t
        | None, Resize h ->
          resize_key h t |> Option.map ~f:(fun a -> App.Action.Resize a)
        | None, (Field | Multiline | Control | Terminal | Page | Message _) ->
          None))
;;
