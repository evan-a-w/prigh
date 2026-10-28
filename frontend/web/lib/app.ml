open! Core
open! Import

module Auth_purpose = struct
  type t =
    | Refresh
    | Login_picker
    | Logout_picker
    | Show
  [@@deriving sexp_of, equal]
end

module Entries_purpose = struct
  type t =
    | Fork
    | Rewind
    | Tree
  [@@deriving sexp_of, equal]
end

module Users_purpose = struct
  type t =
    | Probe
    | Picker
  [@@deriving sexp_of, equal]
end

module Skills_purpose = struct
  type t =
    | Complete
    | Picker of string
  [@@deriving sexp_of, equal]
end

module Mcp_purpose = struct
  type t =
    | Picker
    | Reconnect
    | Refreshed of string
  [@@deriving sexp_of, equal]
end

module Forge_reply = struct
  type t =
    | Status
    | Logout of string
    | List of
        { session : string
        ; filter : Pull_requests.Filter.t
        }
    | View of
        { session : string
        ; number : int
        }
    | Defaults of string
    | Created
    | Checkout of
        { session : string
        ; number : int
        }
  [@@deriving sexp_of, equal]
end

module Worktrees_purpose = struct
  type t =
    | Sidebar of int
    | Picker
    | Review_pick
    | Target of
        { command : string
        ; action : Prigh_ui.Worktree_command.Action.t
        }
    | Review_target of
        { command : string
        ; name : string
        ; base : string option
        ; direct : bool
        ; model : string option
        }
  [@@deriving sexp_of, equal]
end

module Trim_purpose = struct
  type t =
    | Picker
    | Before of
        { from_end : int
        ; at : Time_ns.Alternate_sexp.t option
        }
  [@@deriving sexp_of, equal]
end

module Points_purpose = struct
  type t =
    | Delete_picker
    | Trim_after_picker
    | Trim_before of
        { index : int
        ; at : Time_ns.Alternate_sexp.t option
        }
    | Delete of
        { entry : int
        ; index : int
        ; at : Time_ns.Alternate_sexp.t option
        }
    | Trim_after of
        { index : int
        ; last : int
        ; at : Time_ns.Alternate_sexp.t option
        }
  [@@deriving sexp_of, equal]
end

module Reply_tag = struct
  type t =
    | Ignore
    | Show_error
    | State
    | Page of
        { path : string
        ; before : string option (** [None]: the newest page *)
        }
    | Tool_output of
        { agent_id : string option
        ; call_id : string
        }
    | Outline of string
    | Pending
    | Sent of
        { text : string
        ; images : Image.t list
        }
    | Steered of
        { agent : string
        ; draft : string
        }
    | Sessions
    | Models
    | Reload_state
    | Subagent of string (** the [subagent] call or agent id *)
    | Subagents
    | Jobs
    | Job_output of string
    | Job_started
    | Auth_status of Auth_purpose.t
    | Paths of string
    | Restored
    | Dequeued
    | Unqueued of string
    | Sessions_deleted
    | Archived_set of
        { title : string
        ; archived : bool
        }
    | Refresh_sessions
    | Login_started
    | Notice of string
    | Reminder of
        { command : string
        ; notice : string option
        }
    | Reconnect of int
    | Config
    | Config_saved of string
    | Fallback_saved
    | Default_dir_saved
    | Compaction_saved
    | Default_saved
    | Session_stats
    | Entries of Entries_purpose.t
    | Reload_messages
    | Exported
    | Imported
    | Dir_changed of Dir_change.Target.t
    (** [Missing] asks to create it; the prompt or dialog closes once changed *)
    | Prompt_paths of string
    | Btw of string
    | Users of Users_purpose.t
    | User_switched
    | Skills of
        { place : string
        ; purpose : Skills_purpose.t
        }
    | Mcp of Mcp_purpose.t
    | Worktrees of
        { place : string
        ; purpose : Worktrees_purpose.t
        }
    | Worktree_created of { command : string }
    | Review_checked_out of Code_review.Target.t
    | Worktree_entered of string
    | Worktree_went_back of
        { command : string
        ; home : Session_home.t option
        }
    | Worktree_session of
        { path : string
        ; notice : string
        }
    | Worktree_removed of
        { target : string
        ; force : bool
        }
    | Repo_worktrees of string
    | Plan_switched of Context_menu.Plan.t
    | Plan_entered of Context_menu.Plan.t
    | Mode_set of string
    | Model_set
    | Filter_saved of string
    | Thinking_set
    | Git_diff
    | Command_prompt of string (** [/review] or [/init] as typed *)
    | Files of Files.Reply.t
    | Review of Code_review.Reply.t
    | Sent_with_files of
        { text : string
        ; images : Image.t list
        ; attachments : Files.Attachment.t list
        }
    | Resync of
        { session : string (** its id *)
        ; from : string option
        }
    (** the session's messages after reconnecting, [from] the first one on
        show *)
    | Resync_pending
    | Answer
    (** to a confirmation or a question: asked again when it does not
        reach the agent *)
    | Session_gone of
        { generation : int
        ; session : string
        } (** reconnected, but the backend no longer has [session] *)
    | Forge of Forge_reply.t
    | Prefs
    | Generated of string
    | Git of Git_bar.Reply.t
    | Nix_status of { open_settings : bool }
    | Nix_collected
    | Trim_points of Trim_purpose.t
    | Trimmed
    | Untrimmed
    | Trimmed_messages of string
    | Message_points of Points_purpose.t
    | Deleted of { entry : int option }
    | Trimmed_after
    | Range_restored of string
    | Hidden_messages of
        { path : string
        ; entry : string
        }
  [@@deriving sexp_of, equal]
end

module Command = struct
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * Json.t) list
        ; tag : Reply_tag.t
        }
    | Reconnect of
        { generation : int
        ; delay_ms : int
        ; session : string option
        }
    | Set_url_session of string
    | Expire_toast of
        { id : int
        ; after_ms : int
        }
    | Focus of string
    | Clear_text_selection
    | Place_popover
    | Place_menu of Context_menu.Anchor.t
    | Open_tab of string
    | Set_caret of int
    (** the editor's caret, at this byte offset of the draft, once rendered
        (and focused): after text was put in at the caret *)
    | Sign_out
    | Reveal of string list
    (** scroll the main chat to a subagent's card: the call ids from the
        top-level one down *)
    | Copy of string
    | Switch_account of Accounts.Account.t
    | Add_account
    | Scroll_chat of int
    | Jump_to_user_message of int
    | Scroll_to_entry of int
    | Scroll_to_bottom
    | Scroll_to_trimmed
    | Scroll_to_hidden of string
    | Scroll_to_message of int
    | Focus_terminal
    | Notify of Prigh_ui.Notify.t
    | Request_notification_permission
    | Set_title of string
    | Files of Files.Page.t
    | Cache_prefs of Prefs.Change.t list
    | Apply_theme of Theme.t
    | Keep_scroll_position
    (** older messages are being added above: what shows stays in place *)
    | Probe_connection
    (** the socket may have died unnoticed (a suspended page, a new network):
        drop it unless the backend answers soon *)
    | Drop_connection
    | Reload_page of { keep_session : bool }
    (** the connection keeps dropping: load the page again, in this
        session or not *)
    | Auth_failed of string
    (** the backend refused the saved sign-in: the sign-in page, saying this *)
  [@@deriving sexp_of, equal]
end

module Connection = struct
  type t =
    | Connected
    | Reconnecting of
        { attempt : int
        ; generation : int
        }
  [@@deriving sexp_of, equal]

  let delay_ms ~attempt =
    if attempt <= 0
    then 0
    else Int.min 30_000 (250 * (1 lsl Int.min 10 (attempt - 1)))
  ;;
end

module Toast = Toast
module Drops = Drops

module Action = struct
  type t =
    | Start
    | Hello of Hello_reply.t
    | Saved_login
    | Event of Event.t
    | Protocol_error of string
    | Backend_closed
    | Reply of Reply_tag.t * (Json.t, string) Result.t
    | Tick of Time_ns.t
    | Set_utc_offset of Time_ns.Span.t
    | Set_narrow of bool
    | Page_hidden of bool
    | Restore_prefs of
        { prefs : Prefs.Change.t list
        ; legacy : bool
        }
    | Set_notification_permission of string
    | Notification_permission of string
    | Set_draft of string
    | Edit of
        { text : string
        ; cursor : int
        }
    | Send
    | Send_follow_up
    | Abort
    | History_older
    | History_newer
    | Complete_move of int
    | Complete_accept of { run : bool }
    | Complete_choose of int
    | Complete_close
    | New_session
    | Switch_session of string
    | Ask_delete of string
    | Select_sessions of bool
    | Mark_session of
        { path : string
        ; range : bool
        }
    | Delete_marked
    | Ask_delete_archived
    | Set_archived of
        { path : string
        ; archived : bool
        }
    | Set_session_query of string
    | Toggle_archived
    | Open_sessions
    | Session_nav of
        { from : string option
        ; by : int
        }
    | Open_first_session
    | Leave_sidebar
    | Set_model of string
    | Set_thinking of string
    | Open_chip of Chip.t
    | Open_filter (** the transcript filter's menu; again: closes it *)
    | Filter_press of Filter_menu.Row.t option
    (** a row of the filter's menu (by click), or the one selected (Space) *)
    | Chip_step of int
    | Open_snippets of int option
    | Open_help
    | Open_settings
    | Open_rename
    | Open_subagents of string option
    (** the agents panel; with a number, an agent's or a job's id *)
    | Toggle_subagents
    | Select_item of Agents.Item.t
    | Agents_back
    | Focus_agent of int
    | Cycle_agent of int
    | Show_in_chat of string
    | Clock of Time_ns.t
    | Picker_query of string
    | Picker_move of int
    | Picker_accept
    | Picker_choose of string
    | Dialog_input of string
    | Dialog_move of int
    | Dialog_accept
    | Close_dialog
    | Login_choose of int
    | Start_login of string
    | Logout of string
    | Cancel_subagent of string
    | Kill_job of string
    | Dequeue
    | Edit_queued of Recipient.t * Queued.Message.t
    | Remove_queued of Recipient.t * Queued.Message.t
    | Set_recipient of Recipient.t
    | Remove_reminder of string
    | Toggle_folded of string
    | New_session_in of string
    | New_worktree
    | New_worktree_input of
        { field : New_worktree.Field.t
        ; text : string
        }
    | Open_worktree of
        { path : string
        ; new_session : bool
        }
    | Ask_remove_worktree of string
    | Remove_worktree of { force : bool }
    | Open_menu of Context_menu.t
    | Toggle_sidebar
    | Answer_confirm of
        { call_id : string
        ; agent_id : string option
        ; answer : Confirm_prompt.Answer.t
        }
    | Confirm_reason_input of string
    | Send_confirm_reason
    | Cancel_confirm_reason
    | Question of Question_dialog.Action.t
    | Cycle_mode
    | Add_image of Image.t
    | Remove_image of int
    | Show_toast of
        { text : string
        ; error : bool
        }
    | Dismiss_toast of int
    | Sign_out
    | Set_accounts of
        { accounts : Accounts.Account.t list
        ; current : Accounts.Account.t option
        }
    | Open_accounts
    | Cycle_verbosity
    | Toggle_fold of string
    | Cycle_model of int
    | Cycle_thinking
    | Copy_last
    | Close_btw
    | Dialog_toggle
    | Toggle_scoped of string
    | Dialog_complete
    | Choose_suggestion of int
    | Retry_connection
    | Reload_page of { keep_session : bool }
    | Scroll_chat of int
    | Jump_to_user_message of int
    | Jump_to of Jump.Target.t
    | Jump_older
    | Open_jump_list
    | Ask_trim of Jump.Target.t
    | Show_trimmed of bool
    | Untrim
    | Ask_delete_message of int
    | Ask_trim_after of int
    | Restore_hidden of Hidden.t
    | Restore_range of string
    | Show_hidden of
        { entry : string
        ; show : bool
        }
    | Chat_scrolled of { at_bottom : bool }
    | Jump_to_bottom
    | Run of string
    | Toggle_terminal
    | Close_terminal
    | New_shell
    | Terminal_status of
        { key : string
        ; status : Terminal.Status.t
        }
    | Files of Files.Action.t
    | Review of Code_review.Action.t
    | Review_pick of Review_picker.Which.t
    | Review_direct of bool
    | Review_checkout
    | Review_worktree of
        { path : string
        ; base : string option
        }
    | Resize of Resize.Action.t
    | Load_older
    | Prefetch_session of string (** a path *)
    | Load_output of string (** a [Fold] key *)
    | Network of bool (** the browser's [online] and [offline] events *)
    | Pr_filter of Pull_requests.Filter.t
    | Pr_open of int
    | Pr_checkout of int
    | New_pr
    | Pr_form of Pr_create.Action.t
    | Preview_accent of string
    | Generate of Generate.Target.t
    | Generate_stop
    | Generate_undo of Generate.Target.t
    | Git of Git_bar.Action.t
  [@@deriving sexp_of]
end

module Model = struct
  type t =
    { connection : Connection.t
    ; generation : int
    ; drops : Drops.t
    ; hello : Hello_reply.t option
    ; saved_login : bool
    ; state : State.t option
    ; chat : Chat.t
    ; sessions : Session_summary.t list
    ; models : Llm.t list
    ; auth : Auth_status.t list
    ; now : Time_ns.t option
    ; utc_offset : Time_ns.Span.t
    ; narrow : bool
    ; draft : string
    ; cursor : int
    ; completion : Completion.t option
    ; history : History.t
    ; images : Image.t list
    ; queue : Queued.t
    ; agent_queues : Agent_queue.t list
    ; recipient : Recipient.t
    ; confirms : Pending.Confirm.t list
    ; confirm_reason : string option
    ; questions : Question_dialog.t list
    ; dialog : Dialog.t option
    ; cancelled_login : string option
    ; toasts : Toast.t list
    ; next_toast : int
    ; sidebar_open : bool
    ; scrolled_up : bool
    ; session_query : string
    ; archived_open : bool
    ; agents : Agents.t
    ; verbosity : Prigh_ui.Verbosity.t
    ; folds : String.Set.t
    ; transcript_filter : Transcript_filter.t
    ; config : Config.t option
    ; btw : Btw.t option
    ; btw_seq : int
    ; accounts : Accounts.Account.t list
    ; account : Accounts.Account.t option
    ; users : string list option
    ; terminal : Terminal.t
    ; skills : Skills.t
    ; worktrees : Worktrees.t
    ; repo_worktrees : Repo_listings.t
    ; folded : String.Set.t
    ; pending : In_flight.t
    ; marks : Session_marks.t
    ; notifications : Page_notify.t
    ; files : Files.t
    ; review : Code_review.t
    ; review_memory : Review_memory.t
    ; layout : Resize.t
    ; paging : Paging.t
    ; chat_cache : Chat_cache.t
    ; jump : Jump.t
    ; trimmed : Trimmed.t
    ; outputs_loading : String.Set.t (** [Fold] keys *)
    ; session_gone : string option
      (** reconnected to a backend that no longer has this session: its
          transcript stays on show, read-only *)
    ; forges : Forges.t
    ; nix_gc : Nix_gc.t
    ; return_to_settings : bool
    ; theme : Theme.t
    ; theme_preview : Theme.t option
    ; prefs : Pref_sync.t
    ; generate : Generate.t
    ; git_bar : Git_bar.t
    }
  [@@deriving sexp_of]

  let look t =
    match t.dialog with
    | Some (Picker { kind = Themes; picker }) ->
      (match Picker.selected_item picker with
       | Some item ->
         { t.theme with
           name =
             Option.value (Theme.Name.of_string item.id) ~default:t.theme.name
         }
       | None -> t.theme)
    | Some (Picker { kind = Accents; picker }) ->
      let typed = Theme.Accent.of_string (Picker.query picker) in
      (match Picker.selected_item picker, typed with
       | _, Some (Custom _ as accent) -> { t.theme with accent }
       | Some item, _ ->
         { t.theme with
           accent =
             Option.value
               (Theme.Accent.of_string item.id)
               ~default:t.theme.accent
         }
       | None, _ -> t.theme)
    | _ -> Option.value t.theme_preview ~default:t.theme
  ;;

  let shown t =
    { Resize.Shown.sidebar = t.sidebar_open
    ; files = t.files.open_
    ; preview = Option.is_some t.files.preview
    ; agents = t.agents.open_
    ; reviewing = t.review.open_
    ; narrow = t.narrow
    }
  ;;

  let running t =
    match t.state with
    | Some s -> s.running
    | None -> false
  ;;

  let docked t = t.sidebar_open && not t.narrow
  let waiting t = not (List.is_empty t.confirms && List.is_empty t.questions)

  let active t =
    match t.state with
    | Some s ->
      s.running
      || List.exists s.subagents ~f:(fun a -> a.running)
      || List.exists s.jobs ~f:(fun j -> j.running)
    | None -> false
  ;;

  let popup t =
    Option.filter t.completion ~f:(fun c ->
      not (List.is_empty (Completion.items c)))
  ;;

  let ticking t =
    t.agents.open_
    && (Agents.running_agents t.agents + Agents.running_jobs t.agents > 0
        || Agents.fetching t.agents)
  ;;

  let fold t =
    Fold.create
      ~filter:t.transcript_filter
      ~verbosity:t.verbosity
      ~toggled:t.folds
      ()
  ;;

  let message_index t = Message_index.create ~paging:t.paging t.chat

  let context_start t =
    if t.paging.older = 0 then Chat.first t.chat else Chat.first t.chat - 1
  ;;

  let message_menu t n : Context_menu.Message.t option =
    let index = message_index t in
    let menu (_ : Message_index.Span.t) ~reply ~calls text =
      Some
        { Context_menu.Message.reply
        ; calls
        ; text
        ; first = n <= context_start t
        ; newest = [%equal: int option] (Message_index.newest index) (Some n)
        ; running = running t
        }
    in
    match
      ( Message_index.span index n
      , List.nth (Chat.entries t.chat) (n - Chat.first t.chat) )
    with
    | Some span, Some (User u) when Option.is_none (Message.User.source u) ->
      let body = Message.User.body u in
      menu
        span
        ~reply:false
        ~calls:0
        (Option.value_map (Skill_message.parse body) ~default:body ~f:(fun s ->
           s.args))
    | Some span, Some (Assistant { message; streaming = false }) ->
      menu
        span
        ~reply:true
        ~calls:
          (List.count message.content ~f:(function
             | Tool_call _ -> true
             | Text _ | Thinking _ | Server_tool _ -> false))
        (List.filter_map message.content ~f:(function
           | Text t when not (String.is_empty (String.strip t)) ->
             Some (String.strip t)
           | Text _ | Thinking _ | Tool_call _ | Server_tool _ -> None)
         |> String.concat ~sep:"\n\n")
    | _ -> None
  ;;

  let message_time t =
    Message_time.create
      ~now:(Option.value t.now ~default:Time_ns.epoch)
      ~utc_offset:t.utc_offset
  ;;

  let kept_from_deletion t (s : Session_summary.t) =
    let current =
      Option.exists t.state ~f:(fun state -> String.equal state.session_id s.id)
    in
    if s.running
    then Some "is running: it will be kept (wait for it or stop it first)"
    else if current
    then
      Some
        "is the session you are in: it will be kept (switch to another first)"
    else if s.clients > 0
    then
      Some
        "is open in another tab or terminal: it will be kept (close it there \
         first)"
    else None
  ;;
end

let thinking_levels = Prigh_ui.Commands.thinking_levels

let init =
  { Model.connection = Connected
  ; drops = Drops.initial
  ; generation = 0
  ; hello = None
  ; saved_login = false
  ; state = None
  ; chat = Chat.empty
  ; sessions = []
  ; models = []
  ; auth = []
  ; now = None
  ; utc_offset = Time_ns.Span.zero
  ; narrow = false
  ; draft = ""
  ; cursor = 0
  ; completion = None
  ; history = History.empty
  ; images = []
  ; queue = Queued.empty
  ; agent_queues = []
  ; recipient = Main
  ; confirms = []
  ; confirm_reason = None
  ; questions = []
  ; dialog = None
  ; cancelled_login = None
  ; toasts = []
  ; next_toast = 0
  ; sidebar_open = true
  ; scrolled_up = false
  ; session_query = ""
  ; archived_open = false
  ; agents = Agents.empty
  ; verbosity = Normal
  ; folds = String.Set.empty
  ; transcript_filter = Transcript_filter.all
  ; config = None
  ; btw = None
  ; btw_seq = 0
  ; accounts = []
  ; account = None
  ; users = None
  ; terminal = Terminal.closed
  ; skills = Skills.empty
  ; worktrees = Worktrees.empty
  ; repo_worktrees = Repo_listings.empty
  ; folded = String.Set.empty
  ; pending = In_flight.none
  ; marks = Session_marks.empty
  ; notifications = Page_notify.init
  ; files = Files.closed
  ; review = Code_review.empty
  ; review_memory = Review_memory.empty
  ; layout = Resize.empty
  ; paging = Paging.all
  ; chat_cache = Chat_cache.empty
  ; jump = Jump.empty
  ; trimmed = Trimmed.empty
  ; outputs_loading = String.Set.empty
  ; session_gone = None
  ; forges = Forges.empty
  ; nix_gc = Nix_gc.empty
  ; return_to_settings = false
  ; theme = Theme.default
  ; theme_preview = None
  ; prefs = Pref_sync.init
  ; generate = Generate.empty
  ; git_bar = Git_bar.empty
  }
;;

let rpc ?(tag = Reply_tag.Show_error) method_ params =
  Command.Rpc { method_; params; tag }
;;

let call ?tag (method_ : Request.Method.t) =
  rpc ?tag (Request.Method.name method_) (Request.Method.params method_)
;;

let str s = `String s

let toast (m : Model.t) ?(error = false) ?replaces ?undo text =
  let toasts, toast =
    Toast.push m.toasts ~id:m.next_toast ?replaces ?undo ~error text
  in
  ( { m with toasts; next_toast = m.next_toast + 1 }
  , match Toast.lifetime_ms toast with
    | None -> []
    | Some after_ms -> [ Command.Expire_toast { id = toast.id; after_ms } ] )
;;

let error m text = toast m ~error:true text

(* What a trim, or its undo, did to the context: only the latest shows. *)
let context_toast m text =
  toast m ~replaces:[ "Trimmed "; "Back in the context: " ] text
;;

(* At most [max] bytes, cut at a character. *)
let clamp ?(max = 60) text =
  if String.length text <= max
  then text
  else (
    let rec cut i =
      if i > 0 && Char.to_int text.[i] land 0xC0 = 0x80 then cut (i - 1) else i
    in
    String.prefix text (cut (max - 1)) ^ "…")
;;

let drop_toast (m : Model.t) text =
  { m with
    toasts = List.filter m.toasts ~f:(fun t -> not (String.equal t.text text))
  }
;;

(* What a call made while the connection is down says. *)
let not_connected =
  "Not connected to the backend: try again once it reconnects."
;;

let reading_changes = "Reading the changes…"
let reading_review = "Reading the changes to review…"
let focus_editor = Command.Focus "editor"

let startup =
  [ rpc "get_state" [] ~tag:State
  ; rpc "list_models" [] ~tag:Models
  ; rpc "auth_status" [] ~tag:(Auth_status Refresh)
  ; rpc "get_config" [] ~tag:Config
  ; call Forge_status ~tag:(Forge Status)
  ]
;;

let decode (m : Model.t) json of_json ~f =
  match of_json json with
  | Ok value -> f value
  | Error e -> error m (Error.to_string_hum e)
;;

let decode_list of_json json =
  match json with
  | `Array items -> Or_error.all (List.map items ~f:of_json)
  | _ -> Or_error.error_string "expected an array"
;;

let strings json =
  decode_list
    (function
      | `String s -> Ok s
      | _ -> Or_error.error_string "expected a string")
    json
;;

let field json name =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let close_dialog (m : Model.t) =
  match m.return_to_settings, m.dialog with
  | true, Some (Login _ | Forge_host _ | Nix_gc_confirm _) ->
    ( { m with
        dialog = Some Settings
      ; return_to_settings = false
      ; theme_preview = None
      }
    , [ Command.Focus "dialog" ] )
  | _, Some (Review_pick p) ->
    ( { m with dialog = None; return_to_settings = false; theme_preview = None }
    , [ Command.Focus
          (match Review_picker.which p with
           | Target -> "review-target-button"
           | Base -> "review-base-button"
           | Since _ when m.review.open_ -> "review-since-button"
           | Since _ -> "files-since-button")
      ] )
  | _, Some (Picker { kind = Menu { target = Message n; _ }; _ })
    when Option.is_some (Model.message_menu m n) ->
    ( { m with dialog = None; return_to_settings = false; theme_preview = None }
    , [ Command.Focus (Chat_view.message_id n) ] )
  | _ ->
    ( { m with dialog = None; return_to_settings = false; theme_preview = None }
    , [ focus_editor ] )
;;

let list_subagents = rpc "list_subagents" [] ~tag:Subagents
let list_jobs = rpc "list_jobs" [] ~tag:Jobs

let cancel_btw (m : Model.t) =
  match m.btw with
  | Some { status = Streaming; id; _ } ->
    [ rpc "btw_cancel" [ "btw_id", str id ] ~tag:Ignore ]
  | _ -> []
;;

let cancel_generate (m : Model.t) =
  match m.generate.running with
  | Some r -> [ call (Generate_cancel { gen_id = r.id }) ~tag:Ignore ]
  | None -> []
;;

(* A new session (switched, new, forked) starts from its own messages. *)
(* ---- the file browser and the attachments *)

let files_out (m : Model.t) (outs : Files.Out.t list) =
  List.fold outs ~init:(m, []) ~f:(fun (m, cmds) (out : Files.Out.t) ->
    match out with
    | Rpc { method_; params; reply } ->
      m, cmds @ [ rpc method_ params ~tag:(Files reply) ]
    | Page page -> m, cmds @ [ Command.Files page ]
    | Toast { text; error } ->
      let m, more = toast m ~error text in
      m, cmds @ more
    | Copy text -> m, cmds @ [ Command.Copy text ]
    | Insert text ->
      let draft =
        if
          String.is_empty m.draft
          || Char.is_whitespace (String.get m.draft (String.length m.draft - 1))
        then m.draft ^ text ^ " "
        else m.draft ^ " " ^ text ^ " "
      in
      { m with draft; cursor = String.length draft }, cmds @ [ focus_editor ]
    | Focus_editor -> m, cmds @ [ focus_editor ])
;;

let review_out (m : Model.t) (outs : Code_review.Out.t list) =
  List.fold outs ~init:(m, []) ~f:(fun (m, cmds) (out : Code_review.Out.t) ->
    match out with
    | Rpc { method_; reply } -> m, cmds @ [ call method_ ~tag:(Review reply) ]
    | Get_diff { reviewed; base; direct } ->
      let diff =
        rpc
          "git_diff"
          (Git_diff.params ?base ~direct ~target:reviewed ())
          ~tag:(Review Diff)
      in
      (* The page's git bar follows the worktree it reviews; a branch that
         is not checked out has none. *)
      (match reviewed with
       | Dir dir when m.review.open_ ->
         let git_bar, outs = Git_bar.watch m.git_bar ~dir in
         ( { m with git_bar }
         , cmds
           @ [ diff ]
           @ List.filter_map outs ~f:(function
             | Rpc { method_; reply } -> Some (call method_ ~tag:(Git reply))
             | Draft_message | Changed | Create_pr _ -> None) )
       | Dir _ | Rev _ -> m, cmds @ [ diff ])
    | Toast { text; error } ->
      let m, more = toast m ~error text in
      m, cmds @ more
    | Focus id -> m, cmds @ [ Command.Focus id ]
    | Clear_text_selection -> m, cmds @ [ Command.Clear_text_selection ])
;;

let with_review (m : Model.t) (review, outs) = review_out { m with review } outs

(* What the agent may have changed in the working tree: an edit, a write, a
   command, its own or a subagent's; and a run's end. *)
let rec touches_files (e : Event.t) =
  match e with
  | Tool_end { call; _ } ->
    List.mem
      [ "edit"; "write"; "apply_patch"; "bash" ]
      call.name
      ~equal:String.equal
  | Subagent { event; _ } -> touches_files event
  | Agent_end _ -> true
  | _ -> false
;;

let with_files (m : Model.t) (files, outs) =
  let was_open = m.files.open_ in
  let m, cmds = files_out { m with files } outs in
  (* The browser marks what changed: it reads the changes when it opens. *)
  if m.files.open_ && not was_open
  then (
    let m, more = with_review m (Code_review.refresh m.review) in
    m, cmds @ more)
  else m, cmds
;;

let files_where (m : Model.t) =
  Option.map m.state ~f:(fun (state : State.t) ->
    let name, _ = Terminal.where m.terminal state in
    { Files.Where.session = state.session_id
    ; host = state.active_host
    ; host_name = name
    ; cwd = state.cwd
    })
;;

(* ---- the transcript a page at a time *)

let page_rpc path ~before =
  call
    (Get_messages_page
       { session = path; before; from = None; limit = Paging.size })
    ~tag:(Page { path; before })
;;

(* Again what is on show and what came since: from its first message when
   older ones are not loaded, else all of them (or the newest page while the
   first one is still on its way). *)
let resync_rpc (m : Model.t) (state : State.t) =
  let from = if m.pending.messages then None else m.paging.before in
  let limit =
    if m.pending.messages || Option.is_some from
    then Paging.size
    else Paging.everything
  in
  call
    (Get_messages_page
       { session = state.session_path; before = None; from; limit })
    ~tag:(Resync { session = state.session_id; from })
;;

let session_path (m : Model.t) = Option.map m.state ~f:(fun s -> s.session_path)

let session_id (m : Model.t) =
  Option.value_map m.state ~default:"" ~f:(fun s -> s.session_id)
;;

let is_current (m : Model.t) path =
  Option.equal String.equal (session_path m) (Some path)
;;

(* The transcript we leave stays at hand for coming back to it, its newest
   page forgotten if it changed since it came (it is refreshed then). *)
let keep_chat (m : Model.t) =
  match session_path m with
  | Some path when not m.pending.messages ->
    let entry =
      match Chat_cache.find m.chat_cache path with
      | Some cached when phys_equal cached.chat m.chat -> cached
      | Some _ | None ->
        { Chat_cache.Entry.chat = m.chat
        ; paging = { m.paging with loading = false }
        ; newest = []
        }
    in
    Chat_cache.set m.chat_cache path entry
  | Some _ | None -> m.chat_cache
;;

(* The newest page of [path], unless on its way. *)
let fetch_newest (m : Model.t) path =
  if Chat_cache.loading m.chat_cache path
  then m, []
  else
    ( { m with chat_cache = Chat_cache.set_loading m.chat_cache path true }
    , [ page_rpc path ~before:None ] )
;;

let subagent_rpc call_id =
  rpc
    "get_subagent"
    [ "id", str call_id; "elide", `True ]
    ~tag:(Subagent call_id)
;;

(* The session's transcript filter, as the backend keeps it. *)
let session_filter (state : State.t) =
  Option.value_map
    state.transcript_filter
    ~default:Transcript_filter.all
    ~f:Transcript_filter.of_string
;;

(* Another frontend changed the session's filter (or ours came back): an
   open menu keeps what the user toggled in it, and Esc puts back the
   session's. *)
let filter_changed (m : Model.t) filter =
  match m.dialog with
  | Some (Filter menu) ->
    let transcript_filter =
      if Transcript_filter.equal m.transcript_filter menu.original
      then filter
      else m.transcript_filter
    in
    { m with
      dialog = Some (Filter { menu with original = filter })
    ; transcript_filter
    }
  | _ -> { m with transcript_filter = filter }
;;

let set_state_without_files (m : Model.t) (state : State.t) =
  let chat_cache = keep_chat m in
  let changed =
    match m.state with
    | Some old -> not (String.equal old.session_id state.session_id)
    | None -> true
  in
  let background_changed =
    match m.state with
    | Some old ->
      [ ( not ([%equal: State.Subagent.t list] old.subagents state.subagents)
        , list_subagents )
      ; not ([%equal: State.Job.t list] old.jobs state.jobs), list_jobs
      ]
      |> List.filter_map ~f:(fun (changed, cmd) -> Option.some_if changed cmd)
    | None -> []
  in
  let pending =
    let confirmed key = Option.filter ~f:(Fn.non (String.equal key)) in
    { m.pending with
      model = confirmed state.model.key m.pending.model
    ; thinking =
        Option.bind m.pending.thinking ~f:(fun level ->
          Option.some_if state.model.supports_thinking level)
        |> confirmed state.thinking
    ; mode =
        Option.filter
          m.pending.mode
          ~f:(Fn.non (Session_mode.equal (Session_mode.of_state state)))
    }
  in
  let trim_changed =
    Option.exists m.state ~f:(fun (old : State.t) ->
      not
        ([%equal: Trim.t option] old.trim state.trim
         && [%equal: Hidden.t list] old.hidden state.hidden))
  in
  let m =
    match m.state with
    | _ when changed -> { m with transcript_filter = session_filter state }
    | Some old
      when m.pending.filters > 0
           || [%equal: string option]
                old.transcript_filter
                state.transcript_filter -> m
    | Some _ | None -> filter_changed m (session_filter state)
  in
  let m = { m with state = Some state; pending } in
  if changed
  then (
    let cached = Chat_cache.find chat_cache state.session_path in
    let m, fetch = fetch_newest { m with chat_cache } state.session_path in
    ( { m with
        pending =
          { In_flight.none with
            messages = Option.is_none cached
          ; refreshing =
              Option.is_some cached
              && Chat_cache.loading m.chat_cache state.session_path
          }
      ; chat =
          (match cached with
           | Some c when state.running ->
             Chat.set_running (Chat.missed_output c.chat) true
           | Some c -> Chat.set_running c.chat false
           | None -> Chat.set_running Chat.empty state.running)
      ; paging =
          Option.value_map cached ~default:Paging.all ~f:(fun c -> c.paging)
      ; jump = Jump.empty
      ; trimmed = Trimmed.empty
      ; outputs_loading = String.Set.empty
      ; folds = String.Set.empty
      ; confirms = []
      ; confirm_reason = None
      ; questions = []
      ; queue = Queued.empty
      ; agent_queues = []
      ; recipient = Main
      ; dialog = Option.filter m.dialog ~f:(Fn.non Dialog.per_session)
      ; agents = { Agents.empty with open_ = m.agents.open_ && not m.narrow }
      ; btw = None
      ; generate = Generate.empty
      ; git_bar = Git_bar.empty
      ; completion = None
      ; scrolled_up = false
      ; skills = Skills.empty
      ; worktrees = Worktrees.reset m.worktrees
      ; toasts =
          List.filter m.toasts ~f:(fun t ->
            not (t.error || Option.is_some t.undo))
      ; session_gone = None
      }
    , cancel_btw m
      @ cancel_generate m
      @ [ Command.Set_url_session state.session_id ]
      @ fetch
      @ (if Option.is_some cached then [ Command.Scroll_to_bottom ] else [])
      @ [ rpc "get_pending" [] ~tag:Pending
        ; rpc "list_sessions" [] ~tag:Sessions
        ; list_subagents
        ; list_jobs
        ] ))
  else if trim_changed
  then (
    (* The context starts elsewhere: the transcript, which is the context,
       again from the backend. *)
    let path = state.session_path in
    let m, fetch =
      fetch_newest
        { m with chat_cache = Chat_cache.remove m.chat_cache path }
        path
    in
    ( { m with
        chat = Chat.set_running m.chat state.running
      ; jump = Jump.empty
      ; trimmed =
          { m.trimmed with
            shown = Folded
          ; ranges =
              Map.filter_keys m.trimmed.ranges ~f:(fun entry ->
                List.exists state.hidden ~f:(fun h ->
                  String.equal h.entry entry))
          }
      }
    , background_changed @ fetch ))
  else
    { m with chat = Chat.set_running m.chat state.running }, background_changed
;;

let set_state (m : Model.t) (state : State.t) =
  let m, cmds = set_state_without_files m state in
  let m, more = with_files m (Files.set_where m.files (files_where m)) in
  let m, review =
    with_review
      m
      (Code_review.set_session
         m.review
         (Some state.session_id)
         ~watching:m.files.open_)
  in
  m, cmds @ more @ review
;;

let logged_in (m : Model.t) provider =
  List.exists m.auth ~f:(fun s ->
    String.equal s.provider provider && Option.is_some s.configured)
;;

let compact_tokens n =
  if n >= 1_000_000
  then
    sprintf
      "%gM"
      (Float.round_decimal ~decimal_digits:1 (Float.of_int n /. 1e6))
  else if n >= 1000
  then sprintf "%dk" (n / 1000)
  else Int.to_string n
;;

(* Those that can be used first, each provider's together. *)
let models_picker ?(query = "") (m : Model.t) =
  let current = Option.map m.state ~f:(fun s -> s.model.key) in
  let known = not (List.is_empty m.auth) in
  let usable (model : Llm.t) = (not known) || logged_in m model.provider in
  let first (model : Llm.t) =
    List.findi m.models ~f:(fun _ (other : Llm.t) ->
      String.equal other.provider model.provider)
    |> Option.value_map ~default:0 ~f:fst
  in
  let models =
    List.stable_sort m.models ~compare:(fun a b ->
      match Bool.compare (usable b) (usable a) with
      | 0 -> Int.compare (first a) (first b)
      | c -> c)
  in
  let items =
    List.map models ~f:(fun model ->
      Picker.Item.create
        ~id:model.key
        ~detail:
          (sprintf
             "%s, %s ctx%s"
             model.provider
             (compact_tokens model.context_window)
             (if usable model then "" else ", not logged in"))
        ~search:(model.name ^ " " ^ model.provider ^ " " ^ model.key)
        ~marked:(Option.equal String.equal current (Some model.key))
        ~dimmed:(not (usable model))
        model.name)
  in
  Picker.create ~query ~title:"Switch model" items
;;

let model_picker ?query m =
  Dialog.Picker { kind = Models; picker = models_picker ?query m }
;;

let thinking_levels_picker (m : Model.t) =
  let current = Option.value_map m.state ~default:"" ~f:(fun s -> s.thinking) in
  let detail = function
    | "off" -> "answer straight away"
    | "low" -> "a little"
    | "on" -> "the provider's default budget"
    | "high" -> "more"
    | "max" -> "as much as the model allows"
    | _ -> ""
  in
  Picker.create
    ~title:"Thinking level"
    (List.map thinking_levels ~f:(fun level ->
       Picker.Item.create
         ~id:level
         ~detail:(detail level)
         ~marked:(String.equal level current)
         level))
;;

let custom_detail (c : Auth_status.Custom.t) =
  sprintf "custom, %s, %s" c.base_url c.api_label
;;

let key_source (s : Auth_status.t) =
  match s.configured with
  | Some { source = "no key"; _ } | None -> "no key"
  | Some c -> "key: " ^ c.source
;;

let is_custom (auth : Auth_status.t list) provider =
  String.equal provider "custom"
  || List.exists auth ~f:(fun s ->
    String.equal s.provider provider && Option.is_some s.custom)
;;

let add_custom_item =
  Picker.Item.create
    ~id:"custom api_key"
    ~detail:
      "add an OpenAI-compatible endpoint (aiproxy, LiteLLM, OpenRouter, vLLM, \
       Ollama…)"
    ~search:"custom openai compatible endpoint"
    "Custom provider"
;;

let login_picker (statuses : Auth_status.t list) ~forges =
  let items =
    List.concat_map statuses ~f:(fun s ->
      List.map s.methods ~f:(fun meth ->
        match s.custom with
        | Some c ->
          Picker.Item.create
            ~id:(s.provider ^ " " ^ meth.method_)
            ~detail:(custom_detail c ^ ", edit")
            ~search:(String.concat ~sep:" " [ s.name; "custom"; c.base_url ])
            s.name
        | None ->
          let configured =
            match s.configured with
            | Some c when String.equal c.method_ meth.method_ ->
              sprintf ", logged in via %s" c.source
            | _ -> ""
          in
          Picker.Item.create
            ~id:(s.provider ^ " " ^ meth.method_)
            ~detail:(meth.label ^ configured)
            ~search:
              (String.concat
                 ~sep:" "
                 [ s.name; s.provider; meth.method_; meth.label ])
            ~marked:(not (String.is_empty configured))
            s.name))
    @ [ add_custom_item ]
    @ Forges.login_items forges
  in
  Dialog.Picker { kind = Login; picker = Picker.create ~title:"Log in" items }
;;

let logout_picker (statuses : Auth_status.t list) ~forges =
  List.filter_map statuses ~f:(fun s ->
    Option.map s.configured ~f:(fun c ->
      Picker.Item.create
        ~id:s.provider
        ~detail:
          (match s.custom with
           | Some custom -> custom_detail custom ^ ", " ^ key_source s
           | None -> sprintf "%s via %s" c.method_ c.source)
        ~search:(s.name ^ " " ^ s.provider)
        s.name))
  @ Forges.logout_items forges
  |> function
  | [] -> None
  | items ->
    Some
      (Dialog.Picker
         { kind = Logout; picker = Picker.create ~title:"Log out" items })
;;

let open_dialog (m : Model.t) ?(focus = "dialog") dialog =
  ( { m with
      dialog = Some dialog
    ; completion = None
    ; return_to_settings = false
    }
  , [ Command.Focus focus ] )
;;

let open_picker m dialog = open_dialog m ~focus:"picker-input" dialog

(* A dialog that needs a list from the backend shows at once, saying that it
   is loading; the reply replaces it (see [loads]). *)
let loading (m : Model.t) title cmds =
  let m, focus = open_dialog m (Loading title) in
  m, focus @ cmds
;;

let superuser (m : Model.t) = Option.exists m.hello ~f:(fun h -> h.superuser)

let fetch_nix_status (m : Model.t) ~open_settings =
  ( { m with nix_gc = Nix_gc.loading m.nix_gc }
  , [ call Nix_store_status ~tag:(Nix_status { open_settings }) ] )
;;

(* Superusers see the Nix store there, as it is now. *)
let open_settings ?(fetch = true) (m : Model.t) =
  let m, cmds = open_dialog m Settings in
  if fetch && superuser m
  then (
    let m, fetch = fetch_nix_status m ~open_settings:false in
    m, cmds @ fetch)
  else m, cmds
;;

let nix_gc_still_running m =
  error
    m
    "The Nix garbage collection you started is still running: Settings shows \
     how far it got."
;;

let start_nix_gc (m : Model.t) generations =
  if Option.is_some m.nix_gc.collecting
  then nix_gc_still_running m
  else (
    let m, cmds =
      toast
        { m with nix_gc = Nix_gc.start m.nix_gc }
        (String.capitalize (Prigh_ui.Nix_gc_command.starting generations))
    in
    ( m
    , cmds
      @ [ call
            (Nix_store_gc { delete_generations = generations })
            ~tag:Nix_collected
        ] ))
;;

(* [/nix-gc]: Settings, with the store. *)
let nix_gc_command (m : Model.t) rest =
  match Prigh_ui.Nix_gc_command.parse rest with
  | Status -> fetch_nix_status m ~open_settings:true
  | Error e -> error m (String.capitalize e)
  | Collect _ when Option.is_some m.nix_gc.collecting -> nix_gc_still_running m
  | Collect None -> start_nix_gc m None
  | Collect (Some generations) -> open_dialog m (Nix_gc_confirm generations)
;;

let start_login (m : Model.t) ~provider ~method_ =
  let m, cmds = open_dialog m (Login (Login_flow.start provider)) in
  ( m
  , cmds
    @ [ rpc
          "login"
          (("provider", str provider)
           :: Option.value_map method_ ~default:[] ~f:(fun m ->
             [ "method", str m ]))
          ~tag:Login_started
      ] )
;;

(* A custom provider's logout asks what to remove; the others just happen. *)
let logout (m : Model.t) provider =
  if is_custom m.auth provider
  then (
    let m, cmds =
      open_dialog m (Login (Login_flow.start ~purpose:Logout provider))
    in
    m, cmds @ [ rpc "logout" [ "provider", str provider ] ~tag:Login_started ])
  else m, [ rpc "logout" [ "provider", str provider ] ]
;;

(* ---- git hosts ---------------------------------------------------------- *)

let forge_login (m : Model.t) ~host ~kind ~method_ ~again =
  let m, cmds = open_dialog m (Login (Login_flow.start ~forge:again host)) in
  m, cmds @ [ call (Forge_login { host; kind; method_ }) ~tag:Login_started ]
;;

let forge_account_login (m : Model.t) host =
  match Forges.find m.forges host with
  | Some a ->
    forge_login
      m
      ~host:a.host
      ~kind:(Some a.kind)
      ~method_:None
      ~again:(Forge_account.login_arg a)
  | None -> forge_login m ~host ~kind:None ~method_:None ~again:host
;;

let forge_host_dialog (m : Model.t) =
  open_dialog m ~focus:"dialog-input" (Forge_host { input = ""; error = None })
;;

let signing_out host = sprintf "Signing out of %s…" host

let forge_logout (m : Model.t) host =
  let m =
    { m with
      forges = { m.forges with signing_out = Set.add m.forges.signing_out host }
    }
  in
  let m, cmds = toast m (signing_out host) in
  m, cmds @ [ call (Forge_logout host) ~tag:(Forge (Logout host)) ]
;;

(* ---- pull requests ------------------------------------------------------ *)

let list_prs (m : Model.t) (l : Pull_requests.t) =
  let m, cmds = open_picker m (Pulls l) in
  ( m
  , cmds
    @ [ call
          (Pr_list { state = Some (Pull_requests.Filter.to_string l.filter) })
          ~tag:(Forge (List { session = session_id m; filter = l.filter }))
      ] )
;;

let open_pr ?back (m : Model.t) number =
  let m, cmds = open_dialog m (Pull (Pr_page.create ?back number)) in
  ( m
  , cmds
    @ [ call
          (Pr_view number)
          ~tag:(Forge (View { session = session_id m; number }))
      ] )
;;

let new_pr ?back ?dir (m : Model.t) ~title =
  let m, cmds =
    open_dialog m (Pr_create (Pr_create.create ?back ?dir ~title ()))
  in
  ( m
  , cmds @ [ call (Pr_defaults { dir }) ~tag:(Forge (Defaults (session_id m))) ]
  )
;;

let checkout_pr (m : Model.t) number =
  let m, cmds =
    match m.dialog with
    | Some (Pull p) when p.number = number ->
      { m with dialog = Some (Pull { p with checkout = Busy }) }, []
    | _ -> toast m (sprintf "Checking out pull request #%d…" number)
  in
  ( m
  , cmds
    @ [ call
          (Pr_checkout number)
          ~tag:(Forge (Checkout { session = session_id m; number }))
      ] )
;;

let pr_command (m : Model.t) rest =
  match Pull_requests.Command.parse rest with
  | Error usage -> error m usage
  | Ok (List filter) -> list_prs m (Pull_requests.create filter)
  | Ok (View number) -> open_pr m number
  | Ok (New title) -> new_pr m ~title
  | Ok (Checkout number) -> checkout_pr m number
;;

let no_thinking (m : Model.t) (state : State.t) =
  error
    m
    (sprintf
       "%s has no thinking levels: switch to a model that thinks with /model"
       state.model.name)
;;

let open_thinking_picker (m : Model.t) =
  match m.state with
  | Some state when not state.model.supports_thinking -> no_thinking m state
  | _ ->
    open_picker
      m
      (Dialog.Picker { kind = Thinking; picker = thinking_levels_picker m })
;;

let set_thinking (m : Model.t) level =
  match m.state with
  | Some state when not state.model.supports_thinking -> no_thinking m state
  | state ->
    let unchanged =
      Option.exists state ~f:(fun s -> String.equal s.thinking level)
    in
    ( (if unchanged
       then m
       else { m with pending = { m.pending with thinking = Some level } })
    , [ rpc "set_thinking" [ "thinking", str level ] ~tag:Thinking_set ] )
;;

let set_model (m : Model.t) key =
  let set = rpc "set_model" [ "model", str key ] ~tag:Model_set in
  let m =
    match m.state with
    | Some state when String.equal state.model.key key -> m
    | _ -> { m with pending = { m.pending with model = Some key } }
  in
  match List.find m.models ~f:(fun model -> String.equal model.key key) with
  | Some model
    when (not (List.is_empty m.auth)) && not (logged_in m model.provider) ->
    let m, cmds =
      toast
        m
        (sprintf
           "%s needs %s, which is not logged in: /login %s logs in."
           model.name
           model.provider
           model.provider)
    in
    m, set :: cmds
  | _ -> m, [ set ]
;;

let set_draft (m : Model.t) ?cursor draft =
  { m with draft; cursor = Option.value cursor ~default:(String.length draft) }
;;

let complete_draft (m : Model.t) ~skills =
  match
    Completion.compute
      ~skills
      ~snippets:(Option.value_map m.config ~default:[] ~f:(fun c -> c.snippets))
      ~reminders:
        (Option.value_map m.state ~default:[] ~f:(fun s -> s.reminders))
      ~worktrees:
        (Option.value_map
           (Worktrees.listing m.worktrees)
           ~default:[]
           ~f:(fun (l : Worktree.Listing.t) -> l.worktrees))
      ?refs:(Option.map m.review.diff ~f:Git_diff.refs)
      ~sessions:m.sessions
      ~forges:(Forges.list m.forges)
      ~hosts:(Option.value_map m.state ~default:[] ~f:(fun s -> s.hosts))
      ~users:(Option.value m.users ~default:[])
      ~superuser:(superuser m)
      ~text:m.draft
      ~cursor:m.cursor
      ~models:m.models
      ~auth:m.auth
      ~current_model:(Option.map m.state ~f:(fun s -> s.model.key))
      ()
  with
  | None -> { m with completion = None }, []
  | Some c ->
    (match m.completion with
     | Some old when Completion.same old c -> m, []
     | _ ->
       ( { m with completion = Some c }
       , (match Completion.request c with
          | None -> []
          | Some (method_, params) ->
            [ rpc method_ params ~tag:(Paths (Completion.prefix c)) ]) ))
;;

(* [/skill:] completes the skills listed where the tools run, fetched the
   first time it is typed there. *)
let fetch_skills (m : Model.t) =
  match Skills.place m.state with
  | Some place
    when String.is_prefix m.draft ~prefix:"/skill:"
         && Skills.unknown m.skills ~place ->
    ( { m with skills = Skills.requested ~place }
    , [ call List_skills ~tag:(Skills { place; purpose = Complete }) ] )
  | _ -> m, []
;;

let refresh_completion (m : Model.t) =
  let m, fetch = fetch_skills m in
  let skills =
    Option.bind (Skills.place m.state) ~f:(fun place ->
      Skills.find m.skills ~place)
  in
  let m, cmds = complete_draft m ~skills:(Option.value skills ~default:[]) in
  m, fetch @ cmds
;;

let edit m ?cursor text = refresh_completion (set_draft m ?cursor text)

(* A programmatic edit: the browser would put the caret at the end. *)
let edit_at_caret m ~cursor text =
  let m, cmds = edit m ~cursor text in
  m, cmds @ [ Command.Set_caret cursor ]
;;

let open_sessions (m : Model.t) =
  { m with sidebar_open = true }, [ Command.Focus "session-search" ]
;;

let switch_session (m : Model.t) path =
  let m = { m with sidebar_open = m.sidebar_open && not m.narrow } in
  let current = Option.map m.state ~f:(fun s -> s.session_path) in
  if Option.equal String.equal current (Some path)
  then m, [ focus_editor ]
  else (
    let m, fetch = fetch_newest m path in
    ( { m with pending = { m.pending with session = Some (Switching path) } }
    , [ rpc "switch_session" [ "path", str path ] ~tag:Reload_state ]
      @ fetch
      @ [ focus_editor ] ))
;;

(* The sidebar's sessions in the order it shows them. *)
let shown_sessions (m : Model.t) =
  Session_list.shown
    m.sessions
    ~query:m.session_query
    ~archived_open:m.archived_open
;;

(* What is known of the repositories' worktrees: the session's own
   repository's, then the others'. *)
let worktree_listings (m : Model.t) =
  Option.to_list (Worktrees.listing m.worktrees)
  @ Repo_listings.listings m.repo_worktrees
;;

let session_order (m : Model.t) =
  List.map (shown_sessions m) ~f:(fun (s : Session_summary.t) -> s.path)
;;

let find_session_by_path (m : Model.t) path =
  List.find m.sessions ~f:(fun s -> String.equal s.path path)
;;

(* A path, an id, a unique id prefix or a name, as /archive and /delete take. *)
let find_session (m : Model.t) key =
  let key = String.strip key in
  let exactly =
    List.filter m.sessions ~f:(fun (s : Session_summary.t) ->
      String.equal s.path key
      || String.equal s.id key
      || Option.exists s.name ~f:(String.equal key))
  in
  let candidates =
    match exactly with
    | [] ->
      List.filter m.sessions ~f:(fun (s : Session_summary.t) ->
        String.is_prefix s.id ~prefix:key)
    | exactly -> exactly
  in
  match candidates with
  | [ s ] -> Ok s
  | [] ->
    Error
      (sprintf
         "No session “%s”: give its name, id or path (Ctrl+K searches them)."
         key)
  | several ->
    Error
      (sprintf
         "“%s” matches %d sessions (%s): give more of the id."
         key
         (List.length several)
         (String.concat
            ~sep:", "
            (List.map (List.take several 3) ~f:(fun (s : Session_summary.t) ->
               String.prefix s.id 8))))
;;

let set_archived (m : Model.t) (s : Session_summary.t) ~archived =
  if archived && s.running
  then
    error
      m
      (sprintf
         "“%s” is running: wait for it or stop it (Esc in it) before archiving."
         (Session_list.title s))
  else
    ( { m with
        sessions =
          List.map m.sessions ~f:(fun (x : Session_summary.t) ->
            if String.equal x.path s.path then { x with archived } else x)
      }
    , [ call
          (Set_archived { path = s.path; archived })
          ~tag:(Archived_set { title = Session_list.title s; archived })
      ] )
;;

let ask_delete (m : Model.t) paths =
  match paths with
  | [] -> toast m "Select the sessions to delete first (click them)."
  | paths -> open_dialog m (Delete (Sessions paths))
;;

let ask_delete_archived (m : Model.t) =
  if List.exists m.sessions ~f:(fun s -> s.archived)
  then open_dialog m (Delete Archived)
  else toast m "There are no archived sessions."
;;

(* A new session (or a clone, a fork) is on its way: the chat says so. *)
let starting (m : Model.t) =
  { m with pending = { m.pending with session = Some Starting } }
;;

(* The keyboard through the sidebar's (filtered) sessions: down from the
   search to the first, up from the first back to the search. *)
let session_nav (m : Model.t) ~from ~by =
  let ids = List.map (shown_sessions m) ~f:(fun s -> s.id) in
  let index =
    match from with
    | None -> -1
    | Some id ->
      Option.value_map
        (List.findi ids ~f:(fun _ id' -> String.equal id id'))
        ~default:(-1)
        ~f:fst
  in
  let target = index + by in
  if target < 0
  then m, [ Command.Focus "session-search" ]
  else (
    match List.nth ids (Int.min target (List.length ids - 1)) with
    | Some id -> m, [ Command.Focus (Session_list.dom_id id) ]
    | None -> m, [])
;;

let open_rename (m : Model.t) =
  let name =
    Option.value_map m.state ~default:"" ~f:(fun s ->
      Option.value s.session_name ~default:"")
  in
  open_dialog m ~focus:"dialog-input" (Rename name)
;;

(* ---- the user's preferences (Pref_sync) *)

module Key = Pref_sync.Key

(* Acting as another user, theirs are not ours: ours stay, in this browser
   only, until we are back. *)
let acting_as (m : Model.t) = Option.bind m.hello ~f:Hello_reply.acting_as

let saves_prefs (m : Model.t) =
  Pref_sync.saving m.prefs && Option.is_none (acting_as m)
;;

(* Kept in the browser, and on the backend unless it keeps none for us. *)
let save_prefs (m : Model.t) (changes : Prefs.Change.t list) =
  if List.is_empty changes
  then []
  else
    Command.Cache_prefs changes
    ::
    (if saves_prefs m
     then [ rpc "set_prefs" (Prefs.set_params changes) ]
     else [])
;;

let save_pref m key value = save_prefs m [ { Prefs.Change.key; value } ]

let fetch_prefs (m : Model.t) =
  match acting_as m with
  | Some _ -> []
  | None -> [ rpc "get_prefs" (Prefs.get_params ~keys:Key.all) ~tag:Prefs ]
;;

(* A preference from the browser's cache, the backend or another of the
   user's frontends. Whether the terminal is open follows only at [startup]:
   another browser opening its own is no reason to open ours. *)
let apply_pref (m : Model.t) ~startup ({ key; value } : Prefs.Change.t) =
  let string =
    match value with
    | Some (`String s) -> Some s
    | _ -> None
  in
  if String.equal key Key.theme
  then
    { m with
      theme =
        { m.theme with
          name =
            Option.value
              (Option.bind string ~f:Theme.Name.of_string)
              ~default:Theme.default.name
        }
    }
  else if String.equal key Key.accent
  then
    { m with
      theme =
        { m.theme with
          accent =
            Option.value
              (Option.bind string ~f:Theme.Accent.of_string)
              ~default:Theme.default.accent
        }
    }
  else if String.equal key Key.verbosity
  then (
    let verbosity =
      Option.value
        (List.find Prigh_ui.Verbosity.all ~f:(fun v ->
           Option.equal String.equal string (Some (Prigh_ui.Verbosity.name v))))
        ~default:Normal
    in
    if Prigh_ui.Verbosity.equal verbosity m.verbosity
    then m
    else { m with verbosity; folds = String.Set.empty })
  else if String.equal key Key.notify
  then (
    let enabled =
      match value with
      | Some `False -> false
      | _ -> true
    in
    { m with notifications = { m.notifications with enabled } })
  else if String.equal key Key.history
  then
    if History.browsing m.history
    then m
    else (
      let entries =
        match value with
        | Some (`Array items) ->
          List.filter_map items ~f:(function
            | `String s -> Some s
            | _ -> None)
        | _ -> []
      in
      { m with history = History.of_list entries })
  else if String.equal key Key.folded
  then (
    let folded =
      match value with
      | Some (`Array items) ->
        String.Set.of_list
          (List.filter_map items ~f:(function
             | `String s -> Some s
             | _ -> None))
      | _ -> String.Set.empty
    in
    { m with folded })
  else if String.equal key Key.review
  then { m with review_memory = Review_memory.of_json value }
  else if String.equal key Key.terminal_open
  then
    if startup
    then (
      let open_ = Option.exists value ~f:(Json.equal `True) in
      { m with terminal = { m.terminal with open_ } })
    else m
  else (
    match
      List.find Resize.Handle.all ~f:(fun h ->
        String.equal (Resize.Handle.pref_key h) key)
    with
    | Some h ->
      let size =
        match value with
        | Some (`Number n) -> Int.of_string_opt n
        | _ -> None
      in
      let layout, _ =
        Resize.update m.layout (Model.shown m) (Restore [ h, size ])
      in
      { m with layout }
    | None -> m)
;;

let apply_prefs m ~startup changes =
  List.fold changes ~init:m ~f:(apply_pref ~startup)
;;

let prefs_loaded (m : Model.t) changes =
  let startup = Pref_sync.Remote.equal m.prefs.remote Unknown in
  let prefs, applied, upload = Pref_sync.loaded m.prefs changes in
  let m = apply_prefs { m with prefs } ~startup applied in
  (* The page may show another account's, from the browser's cache. *)
  ( m
  , Command.Apply_theme (Model.look m)
    :: Command.Cache_prefs applied
    ::
    (if List.is_empty upload
     then []
     else [ rpc "set_prefs" (Prefs.set_params upload) ]) )
;;

let theme_changes (theme : Theme.t) =
  [ { Prefs.Change.key = Key.theme
    ; value = Some (`String (Theme.Name.to_string theme.name))
    }
  ; { key = Key.accent
    ; value = Some (`String (Theme.Accent.to_string theme.accent))
    }
  ]
;;

let set_theme (m : Model.t) (theme : Theme.t) ~replaces ~notice =
  let m = { m with theme; theme_preview = None } in
  let m, cmds = toast m ~replaces notice in
  m, save_prefs m (theme_changes theme) @ cmds
;;

let theme_picker (m : Model.t) =
  Dialog.Picker
    { kind = Themes
    ; picker =
        Picker.create
          ~title:"Theme"
          (List.map Theme.Name.all ~f:(fun n ->
             Picker.Item.create
               ~id:(Theme.Name.to_string n)
               ~detail:(Theme.Name.describe n)
               ~marked:(Theme.Name.equal n m.theme.name)
               (String.capitalize (Theme.Name.to_string n))))
    }
;;

let accent_picker (m : Model.t) =
  let custom =
    match m.theme.accent with
    | Custom _ as accent -> [ accent ]
    | Preset _ -> []
  in
  Dialog.Picker
    { kind = Accents
    ; picker =
        Picker.create
          ~title:"Accent colour (or type #rrggbb)"
          (List.map (Theme.Accent.all_presets @ custom) ~f:(fun a ->
             Picker.Item.create
               ~id:(Theme.Accent.to_string a)
               ~marked:(Theme.Accent.equal a m.theme.accent)
               (match a with
                | Preset p ->
                  String.capitalize (Theme.Accent.Preset.to_string p)
                | Custom hex -> "Custom " ^ hex)))
    }
;;

let theme_named (m : Model.t) name =
  match Theme.Name.of_string name with
  | Some n ->
    set_theme
      m
      { m.theme with name = n }
      ~replaces:[ "Theme: " ]
      ~notice:
        (sprintf
           "Theme: %s, %s"
           (Theme.Name.to_string n)
           (Theme.Name.describe n))
  | None ->
    error
      m
      (sprintf
         "Unknown theme “%s”: use original, dark or light (/theme picks one)."
         name)
;;

let accent_named (m : Model.t) name =
  match Theme.Accent.of_string name with
  | Some accent ->
    set_theme
      m
      { m.theme with accent }
      ~replaces:[ "Accent: " ]
      ~notice:(sprintf "Accent: %s" (Theme.Accent.to_string accent))
  | None ->
    error
      m
      (sprintf
         "Unknown accent “%s”: use orange, blue, green, purple, pink, teal, \
          yellow or a colour like #3b82f6 (/accent picks one)."
         name)
;;

(* ---- the terminal panel *)

let with_terminal (m : Model.t) ~f = { m with terminal = f m.terminal }

(* The shell takes the keyboard (Ctrl+` gives it back); on a phone the panel
   covers the page, the sidebar's drawer included. *)
(* [/terminal DIR]: a shell of its own there, for this session; [None]:
   the session's own again. *)
let with_terminal_dir (m : Model.t) dir =
  match m.state with
  | None -> m
  | Some state -> with_terminal m ~f:(fun t -> Terminal.set_dir t state dir)
;;

let open_terminal (m : Model.t) =
  ( { m with
      terminal = { m.terminal with open_ = true }
    ; sidebar_open = m.sidebar_open && not m.narrow
    }
  , (if m.terminal.open_ then [] else save_pref m Key.terminal_open (Some `True))
    @ [ Command.Focus_terminal ] )
;;

let close_terminal (m : Model.t) =
  ( with_terminal m ~f:(fun t -> { t with open_ = false })
  , save_pref m Key.terminal_open None @ [ focus_editor ] )
;;

let new_shell (m : Model.t) =
  ( with_terminal m ~f:(fun t ->
      { t with open_ = true; generation = t.generation + 1 })
  , [ Command.Focus_terminal ] )
;;

(* ---- the agents panel *)

let now_of (m : Model.t) = Option.value m.now ~default:Time_ns.epoch
let with_agents (m : Model.t) ~f = { m with agents = f m.agents }
let job_output id = rpc "job_output" [ "job_id", str id ] ~tag:(Job_output id)

(* Subagents' transcripts come with their calls (events, or [get_subagent]
   after a reload); those of calls not loaded, and nested ones, are fetched
   when shown. *)
let fetch_transcript (m : Model.t) id =
  let wanted =
    Agents.lineage m.agents id
    |> List.filter ~f:(fun (a : Agents.Agent.t) ->
      Option.is_none (Chat.find_subagent m.chat a.id)
      &&
      match Map.find m.agents.fetches a.id with
      | Some (Asked _) -> false
      | Some (Failed _) | None -> true)
  in
  ( with_agents m ~f:(fun agents ->
      { agents with
        fetches =
          List.fold wanted ~init:agents.fetches ~f:(fun fetches a ->
            Map.set fetches ~key:a.id ~data:(Asked m.now))
      })
  , List.map wanted ~f:(fun (a : Agents.Agent.t) -> subagent_rpc a.id) )
;;

(* After a new chat: the transcript on show, if it went with the old one. *)
let refetch_shown (m : Model.t) =
  match m.agents.selected with
  | Some (Agent id) -> fetch_transcript m id
  | Some (Job _) | None -> m, []
;;

let select (m : Model.t) (item : Agents.Item.t) =
  let m =
    with_agents m ~f:(fun a ->
      { a with
        open_ = true
      ; selected = Some item
      ; output =
          (match item, a.output with
           | Job id, Some (shown, _) when String.equal id shown -> a.output
           | _ -> None)
      })
  in
  match item with
  | Agent id -> fetch_transcript m id
  | Job id -> m, [ job_output id ]
;;

let with_item (m : Model.t) arg ~f =
  match Agents.resolve m.agents arg with
  | Some item -> f m item
  | None ->
    error
      m
      (match Agents.listed m.agents with
       | [] ->
         sprintf "No subagent or job %s: none has run in this session." arg
       | listed ->
         sprintf
           "No subagent or job %s: give its number (1-%d) or id; /agents lists \
            them."
           arg
           (List.length listed))
;;

(* On a phone the panel covers the page, so it takes the focus from the
   editor behind it. *)
let open_agents (m : Model.t) arg =
  let m, commands =
    match arg with
    | None ->
      ( with_agents m ~f:(fun a -> { a with open_ = true; selected = None })
      , [ list_subagents; list_jobs ] )
    | Some arg -> with_item m arg ~f:select
  in
  ( m
  , if m.narrow && m.agents.open_
    then commands @ [ Command.Focus "agents-panel" ]
    else commands )
;;

let close_agents (m : Model.t) =
  ( with_agents m ~f:(fun a -> { a with open_ = false })
  , if m.narrow then [] else [ focus_editor ] )
;;

let agents_back (m : Model.t) =
  match m.agents.selected with
  | Some _ -> with_agents m ~f:(fun a -> { a with selected = None }), []
  | None -> close_agents m
;;

(* ---- steering a subagent *)

let set_recipient (m : Model.t) (recipient : Recipient.t) =
  match recipient with
  | Main -> { m with recipient }, [ focus_editor ]
  | Agent id ->
    (match Recipient.refusal m.agents id with
     | Some why -> error m why
     | None ->
       (* On a phone the panel covers the composer. *)
       let m =
         if m.narrow
         then with_agents m ~f:(fun a -> { a with open_ = false })
         else m
       in
       { m with recipient }, [ focus_editor ])
;;

(* [steer_agent]: [draft] (what was typed) goes back into the editor if it
   fails. *)
let message_agent (m : Model.t) ~agent ~text ~follow_up ~draft =
  ( { m with pending = In_flight.sent m.pending None }
  , [ rpc
        "steer_agent"
        ([ "agent_id", str agent; "text", str text ]
         @ if follow_up then [ "mode", str "follow_up" ] else [])
        ~tag:(Steered { agent; draft })
    ] )
;;

let without_queued (m : Model.t) agent (q : Queued.Message.t) =
  { m with
    agent_queues =
      List.filter_map m.agent_queues ~f:(fun (a : Agent_queue.t) ->
        if not (String.equal a.agent_id agent)
        then Some a
        else (
          let drop kind texts =
            if Queued.Message.Kind.equal q.kind kind
            then List.filteri texts ~f:(fun i _ -> i <> q.index)
            else texts
          in
          match drop Steer a.steer, drop Follow_up a.follow_up with
          | [], [] -> None
          | steer, follow_up -> Some { a with steer; follow_up }))
  }
;;

(* Taken out of the queue at once, before the backend confirms. *)
let unqueue (m : Model.t) (recipient : Recipient.t) q ~tag =
  let params = Queued.Message.remove_params q in
  match recipient with
  | Main ->
    ( { m with queue = Queued.remove m.queue q }
    , [ rpc "remove_queued" params ~tag ] )
  | Agent agent ->
    ( without_queued m agent q
    , [ rpc "remove_agent_queued" (("agent_id", str agent) :: params) ~tag ] )
;;

(* A finished subagent takes no messages: the composer goes back to the main
   agent. Showing a subagent in the panel does not steer it: only its Steer
   button does. *)
let follow_recipient ((m : Model.t), cmds) =
  match m.recipient with
  | Main -> m, cmds
  | Agent id ->
    (match Agents.find_agent m.agents id with
     | None -> { m with recipient = Main }, cmds
     | Some { status = Agents.Status.Running; _ } -> m, cmds
     | Some { status = Complete | Failed; _ } ->
       let m, toast_cmds =
         toast
           { m with recipient = Main }
           (sprintf
              "%s has finished: you are writing to the main agent again"
              id)
       in
       m, cmds @ toast_cmds)
;;

let show_in_chat (m : Model.t) id =
  let path =
    List.map (Agents.lineage m.agents id) ~f:(fun (a : Agents.Agent.t) ->
      a.call_id)
  in
  let m =
    if m.narrow then with_agents m ~f:(fun a -> { a with open_ = false }) else m
  in
  m, [ Command.Reveal path ]
;;

(* While the panel is open, running jobs' last lines and the shown job's
   output are polled. *)
let poll_interval = Time_ns.Span.of_sec 2.
let transcript_timeout = Time_ns.Span.of_sec 20.

let clock (m : Model.t) now =
  let m =
    { m with
      now = Some now
    ; agents = Agents.time_out_fetches m.agents ~now ~after:transcript_timeout
    }
  in
  let a = m.agents in
  let due =
    Option.value_map a.polled_at ~default:true ~f:(fun at ->
      Time_ns.Span.( >= ) (Time_ns.diff now at) poll_interval)
  in
  if a.open_ && due && Agents.running_jobs a > 0
  then
    ( with_agents m ~f:(fun a -> { a with polled_at = Some now })
    , list_jobs
      ::
      (match a.selected with
       | Some (Job id) when Agents.running a (Job id) -> [ job_output id ]
       | _ -> []) )
  else m, []
;;

let stop (m : Model.t) (item : Agents.Item.t) =
  let m =
    with_agents m ~f:(fun a -> { a with stopping = Set.add a.stopping item })
  in
  match item with
  | Agent id -> m, [ rpc "cancel_subagent" [ "agent_id", str id ] ]
  | Job id -> m, [ rpc "kill_job" [ "job_id", str id ] ]
;;

(* [/agents cancel] and [/jobs kill]: open the item in the panel, which
   shows it stopping. *)
let stop_command (m : Model.t) item =
  if Agents.running m.agents item
  then (
    let m, select_cmds = select m item in
    let m, stop_cmds = stop m item in
    m, select_cmds @ stop_cmds)
  else (
    let m, cmds = select m item in
    let m, toast_cmds =
      error
        m
        (match item with
         | Agent id -> sprintf "Subagent %s has already finished." id
         | Job id -> sprintf "Job %s has already finished." id)
    in
    m, cmds @ toast_cmds)
;;

let no_job (m : Model.t) arg =
  error
    m
    (match m.agents.jobs with
     | [] ->
       sprintf
         "No job %s: none has run in this session (!&command starts one)."
         arg
     | jobs ->
       sprintf
         "No job %s: give its id (%s); /jobs lists them."
         arg
         (String.concat
            ~sep:", "
            (List.map jobs ~f:(fun (j : Agents.Job.t) -> j.info.id))))
;;

(* [/jobs id] and [/jobs kill id]: a job's id, or its number in the panel. *)
let with_job (m : Model.t) arg ~f =
  match Agents.resolve m.agents arg with
  | Some (Job _ as item) -> f m item
  | Some (Agent _) | None -> no_job m arg
;;

(* ---- parity with the TUI's commands ------------------------------------ *)

let current_state (m : Model.t) ~f =
  match m.state with
  | Some state -> f state
  | None -> error m "Not connected yet: wait for the backend."
;;

let config (m : Model.t) = Option.value m.config ~default:Config.default

(* Only [fields] are sent: the backend keeps the others, which another client
   may have changed meanwhile. *)
let set_config config ~fields ~tag =
  let changed =
    match Config.to_json config with
    | `Object all ->
      `Object
        (List.filter all ~f:(fun (name, _) ->
           List.mem fields name ~equal:String.equal))
    | json -> json
  in
  rpc "set_config" [ "config", changed ] ~tag
;;

let save_config (m : Model.t) config ~fields ~notice =
  ( { m with config = Some config }
  , [ set_config config ~fields ~tag:(Config_saved notice) ] )
;;

let with_config (m : Model.t) ~f =
  match m.config with
  | Some config -> f config
  | None -> error m "Not connected yet: wait for the backend."
;;

(* The models Ctrl+P cycles through: the scoped ones, else the logged-in
   ones, else all. *)
let scope (m : Model.t) =
  match m.config with
  | Some { scoped_models = _ :: _ as keys; _ } ->
    List.filter m.models ~f:(fun model ->
      List.mem keys model.key ~equal:String.equal)
  | _ ->
    (match
       List.filter m.models ~f:(fun model -> logged_in m model.provider)
     with
     | [] -> m.models
     | logged -> logged)
;;

let cycle_model (m : Model.t) step =
  current_state m ~f:(fun state ->
    match scope m with
    | [] -> error m "No models to cycle through: /login logs in to a provider."
    | [ only ] ->
      toast
        m
        (sprintf
           "%s is the only model in scope: /scoped-models picks more."
           only.name)
    | models ->
      let n = List.length models in
      let index =
        List.findi models ~f:(fun _ (model : Llm.t) ->
          String.equal model.key state.model.key)
        |> Option.value_map ~default:(if step > 0 then -1 else 0) ~f:fst
      in
      let next = List.nth_exn models ((((index + step) % n) + n) % n) in
      let m = { m with state = Some { state with model = next } } in
      let m, cmds =
        toast m ~replaces:[ "Model: " ] (sprintf "Model: %s" next.name)
      in
      ( { m with pending = { m.pending with model = Some next.key } }
      , cmds @ [ rpc "set_model" [ "model", str next.key ] ~tag:Model_set ] ))
;;

let cycle_thinking (m : Model.t) =
  current_state m ~f:(fun state ->
    if not state.model.supports_thinking
    then
      error
        m
        (sprintf
           "%s has no thinking levels: switch to a model that thinks with \
            /model"
           state.model.name)
    else (
      let index =
        List.findi thinking_levels ~f:(fun _ l -> String.equal l state.thinking)
        |> Option.value_map ~default:0 ~f:fst
      in
      let next =
        List.nth_exn thinking_levels ((index + 1) % List.length thinking_levels)
      in
      let m = { m with state = Some { state with thinking = next } } in
      let m, cmds =
        toast m ~replaces:[ "Thinking: " ] (sprintf "Thinking: %s" next)
      in
      ( { m with pending = { m.pending with thinking = Some next } }
      , cmds @ [ rpc "set_thinking" [ "thinking", str next ] ~tag:Thinking_set ]
      )))
;;

let verbosity_detail : Prigh_ui.Verbosity.t -> string = function
  | Quiet -> "tool calls without their output; no thinking"
  | Normal -> "tool output and thinking folded"
  | Verbose -> "everything unfolded"
;;

let set_verbosity (m : Model.t) verbosity =
  let m, cmds =
    toast
      { m with verbosity; folds = String.Set.empty }
      ~replaces:[ "Transcript: " ]
      (sprintf
         "Transcript: %s, %s (Ctrl+O cycles)"
         (Prigh_ui.Verbosity.name verbosity)
         (verbosity_detail verbosity))
  in
  ( m
  , save_pref
      m
      Key.verbosity
      (Some (`String (Prigh_ui.Verbosity.name verbosity)))
    @ cmds )
;;

(* ---- the transcript filter (Filter_menu) *)

(* Kept in the session, so that each has its own. *)
let save_filter (m : Model.t) =
  match m.state with
  | None -> m, []
  | Some state ->
    ( { m with pending = { m.pending with filters = m.pending.filters + 1 } }
    , [ rpc
          "set_transcript_filter"
          [ ( "filter"
            , if Transcript_filter.is_all m.transcript_filter
              then `Null
              else str (Transcript_filter.to_string m.transcript_filter) )
          ]
          ~tag:(Filter_saved state.session_id)
      ] )
;;

(* [/filter ARGS]: acknowledged by a toast, as the transcript may not show
   the change where the user looks. *)
let set_filter (m : Model.t) filter =
  let m = { m with transcript_filter = filter } in
  let m, cmds =
    toast
      m
      ~replaces:[ "Transcript filter: " ]
      (match Transcript_filter.summary filter with
       | None -> "Transcript filter: showing everything"
       | Some summary ->
         "Transcript filter: " ^ summary ^ " (/filter all shows everything)")
  in
  let m, save = save_filter m in
  m, save @ cmds
;;

let open_filter (m : Model.t) =
  match m.dialog with
  | Some (Filter menu) ->
    let m, cmds = close_dialog m in
    let m, save =
      if Transcript_filter.equal m.transcript_filter menu.original
      then m, []
      else save_filter m
    in
    m, save @ cmds
  | _ ->
    let m, cmds =
      open_dialog m (Filter (Filter_menu.create m.transcript_filter))
    in
    m, cmds @ [ Command.Place_menu (Element Filter_menu.anchor) ]
;;

let filter_press (m : Model.t) row =
  match m.dialog with
  | Some (Filter menu) ->
    let row = Option.value row ~default:(Filter_menu.selected menu) in
    ( { m with
        dialog = Some (Filter (Filter_menu.select menu row))
      ; transcript_filter = Filter_menu.press row m.transcript_filter
      }
    , [] )
  | _ -> m, []
;;

let verbosity_picker (m : Model.t) =
  Dialog.Picker
    { kind = Verbosity
    ; picker =
        Picker.create
          ~title:"Transcript verbosity"
          (List.map Prigh_ui.Verbosity.all ~f:(fun v ->
             Picker.Item.create
               ~id:(Prigh_ui.Verbosity.name v)
               ~detail:(verbosity_detail v)
               ~marked:(Prigh_ui.Verbosity.equal v m.verbosity)
               (String.capitalize (Prigh_ui.Verbosity.name v))))
    }
;;

(* The default mode for new sessions, which the backend applies to this
   session too. *)
let set_confirm (m : Model.t) enabled =
  let approval_mode : Approval_mode.t = if enabled then Ask else Auto in
  let m =
    { m with
      state =
        Option.map m.state ~f:(fun s ->
          { s with approval_mode; plan_mode = false })
    }
  in
  save_config
    m
    { (config m) with approval_mode }
    ~fields:[ "approval_mode" ]
    ~notice:
      (if enabled
       then
         "Tool confirmation on (mode ask, also for new sessions): "
         ^ Session_mode.describe { approval = Ask; plan = false }
       else
         "Tool confirmation off (mode auto, also for new sessions): "
         ^ Session_mode.describe { approval = Auto; plan = false })
;;

let mode_text (mode : Session_mode.t) =
  sprintf "Mode: %s (%s)" (Session_mode.label mode) (Session_mode.describe mode)
;;

(* Shown at once; [set_mode]'s reply is the backend's state. *)
let set_mode (m : Model.t) (mode : Session_mode.t) =
  current_state m ~f:(fun s ->
    let m =
      { m with
        state =
          Some { s with approval_mode = mode.approval; plan_mode = mode.plan }
      ; pending = { m.pending with mode = Some mode }
      }
    in
    ( m
    , [ rpc
          "set_mode"
          (Session_mode.params mode)
          ~tag:(Mode_set (mode_text mode))
      ] ))
;;

let cycle_mode (m : Model.t) =
  current_state m ~f:(fun s ->
    set_mode m (Session_mode.cycle (Session_mode.of_state s)))
;;

let modes_picker (state : State.t) =
  let current = Session_mode.of_state state in
  Picker.create
    ~title:"When tools ask first (Alt+M cycles)"
    (List.map (Session_mode.choices ~current) ~f:(fun mode ->
       Picker.Item.create
         ~id:(Session_mode.label mode)
         ~detail:(Session_mode.describe mode)
         ~search:(Session_mode.label mode ^ " " ^ Session_mode.title mode)
         ~marked:(Session_mode.equal mode current)
         (Session_mode.title mode)))
;;

let mode_picker (m : Model.t) =
  current_state m ~f:(fun s ->
    open_picker m (Dialog.Picker { kind = Modes; picker = modes_picker s }))
;;

let chip_kind : Chip.t -> Dialog.Picker_kind.t = function
  | Model -> Models
  | Thinking -> Thinking
  | Mode -> Modes
;;

let chip_opens_nothing (state : State.t) : Chip.t -> bool = function
  | Thinking -> not state.model.supports_thinking
  | Model | Mode -> false
;;

(* A chip's menu, over it; the same chip again closes it. *)
let open_chip (m : Model.t) (chip : Chip.t) =
  match m.dialog with
  | Some (Popover { chip = shown; _ }) when Chip.equal shown chip ->
    close_dialog m
  | _ ->
    current_state m ~f:(fun state ->
      if chip_opens_nothing state chip
      then no_thinking m state
      else (
        let picker =
          match chip with
          | Model -> models_picker m
          | Thinking -> thinking_levels_picker m
          | Mode -> modes_picker state
        in
        let m, cmds =
          open_dialog
            m
            ~focus:
              (match chip with
               | Model -> "popover-filter"
               | Thinking | Mode -> "popover-list")
            (Popover { chip; picker })
        in
        m, cmds @ [ Command.Place_popover ]))
;;

(* ← → and Tab: the neighbouring chip's menu, as in a menu bar. *)
let step_chip (m : Model.t) delta =
  match m.dialog, m.state with
  | Some (Popover { chip; _ }), Some state ->
    open_chip m (Chip.step chip delta ~skip:(chip_opens_nothing state))
  | _ -> m, []
;;

let mode_by_name (m : Model.t) name =
  current_state m ~f:(fun s ->
    match Session_mode.of_name ~current:(Session_mode.of_state s) name with
    | Some mode -> set_mode m mode
    | None ->
      error
        m
        (sprintf
           "Unknown mode “%s”: use %s (/mode picks one)."
           name
           (String.concat ~sep:", " Session_mode.names)))
;;

let plan_mode (m : Model.t) arg =
  current_state m ~f:(fun s ->
    let mode = Session_mode.of_state s in
    match arg with
    | "" -> set_mode m { mode with plan = not s.plan_mode }
    | "on" -> set_mode m { mode with plan = true }
    | "off" -> set_mode m { mode with plan = false }
    | other ->
      error
        m
        (sprintf
           "Unknown argument “%s”: use /plan on, /plan off, or /plan to toggle."
           other))
;;

let show_plan (m : Model.t) =
  match Option.bind m.state ~f:(fun s -> s.plan) with
  | None ->
    toast
      m
      "No plan: the model has not made one (it makes one with update_plan)."
  | Some plan ->
    open_dialog
      m
      (Text
         { title =
             sprintf "Plan %d/%d" (Plan.completed plan) (List.length plan.steps)
         ; text = String.concat ~sep:"\n" (Plan_view.lines plan)
         })
;;

let show_permissions (m : Model.t) =
  current_state m ~f:(fun s ->
    let rules title = function
      | [] -> []
      | rules -> [ title ^ ": " ^ String.concat ~sep:", " rules ]
    in
    let config =
      match m.config with
      | None -> []
      | Some c ->
        rules "allow (config)" c.permissions.allow
        @ rules "ask (config)" c.permissions.ask
        @ rules "deny (config)" c.permissions.deny
    in
    let lines =
      [ mode_text (Session_mode.of_state s) ]
      @ (match s.session_rules with
         | [] ->
           [ "Allowed this session: nothing yet (a confirmation's \"always\" \
              adds to it)"
           ]
         | r -> rules "Allowed this session" r)
      @ config
      @ [ ""
        ; "Rules are permissions in ~/.prigh/config.json, e.g. {\"allow\": \
           [\"bash(git status:*)\"], \"deny\": [\"bash(rm:*)\"]}."
        ; "/mode or Alt+M switches the mode."
        ]
    in
    open_dialog
      m
      (Text { title = "Permissions"; text = String.concat ~sep:"\n" lines }))
;;

let confirm_picker (m : Model.t) =
  let current =
    Option.map m.config ~f:(fun c ->
      not (Prigh_protocol.Approval_mode.equal c.approval_mode Auto))
  in
  Dialog.Picker
    { kind = Confirm_tools
    ; picker =
        Picker.create
          ~title:"Tool confirmation"
          [ Picker.Item.create
              ~id:"on"
              ~detail:"ask before bash, write and edit run"
              ~marked:(Option.equal Bool.equal current (Some true))
              "On"
          ; Picker.Item.create
              ~id:"off"
              ~detail:"run tools without asking"
              ~marked:(Option.equal Bool.equal current (Some false))
              "Off"
          ]
    }
;;

let scoped_models_dialog (m : Model.t) =
  let checked =
    match m.config with
    | Some { scoped_models = _ :: _ as keys; _ } -> String.Set.of_list keys
    | _ ->
      String.Set.of_list
        (List.filter_map m.models ~f:(fun model ->
           Option.some_if (logged_in m model.provider) model.key))
  in
  Dialog.Scoped_models
    { checked
    ; picker =
        Picker.create
          ~title:"Scoped models"
          (List.map m.models ~f:(fun (model : Llm.t) ->
             Picker.Item.create
               ~id:model.key
               ~detail:model.provider
               ~search:(model.name ^ " " ^ model.key)
               ~dimmed:(not (logged_in m model.provider))
               model.name))
    }
;;

let save_scoped (m : Model.t) checked =
  let scoped_models =
    List.filter_map m.models ~f:(fun model ->
      Option.some_if (Set.mem checked model.key) model.key)
  in
  save_config
    { m with dialog = None }
    { (config m) with scoped_models }
    ~fields:[ "scoped_models" ]
    ~notice:
      (match scoped_models with
       | [] -> "Scope cleared: Ctrl+P cycles through the logged-in models"
       | l ->
         sprintf
           "%d scoped model%s: Ctrl+P and Alt+P cycle through them"
           (List.length l)
           (if List.length l = 1 then "" else "s"))
;;

let fallback_summary (m : Model.t) (config : Config.t) =
  match config.fallback_models with
  | [] ->
    "No fallback models: /fallback MODEL [MODEL...] sets the chain that takes \
     over when a model's usage runs out"
  | keys ->
    sprintf
      "Fallback: %s%s"
      (String.concat ~sep:" → " keys)
      (Option.value_map m.state ~default:"" ~f:(fun s ->
         sprintf " (now on %s)" s.model.key))
;;

(* The backend resolves names and prefixes as /model does, and says which
   models come closest to one it doesn't know. *)
let set_fallback (m : Model.t) models =
  with_config m ~f:(fun config ->
    ( m
    , [ set_config
          { config with fallback_models = models }
          ~fields:[ "fallback_models" ]
          ~tag:Fallback_saved
      ] ))
;;

let fallback_saved (m : Model.t) (config : Config.t) =
  match config.fallback_models with
  | [] ->
    toast
      m
      "Fallback cleared: a model whose usage runs out stops the run (/fallback \
       MODEL... sets a chain)"
  | _ -> toast m (fallback_summary m config)
;;

let default_dir_summary (config : Config.t) =
  match config.default_cwd with
  | Some dir ->
    sprintf
      "Default directory: %s (new sessions start there; /default-dir off \
       clears it)"
      dir
  | None ->
    "No default directory: new sessions start in the backend's working \
     directory; /default-dir PATH sets one"
;;

(* The backend resolves [default_cwd] where it was started, which the user
   cannot see, so a relative path is taken from the directory completion
   listed: the backend host's. *)
let default_dir (m : Model.t) dir =
  let normalize path =
    String.split path ~on:'/'
    |> List.fold ~init:[] ~f:(fun parts part ->
      match part, parts with
      | ("" | "."), _ | "..", [] -> parts
      | "..", _ :: parent -> parent
      | part, _ -> part :: parts)
    |> List.rev
    |> String.concat ~sep:"/"
    |> ( ^ ) "/"
  in
  let backend_cwd (s : State.t) =
    if String.equal s.active_host Host.backend_id
    then Some s.cwd
    else
      List.find_map s.hosts ~f:(fun (h : Host.t) ->
        Option.some_if (String.equal h.id Host.backend_id) h.cwd)
  in
  if String.is_prefix dir ~prefix:"~"
  then Ok dir
  else if String.is_prefix dir ~prefix:"/"
  then Ok (normalize dir)
  else (
    match Option.bind m.state ~f:backend_cwd with
    | Some cwd -> Ok (normalize (cwd ^ "/" ^ dir))
    | None ->
      Error
        (sprintf
           "Relative to what? The backend's directory isn't known yet: give an \
            absolute path (/default-dir /path/to/%s)"
           dir))
;;

let set_default_dir (m : Model.t) dir =
  with_config m ~f:(fun config ->
    ( m
    , [ set_config
          { config with default_cwd = dir }
          ~fields:[ "default_cwd" ]
          ~tag:Default_dir_saved
      ] ))
;;

let compaction_summary config =
  String.capitalize (Prigh_ui.Compaction_command.summary config)
;;

(* The backend resolves the model as /model does, and says which models
   come closest to one it doesn't know. *)
let compaction_command (m : Model.t) rest =
  match Prigh_ui.Compaction_command.parse rest with
  | Show -> with_config m ~f:(fun c -> toast m (compaction_summary c))
  | Error e -> error m (String.capitalize e)
  | (Set_at _ | Set_model _ | Set_mask _) as action ->
    with_config m ~f:(fun config ->
      ( m
      , [ set_config
            (Prigh_ui.Compaction_command.apply action config)
            ~fields:
              (List.map (Prigh_ui.Compaction_command.fields action) ~f:fst)
            ~tag:Compaction_saved
        ] ))
;;

let last_reply (m : Model.t) =
  List.rev (Chat.entries m.chat)
  |> List.find_map ~f:(fun (entry : Chat.Entry.t) ->
    match entry with
    | Assistant { message; _ } ->
      (match
         List.filter_map message.content ~f:(function
           | Text t when not (String.is_empty (String.strip t)) ->
             Some (String.strip t)
           | _ -> None)
       with
       | [] -> None
       | texts -> Some (String.concat ~sep:"\n\n" texts))
    | _ -> None)
;;

let copy_last (m : Model.t) =
  match last_reply m with
  | None -> error m "Nothing to copy yet: no reply in this session."
  | Some text ->
    let m, cmds = toast m "Copied the last reply to the clipboard" in
    m, Command.Copy text :: cmds
;;

(* A newer question replaces (and cancels) the previous one. *)
let start_btw (m : Model.t) question =
  let id = sprintf "btw-%d" (m.btw_seq + 1) in
  ( { m with btw = Some (Btw.create ~id ~question); btw_seq = m.btw_seq + 1 }
  , cancel_btw m
    @ [ rpc "btw" [ "question", str question; "btw_id", str id ] ~tag:(Btw id) ]
  )
;;

let close_btw (m : Model.t) =
  { m with btw = None }, cancel_btw m @ [ focus_editor ]
;;

let update_btw (m : Model.t) id ~f =
  match m.btw with
  | Some btw when String.equal btw.id id -> { m with btw = Some (f btw) }
  | _ -> m
;;

let host_label (m : Model.t) (h : Host.t) =
  let here =
    Option.exists m.hello ~f:(fun (hello : Hello_reply.t) ->
      String.equal hello.client_id h.id)
  in
  if here
  then h.name ^ " (this browser)"
  else (
    let ours = Option.map m.state ~f:(fun s -> s.session_id) in
    match h.session_id with
    | Some id when not (Option.equal String.equal ours (Some id)) ->
      sprintf "%s (in %s)" h.name (Option.value h.session_name ~default:id)
    | _ -> h.name)
;;

let hosts_picker (m : Model.t) =
  current_state m ~f:(fun state ->
    open_picker
      m
      (Picker
         { kind = Hosts
         ; picker =
             Picker.create
               ~title:"Where tools run"
               (List.map state.hosts ~f:(fun h ->
                  Picker.Item.create
                    ~id:h.id
                    ~detail:h.cwd
                    ~search:(h.name ^ " " ^ h.id ^ " " ^ h.cwd)
                    ~marked:(String.equal h.id state.active_host)
                    (host_label m h)))
         }))
;;

let open_prompt (m : Model.t) prompt =
  let m, cmds = open_dialog m ~focus:"dialog-input" (Prompt prompt) in
  let method_, params =
    Prompt.listing
      prompt
      ~active_host:
        (Option.value_map m.state ~default:Host.backend_id ~f:(fun s ->
           s.active_host))
  in
  m, cmds @ [ rpc method_ params ~tag:(Prompt_paths prompt.input) ]
;;

(* The directory belongs to the host, so switching asks for the one to use
   there, prefilled with the current one. *)
let host_prompt (m : Model.t) (host : Host.t) =
  open_prompt
    m
    (Prompt.create
       ~input:(Option.value_map m.state ~default:host.cwd ~f:(fun s -> s.cwd))
       (Host_cwd { host = host.id; name = host_label m host }))
;;

let switch_host (m : Model.t) arg =
  current_state m ~f:(fun state ->
    let here (h : Host.t) =
      String.equal arg "here"
      && Option.exists m.hello ~f:(fun hello ->
        String.equal hello.client_id h.id)
    in
    match
      List.filter state.hosts ~f:(fun h ->
        String.equal h.id arg || String.equal h.name arg || here h)
    with
    | [ host ] -> host_prompt m host
    | [] ->
      error
        m
        (sprintf
           "No tool host “%s”: one of %s (/host picks one)."
           arg
           (String.concat ~sep:", " (List.map state.hosts ~f:(fun h -> h.name))))
    | _ -> hosts_picker m)
;;

let export (m : Model.t) path =
  let format =
    if String.is_suffix path ~suffix:".jsonl" then "jsonl" else "markdown"
  in
  rpc
    "export"
    (("format", str format)
     :: (if String.is_empty path then [] else [ "path", str path ]))
    ~tag:Exported
  |> fun cmd -> m, [ cmd ]
;;

let change_dir ?(if_missing = Dir_change.If_missing.Ask) ~target path =
  call (Change_dir { target; path; if_missing }) ~tag:(Dir_changed target)
;;

let submit_prompt (m : Model.t) (prompt : Prompt.t) =
  let path = String.strip prompt.input in
  let busy = { m with dialog = Some (Prompt { prompt with busy = true }) } in
  match prompt.action, path with
  | Export, path ->
    let m, cmds = export busy path in
    m, cmds
  | (Cd | Mkdir | Host_cwd _ | Import), "" ->
    ( { m with
        dialog =
          Some
            (Prompt { prompt with error = Some "Type a path (Esc cancels)." })
      }
    , [] )
  | Cd, path -> busy, [ change_dir ~target:Cwd path ]
  | Mkdir, path -> busy, [ change_dir ~if_missing:Create ~target:Cwd path ]
  | Host_cwd { host; name }, path ->
    busy, [ change_dir ~target:(Host { id = host; name }) path ]
  | Import, path -> busy, [ rpc "import" [ "path", str path ] ~tag:Imported ]
;;

let with_prompt (m : Model.t) ~f =
  match m.dialog with
  | Some (Prompt prompt) -> f prompt
  | _ -> m, []
;;

let prompt_input (m : Model.t) prompt =
  let m = { m with dialog = Some (Prompt prompt) } in
  let method_, params =
    Prompt.listing
      prompt
      ~active_host:
        (Option.value_map m.state ~default:Host.backend_id ~f:(fun s ->
           s.active_host))
  in
  m, [ rpc method_ params ~tag:(Prompt_paths prompt.input) ]
;;

(* An inline failure keeps the dialog open so that the answer can be fixed. *)
let prompt_failed (m : Model.t) e =
  match m.dialog with
  | Some (Prompt ({ busy = true; _ } as prompt)) ->
    ( { m with
        dialog = Some (Prompt { prompt with busy = false; error = Some e })
      }
    , [] )
  | _ -> error m (String.capitalize e)
;;

(* Answering a prompt or the create dialog, or [/cd PATH] typed. *)
let dir_change_failed (m : Model.t) e =
  match m.dialog with
  | Some (Create_dir ({ busy = true; _ } as d)) ->
    ( { m with
        dialog = Some (Create_dir { d with busy = false; error = Some e })
      }
    , [] )
  | _ -> prompt_failed m e
;;

let dir_changed (m : Model.t) (target : Dir_change.Target.t) outcome =
  let ours =
    match m.dialog with
    | None | Some (Prompt { busy = true; _ } | Create_dir { busy = true; _ }) ->
      true
    | Some _ -> false
  in
  match (outcome : Dir_change.Outcome.t) with
  | Missing path when ours ->
    open_dialog m (Create_dir { target; path; busy = false; error = None })
  | Missing path ->
    error
      m
      (match target with
       | Cwd -> sprintf "%s doesn't exist: /mkdir %s creates it." path path
       | Host { name; _ } ->
         sprintf "%s doesn't exist on %s: /host %s asks again." path name name)
  | Changed { cwd; created } ->
    let m, close =
      match m.dialog with
      | Some (Prompt { busy = true; _ } | Create_dir { busy = true; _ }) ->
        close_dialog m
      | _ -> m, []
    in
    let notice =
      match target, created, cwd with
      | Cwd, false, Some cwd -> "Working directory: " ^ cwd
      | Cwd, _, None -> "Working directory changed"
      | Cwd, true, Some cwd -> sprintf "Created %s and switched to it" cwd
      | Host { name; _ }, false, Some cwd ->
        sprintf "Tools run on %s, in %s" name cwd
      | Host { name; _ }, _, None -> "Tools run on " ^ name
      | Host { name; _ }, true, Some cwd ->
        sprintf "Created %s: tools run on %s, in it" cwd name
    in
    let m, cmds = toast m notice in
    m, close @ cmds
;;

let prompt_succeeded (m : Model.t) =
  match m.dialog with
  | Some (Prompt { busy = true; _ }) -> close_dialog m
  | _ -> m, []
;;

let help_command (m : Model.t) name =
  let name = String.chop_prefix_if_exists name ~prefix:"/" in
  match Slash.find ~superuser:(superuser m) name with
  | Some spec -> toast m (sprintf "%s: %s" (Slash.Spec.usage spec) spec.help)
  | None ->
    error
      m
      (match Slash.closest ~superuser:(superuser m) name with
       | Some spec -> sprintf "No command /%s. Did you mean /%s?" name spec.name
       | None -> sprintf "No command /%s: /help lists them." name)
;;

let retry_connection (m : Model.t) =
  match m.connection with
  | Connected -> toast m "The backend is connected."
  | Reconnecting { attempt; _ } ->
    let generation = m.generation + 1 in
    let m =
      { m with
        connection = Reconnecting { attempt; generation }
      ; generation
      ; drops = Drops.retry m.drops
      }
    in
    let m, cmds = toast m "Reconnecting now…" in
    ( m
    , cmds
      @ [ Command.Reconnect
            { generation
            ; delay_ms = 0
            ; session = Option.map m.state ~f:(fun s -> s.session_id)
            }
        ] )
;;

(* ---- notifications ----------------------------------------------------- *)

(* [/notify on] from a key press or a click: the browser asks for permission
   only in response to the user. *)
let set_notifications (m : Model.t) enabled =
  let m = { m with notifications = { m.notifications with enabled } } in
  let save =
    save_pref m Key.notify (Some (if enabled then `True else `False))
  in
  if not enabled
  then (
    let m, cmds = toast m "Notifications off: /notify on turns them on" in
    m, save @ cmds)
  else (
    match m.notifications.permission with
    | Default -> m, save @ [ Command.Request_notification_permission ]
    | Granted ->
      let m, cmds = toast m (Page_notify.describe m.notifications) in
      m, save @ cmds
    | Denied | Unsupported ->
      let m, cmds = error m (Page_notify.describe m.notifications) in
      m, save @ cmds)
;;

(* ---- accounts ---------------------------------------------------------- *)

let own_user (m : Model.t) = Option.bind m.hello ~f:(fun h -> h.user)

let account_menu (m : Model.t) =
  let others =
    List.filter_mapi m.accounts ~f:(fun i account ->
      if Option.exists m.account ~f:(Accounts.Account.same account)
      then None
      else
        Some
          (Picker.Item.create
             ~id:(sprintf "switch %d" i)
             ~detail:(Accounts.Account.host account)
             (Accounts.Account.name account)))
  in
  let acting = Option.bind m.hello ~f:Hello_reply.acting_as in
  let act =
    match m.users, acting, own_user m with
    | None, _, _ -> []
    | Some _, None, _ ->
      [ Picker.Item.create
          ~id:"act"
          ~detail:"see and work in another user's sessions"
          "Act as…"
      ]
    | Some _, Some other, own ->
      [ Picker.Item.create
          ~id:"act"
          ~detail:(sprintf "now acting as %s" other)
          "Act as…"
      ; Picker.Item.create
          ~id:"back"
          ~detail:"your own sessions"
          ("Back to " ^ Option.value own ~default:"yourself")
      ]
  in
  Dialog.Picker
    { kind = Accounts
    ; picker =
        Picker.create
          ~title:"Account"
          (others
           @ act
           @ [ Picker.Item.create
                 ~id:"add"
                 ~detail:"sign in as another user, or to another backend"
                 "Add account…"
             ; (if m.notifications.enabled
                then
                  Picker.Item.create
                    ~id:"notify off"
                    ~detail:
                      "stop telling you what happens while the tab is hidden"
                    "Turn notifications off"
                else
                  Picker.Item.create
                    ~id:"notify on"
                    ~detail:"when a turn ends or something waits for you"
                    "Turn notifications on")
             ; Picker.Item.create ~id:"settings" ~detail:"Ctrl+," "Settings"
             ; Picker.Item.create ~id:"help" ~detail:"/help" "Commands and keys"
             ; Picker.Item.create
                 ~id:"signout"
                 ~detail:"forget this account in this browser"
                 "Sign out"
             ])
    }
;;

let users_picker (m : Model.t) users =
  let namespace = Option.bind m.hello ~f:(fun h -> h.namespace) in
  let own = own_user m in
  open_picker
    { m with users = Some users }
    (Picker
       { kind = Users
       ; picker =
           Picker.create
             ~title:"Act as"
             (List.map users ~f:(fun user ->
                Picker.Item.create
                  ~id:user
                  ~detail:
                    (if Option.equal String.equal own (Some user)
                     then "you"
                     else "")
                  ~marked:(Option.equal String.equal namespace (Some user))
                  user))
       })
;;

let act_as (m : Model.t) user =
  m, [ rpc "set_user" [ "user", str user ] ~tag:User_switched ]
;;

(* Everything but the connection belongs to the user we were. *)
let user_switched (m : Model.t) (hello : Hello_reply.t) =
  let cancel = cancel_btw m @ cancel_generate m in
  let m =
    { m with
      hello = Some hello
    ; state = None
    ; chat = Chat.empty
    ; paging = Paging.all
    ; chat_cache = Chat_cache.empty
    ; jump = Jump.empty
    ; trimmed = Trimmed.empty
    ; sessions = []
    ; marks = Session_marks.empty
    ; session_query = ""
    ; confirms = []
    ; confirm_reason = None
    ; questions = []
    ; queue = Queued.empty
    ; agent_queues = []
    ; recipient = Main
    ; dialog = None
    ; completion = None
    ; btw = None
    ; generate = Generate.empty
    ; git_bar = Git_bar.empty
    ; images = []
    ; skills = Skills.empty
    ; worktrees = Worktrees.reset m.worktrees
    ; pending = In_flight.none
    ; session_gone = None
    ; outputs_loading = String.Set.empty
    ; folds = String.Set.empty
    ; agents = { Agents.empty with open_ = m.agents.open_ }
    ; forges = Forges.empty
    ; nix_gc = { Nix_gc.empty with collecting = m.nix_gc.collecting }
    }
  in
  let m, cmds =
    toast
      m
      (match Hello_reply.acting_as hello, hello.user with
       | Some other, _ -> sprintf "Acting as %s" other
       | None, Some own -> sprintf "Back to %s" own
       | None, None -> "Switched user")
  in
  m, cancel @ cmds @ [ focus_editor ] @ startup @ fetch_prefs m
;;

let account_chosen (m : Model.t) id =
  match String.lsplit2 id ~on:' ', id with
  | Some ("switch", i), _ ->
    (match Option.bind (Int.of_string_opt i) ~f:(List.nth m.accounts) with
     | Some account -> m, [ Command.Switch_account account ]
     | None -> m, [])
  | _, "act" -> loading m "Act as" [ rpc "list_users" [] ~tag:(Users Picker) ]
  | _, "back" ->
    (match own_user m with
     | Some own -> act_as m own
     | None -> m, [])
  | _, "add" -> m, [ Command.Add_account ]
  | _, "signout" -> m, [ Command.Sign_out ]
  | _, "settings" -> open_settings m
  | _, "help" -> open_dialog m Help
  | _, ("notify on" | "notify off") ->
    let m, close = close_dialog m in
    let m, cmds = set_notifications m (String.equal id "notify on") in
    m, close @ cmds
  | _ -> m, []
;;

let entries_picker (m : Model.t) (purpose : Entries_purpose.t) json =
  match Session_tree.of_json json with
  | Error e -> error m (Error.to_string_hum e)
  | Ok (head, entries) ->
    (match purpose with
     | Fork | Rewind ->
       (match Session_tree.user_items entries with
        | [] -> toast m "No messages yet: there is nothing to go back to."
        | items ->
          let kind, title =
            match purpose with
            | Fork ->
              Dialog.Picker_kind.Fork entries, "Fork from (edit and resend)"
            | Rewind | Tree -> Rewind entries, "Rewind to"
          in
          open_picker m (Picker { kind; picker = Picker.create ~title items }))
     | Tree ->
       (match Session_tree.tree_items entries ~head with
        | [] -> toast m "No messages yet: the tree is empty."
        | items ->
          (* The head may be a model change or a name: highlight the last
             message before it. *)
          let highlight =
            List.rev items
            |> List.find ~f:(fun (i : Picker.Item.t) -> i.marked)
            |> Option.map ~f:(fun (i : Picker.Item.t) -> i.id)
          in
          ignore (head : string option);
          open_picker
            m
            (Picker
               { kind = Tree
               ; picker = Picker.create ?highlight ~title:"Session tree" items
               })))
;;

let open_skills (m : Model.t) ~query =
  current_state m ~f:(fun state ->
    match Skills.place (Some state) with
    | None -> m, []
    | Some place ->
      loading
        m
        "Skills"
        [ call List_skills ~tag:(Skills { place; purpose = Picker query }) ])
;;

let mcp_chosen (m : Model.t) (l : Mcp_list.t) id =
  match Mcp_servers.find l ~id with
  | None -> m, []
  | Some server ->
    (match server.status with
     | Ready -> open_dialog m (Mcp_tools server)
     | Needs_approval ->
       let m, cmds =
         toast m (sprintf "Approved %s: starting it…" server.name)
       in
       ( m
       , cmds
         @ [ call
               (Mcp_approve { source = server.source; server = server.name })
               ~tag:(Mcp (Refreshed id))
           ] )
     | Failed ->
       let m, cmds = toast m "Restarting the failed MCP servers…" in
       ( m
       , cmds
         @ [ call (List_mcp { reconnect = true }) ~tag:(Mcp (Refreshed id)) ] ))
;;

let mcp_listed (m : Model.t) (purpose : Mcp_purpose.t) (l : Mcp_list.t) =
  let say (text, `Error error) = toast m ~error text in
  match purpose with
  | Picker ->
    if List.is_empty l.servers && List.is_empty l.problems
    then toast m Mcp_servers.none
    else open_picker m (Mcp_servers.picker l)
  | Reconnect -> say (Mcp_servers.summary l)
  | Refreshed id ->
    let m, toast_cmds = say (Mcp_servers.outcome l ~id) in
    (* Back to the list, unless something else has been opened since. *)
    if Option.is_some m.dialog
    then m, toast_cmds
    else (
      let m, cmds = open_picker m (Mcp_servers.picker ~highlight:id l) in
      m, toast_cmds @ cmds)
;;

(* ---- snippets *)

let snippets (m : Model.t) = Option.map m.config ~f:(fun c -> c.snippets)

let last_prompt (m : Model.t) =
  List.rev (Chat.entries m.chat)
  |> List.find_map ~f:(function
    | Chat.Entry.User u ->
      let typed = Injected_note.typed u in
      Option.some_if (not (String.is_empty (String.strip typed))) typed
    | Assistant _ | Shell _ | Compaction _ -> None)
;;

(* At the caret: the editor's, or where the last edit left it. *)
let insert_snippet (m : Model.t) (snippet : Snippet.t) =
  let cursor = Int.clamp_exn m.cursor ~min:0 ~max:(String.length m.draft) in
  edit_at_caret
    m
    ~cursor:(cursor + String.length snippet.text)
    (String.prefix m.draft cursor
     ^ snippet.text
     ^ String.drop_prefix m.draft cursor)
;;

let snippet_action (m : Model.t) (action : Prigh_ui.Snippet_command.Action.t) =
  match action with
  | Pick snippets ->
    open_picker
      m
      (Picker
         { kind = Snippets
         ; picker =
             Picker.create
               ~title:"Snippets"
               (List.map snippets ~f:(fun (s : Snippet.t) ->
                  Picker.Item.create
                    ~id:s.name
                    ~detail:(Snippet.first_line s)
                    ~search:(s.name ^ " " ^ s.text)
                    s.name))
         })
  | Insert snippet -> insert_snippet m snippet
  | Save { snippets; notice } ->
    with_config m ~f:(fun config ->
      save_config m { config with snippets } ~fields:[ "snippets" ] ~notice)
  | Info text -> toast m text
  | Error text -> error m text
;;

(* ---- reminders *)

let reminders (m : Model.t) = Option.map m.state ~f:(fun s -> s.reminders)

(* A command that could not run, back in the editor to correct rather than
   retype, unless something else is being written there. *)
let back_in_editor (m : Model.t) command =
  if String.is_empty m.draft && List.is_empty m.images
  then set_draft m command
  else m
;;

let remind_action
      (m : Model.t)
      ~command
      (action : Prigh_ui.Remind_command.Action.t)
  =
  match action with
  | Pick reminders ->
    open_picker
      m
      (Picker
         { kind = Reminders
         ; picker =
             Picker.create
               ~title:"Reminders"
               (List.map reminders ~f:(fun (r : Reminder.t) ->
                  Picker.Item.create
                    ~id:r.name
                    ~detail:
                      (sprintf
                         "%s · %s"
                         (Reminder.Trigger.describe r.trigger)
                         (Reminder.first_line r))
                    ~search:(r.name ^ " " ^ r.text)
                    r.name))
         })
  | Info text -> toast m (String.capitalize text)
  | Error text -> error (back_in_editor m command) (String.capitalize text)
  | Add _ | Remove _ | Clear _ ->
    (match Prigh_ui.Remind_command.rpc action with
     | None -> m, []
     | Some (method_, params) ->
       let notice =
         match action with
         | Remove name -> Some (Prigh_ui.Remind_command.removed name)
         | Clear n -> Some (Prigh_ui.Remind_command.cleared n)
         | _ -> None
       in
       let m, focus =
         if Option.is_some m.dialog then close_dialog m else m, []
       in
       m, focus @ [ rpc method_ params ~tag:(Reminder { command; notice }) ])
;;

(* ---- worktrees *)

module Worktree_command = Prigh_ui.Worktree_command

let list_worktrees (m : Model.t) purpose =
  match Worktrees.place m.state with
  | Some place ->
    loading
      m
      "Worktrees"
      [ call (List_worktrees { cwd = None }) ~tag:(Worktrees { place; purpose })
      ]
  | None -> error m "No session yet: wait for it to load, then try again."
;;

let worktrees_picker (m : Model.t) (listing : Worktree.Listing.t) =
  let items =
    List.map listing.worktrees ~f:(fun w ->
      let label = Worktree.label w in
      let describe = Worktree_command.describe w in
      Picker.Item.create
        ~id:w.path
        ~detail:
          (Option.value
             (String.chop_prefix describe ~prefix:(label ^ " · "))
             ~default:describe)
        ~search:(Worktree_command.target w ^ " " ^ w.path)
        ~marked:w.current
        ~dimmed:w.prunable
        label)
  in
  open_picker
    m
    (Picker
       { kind = Worktrees listing
       ; picker = Picker.create ~title:("Worktrees of " ^ listing.root) items
       })
;;

(* A new session starts there: it has nowhere to switch back to. *)
let enter_worktree ?(new_session = false) ~notice path =
  call
    (if new_session
     then Change_dir { target = Cwd; path; if_missing = Fail }
     else Enter_worktree path)
    ~tag:(Worktree_entered notice)
;;

let with_entering (m : Model.t) entering =
  { m with worktrees = Worktrees.set_entering m.worktrees entering }
;;

let open_worktree (m : Model.t) (w : Worktree.t) ~new_session =
  if w.prunable
  then
    error
      m
      (sprintf
         "The worktree %s is gone (%s was deleted): /worktree rm %s forgets it."
         (Worktree.label w)
         w.path
         (Worktree_command.target w))
  else (
    let where = sprintf "the worktree %s (%s)" (Worktree.label w) w.path in
    let m = with_entering m (Some w.path) in
    if new_session
    then
      ( { (starting m) with sidebar_open = m.sidebar_open && not m.narrow }
      , [ rpc
            "new_session"
            []
            ~tag:
              (Worktree_session
                 { path = w.path; notice = "New session in " ^ where })
        ] )
    else m, [ enter_worktree ~notice:("Working in " ^ where) w.path ])
;;

let ask_remove_worktree (m : Model.t) (w : Worktree.t) ~force =
  match Worktree_command.cannot_remove w with
  | Some reason -> error m (String.capitalize reason ^ ".")
  | None ->
    open_dialog
      m
      (Remove_worktree
         { worktree = w; repo = None; force; busy = false; error = None })
;;

(* The worktree a sidebar row or the picker names, by its path. *)
let shown_worktree (m : Model.t) path =
  match m.dialog with
  | Some (Picker { kind = Worktrees listing; _ }) ->
    List.find listing.worktrees ~f:(fun w -> String.equal w.path path)
  | _ -> Worktrees.find m.worktrees path
;;

(* A worktree under a session in the sidebar that the session's listing
   does not have (another repository's, a subagent's): what is known of it,
   and its repository. *)
let session_worktree (m : Model.t) path =
  List.find_map m.sessions ~f:(fun s ->
    List.find s.worktrees ~f:(fun w -> String.equal w.path path))
  |> Option.map ~f:(fun (w : Session_worktree.t) ->
    let listed =
      List.find_map (worktree_listings m) ~f:(fun l ->
        List.find l.worktrees ~f:(fun lw -> String.equal lw.path path))
    in
    let worktree : Worktree.t =
      match listed with
      | Some listed -> listed
      | None ->
        { path = w.path
        ; branch = w.branch
        ; head = None
        ; main = false
        ; managed = true
        ; locked = false
        ; prunable = false
        ; changes = None
        ; upstream = None
        ; ahead = 0
        ; behind = 0
        ; current = false
        ; sessions = []
        }
    in
    worktree, w.root)
;;

(* From the sidebar: a worktree of any repository. *)
let ask_remove_session_worktree (m : Model.t) path =
  match shown_worktree m path, session_worktree m path with
  | Some w, _ -> ask_remove_worktree m w ~force:false
  | None, Some (w, repo) ->
    let here =
      Option.exists m.state ~f:(fun s ->
        String.equal s.cwd w.path
        || String.is_prefix s.cwd ~prefix:(w.path ^ "/"))
    in
    if here
    then
      error
        m
        (sprintf
           "This session works in %s: /cd out of it first (e.g. to %s)."
           w.path
           repo)
    else
      open_dialog
        m
        (Remove_worktree
           { worktree = w
           ; repo = Some repo
           ; force = false
           ; busy = false
           ; error = None
           })
  | None, None ->
    error m (sprintf "No worktree at %s any more: /worktree lists them." path)
;;

(* A new session working in [path]: a worktree. *)
let new_session_in (m : Model.t) path =
  ( { (starting (with_entering m (Some path))) with
      sidebar_open = m.sidebar_open && not m.narrow
    }
  , [ rpc
        "new_session"
        []
        ~tag:(Worktree_session { path; notice = "New session in " ^ path })
    ] )
;;

let with_worktree (m : Model.t) path ~f =
  match shown_worktree m path with
  | Some w -> f w
  | None ->
    error m (sprintf "No worktree at %s any more: /worktree lists them." path)
;;

let remove_worktree (m : Model.t) ~force =
  match m.dialog with
  | Some (Remove_worktree ({ busy = false; worktree; _ } as d)) ->
    let force = force || d.force in
    ( { m with
        dialog =
          Some (Remove_worktree { d with force; busy = true; error = None })
      }
    , [ call
          (Remove_worktree { path = worktree.path; force; cwd = d.repo })
          ~tag:
            (Worktree_removed
               { target = Worktree_command.target worktree; force })
      ] )
  | _ -> m, []
;;

let remove_command ~force target =
  sprintf "/worktree rm %s%s" (if force then "-f " else "") target
;;

(* One with uncommitted changes is kept unless forced: say how. *)
let worktree_remove_failed (m : Model.t) ~target ~force e =
  let refused = (not force) && String.is_substring e ~substring:"force" in
  match m.dialog with
  | Some (Remove_worktree ({ busy = true; _ } as d)) ->
    let e =
      if refused
      then
        sprintf
          "%s: “%s” removes it anyway."
          (String.capitalize e)
          (Worktrees.force_label d.worktree)
      else String.capitalize e ^ "."
    in
    ( { m with
        dialog = Some (Remove_worktree { d with busy = false; error = Some e })
      }
    , [] )
  | _ ->
    let command = remove_command ~force:refused target in
    error
      (back_in_editor m command)
      (if refused
       then sprintf "%s: %s removes it anyway." (String.capitalize e) command
       else String.capitalize e ^ ".")
;;

let creating = "Creating the worktree "

let new_worktree_dialog (m : Model.t) =
  current_state m ~f:(fun state ->
    open_dialog
      m
      ~focus:(New_worktree.Field.id Branch)
      (New_worktree
         (New_worktree.create
            ~head:state.git_branch
            ~project_dir:
              (Option.map (Worktrees.listing m.worktrees) ~f:(fun l ->
                 l.project_dir))
            ())))
;;

let create_worktree (m : Model.t) (d : New_worktree.t) =
  if d.busy
  then m, []
  else (
    match New_worktree.request d with
    | Error e ->
      { m with dialog = Some (New_worktree { d with error = Some e }) }, []
    | Ok (branch, base) ->
      ( { m with
          dialog = Some (New_worktree { d with busy = true; error = None })
        }
      , [ call
            (Create_worktree { branch; base; cwd = None })
            ~tag:
              (Worktree_created { command = New_worktree.command ~branch ~base })
        ] ))
;;

let worktree_created (m : Model.t) (c : Worktree.Created.t) =
  let m, close =
    match m.dialog with
    | Some (New_worktree { busy = true; _ }) -> close_dialog m
    | _ -> m, []
  in
  let m =
    with_entering
      { m with
        worktrees = Worktrees.stale m.worktrees
      ; repo_worktrees = Repo_listings.stale m.repo_worktrees
      }
      (Some c.worktree.path)
  in
  let m, cmds =
    toast
      m
      ~replaces:[ creating ]
      (String.capitalize (Worktree_command.created c))
  in
  ( m
  , close
    @ cmds
    @ [ enter_worktree
          ~notice:
            (sprintf
               "Working in the worktree %s (%s)"
               (Worktree.label c.worktree)
               c.worktree.path)
          c.worktree.path
      ] )
;;

let worktree_create_failed (m : Model.t) ~command e =
  match m.dialog with
  | Some (New_worktree ({ busy = true; _ } as d)) ->
    ( { m with
        dialog =
          Some
            (New_worktree
               { d with busy = false; error = Some (String.capitalize e ^ ".") })
      }
    , [] )
  | _ ->
    toast
      (back_in_editor m command)
      ~error:true
      ~replaces:[ creating ]
      (sprintf "Couldn't create the worktree: %s." e)
;;

let worktree_target
      (m : Model.t)
      ~command
      (action : Worktree_command.Action.t)
      listing
  =
  let resolve target ~f =
    match Worktree_command.find listing target with
    | Error e -> error (back_in_editor m command) (String.capitalize e ^ ".")
    | Ok w -> f w
  in
  match action with
  | List | New _ | Back _ | Error _ -> m, []
  | Open { target; new_session } ->
    resolve target ~f:(open_worktree m ~new_session)
  | Remove { target; force } ->
    resolve target ~f:(fun w ->
      let m =
        if Option.is_some (Worktree_command.cannot_remove w)
        then back_in_editor m command
        else m
      in
      ask_remove_worktree m w ~force)
;;

(* ---- /review: the review page on a worktree, or the model's review *)

module Review_command = Prigh_ui.Review_command

(* A worktree or branch without a base named: compared as it was last. *)
let remembered (m : Model.t) (target : Code_review.Target.t) =
  match target.base with
  | Some _ -> target
  | None ->
    (match
       Option.bind
         (Review_memory.key ~own:m.review.own_root target.reviewed)
         ~f:(fun key -> Review_memory.compare m.review_memory ~key)
     with
     | Some c ->
       { target with base = c.base; direct = target.direct || c.direct }
     | None -> target)
;;

let review_show (m : Model.t) (target : Code_review.Target.t) =
  with_review m (Code_review.update m.review (Show (remembered m target)))
;;

(* What the page compares, once its diff is in: the next visit starts
   there. *)
let remember_review (m : Model.t) =
  match m.review.diff, m.review.open_ with
  | Some _, true ->
    let target = m.review.target in
    let memory =
      Review_memory.remember
        m.review_memory
        ~own:m.review.own_root
        target.reviewed
        { base = target.base; direct = target.direct }
    in
    if Review_memory.equal memory m.review_memory
    then m, []
    else
      ( { m with review_memory = memory }
      , save_pref m Key.review (Some (Review_memory.to_json memory)) )
  | _ -> m, []
;;

(* Opening the page on the session's worktree: where it was last. *)
let review_update (m : Model.t) (action : Code_review.Action.t) =
  let opening =
    match action with
    | Open None -> true
    | Toggle -> not m.review.open_
    | _ -> false
  in
  let last =
    Option.bind m.review.own_root ~f:(fun own ->
      Review_memory.last m.review_memory ~own)
  in
  match last with
  | Some reviewed
    when opening
         && Code_review.Target.equal m.review.target Code_review.Target.default
    -> review_show m { Code_review.Target.default with reviewed }
  | _ -> with_review m (Code_review.update m.review action)
;;

let review_worktree (m : Model.t) ~dir ~base =
  review_show m { reviewed = Dir dir; base; direct = false }
;;

let rec review_action (m : Model.t) ~command (action : Review_command.Action.t) =
  match action with
  | Show { target; base; direct } ->
    review_show m { reviewed = target; base; direct }
  | Named { name; base; direct; model } ->
    (match
       Option.map (Worktrees.listing m.worktrees) ~f:(fun l ->
         Worktree_command.find l name)
     with
     | Some (Ok w) ->
       review_show m { reviewed = Dir (Some w.path); base; direct }
     | Some (Error _) | None ->
       (match Worktrees.place m.state with
        | Some place ->
          ( m
          , [ call
                (List_worktrees { cwd = None })
                ~tag:
                  (Worktrees
                     { place
                     ; purpose =
                         Review_target { command; name; base; direct; model }
                     })
            ] )
        | None ->
          review_action
            m
            ~command
            (match model with
             | Some args -> Model args
             | None -> Error "no session yet: wait for it to load")))
  | Model args ->
    let m, cmds = toast m reading_review in
    ( m
    , cmds
      @ [ rpc
            "review"
            [ "args", str args ]
            ~tag:(Command_prompt (String.strip ("/review " ^ args)))
        ] )
  | Error e -> error (back_in_editor m command) (String.capitalize e ^ ".")
;;

let review_target (m : Model.t) ~command ~name ~base ~direct ~model listing =
  review_action
    m
    ~command
    (Review_command.resolve listing ~name ~base ~direct ~model)
;;

(* ---- the review page's pickers *)

let review_picker (m : Model.t) ?worktrees (which : Review_picker.Which.t) =
  match which, m.review.diff with
  | Target, Some d ->
    Some
      (Review_picker.targets
         ~worktrees:
           (match worktrees with
            | Some worktrees -> worktrees
            | None ->
              Option.value_map
                (Worktrees.listing m.worktrees)
                ~default:[]
                ~f:(fun l -> l.worktrees))
         ~reviewed:m.review.target.reviewed
         d)
  | Base, Some d -> Some (Review_picker.bases d)
  | (Target | Base), None -> None
  | Since path, _ ->
    Some
      (Review_action.picker
         m.review
         ~path
         ~me:(own_user m)
         ~time:(Model.message_time m))
;;

let open_review_pick (m : Model.t) (which : Review_picker.Which.t) =
  match m.dialog, review_picker m which with
  | Some (Review_pick p), _
    when Review_picker.Which.equal (Review_picker.which p) which ->
    close_dialog m
  | _, None -> toast m "The changes are still being read: try again."
  | _, Some p when List.is_empty (Review_picker.visible p) ->
    toast
      m
      "No review actions on this file yet: approve it or comment on it first."
  | _, Some p ->
    let m, cmds = open_dialog m ~focus:"review-pick-filter" (Review_pick p) in
    (* The worktrees as they are now: one may have been made since. *)
    ( m
    , cmds
      @
        (match which, Worktrees.place m.state with
        | Target, Some place ->
          [ call
              (List_worktrees { cwd = None })
              ~tag:(Worktrees { place; purpose = Review_pick })
          ]
        | Target, None | (Base | Since _), _ -> []) )
;;

(* The diff, the review or the worktrees changed under an open picker: its
   choices follow. *)
let refresh_review_pick ?worktrees (m : Model.t) =
  match m.dialog with
  | Some (Review_pick p) ->
    let worktrees =
      match worktrees, Review_picker.worktrees p with
      | Some w, _ -> Some w
      | None, [] -> None
      | None, w -> Some w
    in
    (match review_picker m ?worktrees (Review_picker.which p) with
     | Some fresh ->
       { m with dialog = Some (Review_pick (Review_picker.refresh p ~fresh)) }
     | None -> m)
  | _ -> m
;;

let review_picked (m : Model.t) (item : Review_picker.Item.t) =
  let m, _ = close_dialog m in
  let target = m.review.target in
  let m, cmds =
    match item.choice with
    | Review reviewed when Git_diff.Target.equal reviewed target.reviewed ->
      m, []
    | Review reviewed -> review_show m { reviewed; base = None; direct = false }
    | Base base ->
      review_show
        m
        { target with
          base = Some base
        ; direct = target.direct && not (String.equal base "HEAD")
        }
    | Since { path; index = None } -> review_update m (Follow_latest path)
    | Since { path; index = Some index } ->
      review_update m (Pick_since { path; index })
  in
  m, cmds
;;

let review_direct (m : Model.t) direct =
  let target = m.review.target in
  match m.review.diff with
  | Some d
    when not (Bool.equal d.direct direct && Bool.equal target.direct direct) ->
    (* The base the backend chose, named, so that it stays. *)
    review_show m { target with base = Some d.base; direct }
  | Some _ | None -> m, []
;;

(* A branch's name for [create_worktree]: a remote branch's local one. *)
let checkout_branch (d : Git_diff.t) rev =
  if List.mem d.branches rev ~equal:String.equal
  then Some rev
  else if List.mem d.remote_branches rev ~equal:String.equal
  then Option.map (String.lsplit2 rev ~on:'/') ~f:snd
  else None
;;

let review_checkout (m : Model.t) =
  match Code_review.read_only m.review, m.review.diff with
  | Some rev, Some d ->
    (match checkout_branch d rev with
     | None ->
       error
         m
         (sprintf
            "%s is not a branch: make one there first (/worktree new \
             BRANCH              --base %s)."
            rev
            rev)
     | Some branch ->
       let m, cmds = toast m (sprintf "%s%s…" creating branch) in
       ( m
       , cmds
         @ [ call
               (Create_worktree { branch; base = None; cwd = None })
               ~tag:
                 (Review_checked_out
                    { reviewed = Dir None
                    ; base = Some d.base
                    ; direct = d.direct
                    })
           ] ))
  | _ -> m, []
;;

let review_checked_out
      (m : Model.t)
      (target : Code_review.Target.t)
      (c : Worktree.Created.t)
  =
  let m =
    { m with
      worktrees = Worktrees.stale m.worktrees
    ; repo_worktrees = Repo_listings.stale m.repo_worktrees
    }
  in
  let m, toasts =
    toast
      m
      ~replaces:[ creating ]
      (sprintf
         "Checked out %s into %s: edit it there."
         (Worktree.label c.worktree)
         c.worktree.path)
  in
  let m, cmds =
    review_show m { target with reviewed = Dir (Some c.worktree.path) }
  in
  m, toasts @ cmds
;;

let worktree_command (m : Model.t) rest =
  let command = String.strip ("/worktree " ^ rest) in
  match Worktree_command.parse rest with
  | Error text -> error (back_in_editor m command) (String.capitalize text ^ ".")
  | List -> list_worktrees m Picker
  | New { branch; base } ->
    let m, cmds = toast m (sprintf "%s%s…" creating branch) in
    ( m
    , cmds
      @ [ call
            (Create_worktree { branch; base; cwd = None })
            ~tag:(Worktree_created { command })
        ] )
  | Back { main } ->
    (match m.state with
     | Some { home = None; _ } ->
       error
         (back_in_editor m command)
         (String.capitalize Worktree_command.no_home ^ ".")
     | Some { home; _ } ->
       ( m
       , [ call
             (Worktree_back { main })
             ~tag:(Worktree_went_back { command; home })
         ] )
     | None -> m, [])
  | (Open _ | Remove _) as action ->
    list_worktrees m (Target { command; action })
;;

(* The sidebar's listing follows the session, its directory and host, and
   the sessions' directories. *)
let follow_worktrees (before : Model.t) ((m : Model.t), cmds) =
  let moved =
    (not (phys_equal before.sessions m.sessions))
    && not
         ([%equal: (string * string) list]
            (List.map before.sessions ~f:(fun s -> s.id, s.cwd))
            (List.map m.sessions ~f:(fun s -> s.id, s.cwd)))
  in
  let m =
    if moved
    then { m with worktrees = Worktrees.sessions_moved m.worktrees }
    else m
  in
  (* A turn may have changed files, branches or worktrees. *)
  let turn_ended =
    Model.running before
    && (not (Model.running m))
    && Option.equal
         String.equal
         (Option.map before.state ~f:(fun s -> s.session_id))
         (Option.map m.state ~f:(fun s -> s.session_id))
  in
  (* The sessions' worktrees changed: their repositories are listed again,
     so that a new one is not taken for removed. *)
  let worktrees_changed =
    (not (phys_equal before.sessions m.sessions))
    && not
         ([%equal: (string * string list) list]
            (List.map before.sessions ~f:(fun s ->
               s.id, List.map s.worktrees ~f:(fun w -> w.path)))
            (List.map m.sessions ~f:(fun s ->
               s.id, List.map s.worktrees ~f:(fun w -> w.path))))
  in
  let m =
    if turn_ended
    then { m with worktrees = Worktrees.stale ~quiet:true m.worktrees }
    else m
  in
  let m =
    if turn_ended || worktrees_changed
    then { m with repo_worktrees = Repo_listings.stale m.repo_worktrees }
    else m
  in
  match m.connection, m.state with
  | Connected, Some state when Option.is_none m.session_gone ->
    let m, own =
      match Worktrees.wanted m.worktrees m.state with
      | Some place ->
        let worktrees, seq = Worktrees.requested m.worktrees state in
        ( { m with worktrees }
        , [ call
              (List_worktrees { cwd = None })
              ~tag:(Worktrees { place; purpose = Sidebar seq })
          ] )
      | None -> m, []
    in
    (* Only the repositories on the session's tool host can be listed; its
       own repository's listing is the one above. *)
    let own_root =
      List.find_map m.sessions ~f:(fun s ->
        if String.equal s.id state.session_id
        then Option.map s.repo ~f:(fun r -> r.root)
        else None)
    in
    let roots =
      Prigh_ui.Session_worktrees.roots
        (List.filter m.sessions ~f:(fun s ->
           Option.for_all s.host ~f:(String.equal state.active_host)))
      |> List.filter ~f:(fun root ->
        not (Option.exists own_root ~f:(String.equal root)))
      |> Repo_listings.wanted m.repo_worktrees
    in
    ( { m with repo_worktrees = Repo_listings.requested m.repo_worktrees roots }
    , cmds
      @ own
      @ List.map roots ~f:(fun root ->
        call (List_worktrees { cwd = Some root }) ~tag:(Repo_worktrees root)) )
  | _ -> m, cmds
;;

let open_snippets (m : Model.t) ~cursor =
  let m =
    Option.value_map cursor ~default:m ~f:(fun cursor -> { m with cursor })
  in
  snippet_action m (Prigh_ui.Snippet_command.pick (snippets m))
;;

(* [/agents steer <n|id> TEXT], [/agents follow-up <n|id> TEXT]. *)
let agents_message (m : Model.t) rest =
  let split s =
    let s = String.lstrip s in
    match String.lfindi s ~f:(fun _ c -> Char.is_whitespace c) with
    | Some i -> String.prefix s i, String.strip (String.drop_prefix s i)
    | None -> s, ""
  in
  let verb, rest = split rest in
  let target, text = split rest in
  let usage =
    sprintf "Usage: /agents %s <n|id> <message> (/agents lists them)" verb
  in
  match target, text with
  | "", _ | _, "" -> error m usage
  | target, text ->
    let agent =
      match Agents.resolve m.agents target with
      | Some (Agent id) -> Ok id
      | Some (Job id) ->
        Error
          (sprintf
             "%s is a background job: it takes no messages (/agents lists the \
              subagents)."
             id)
      | None when Option.is_some (Int.of_string_opt target) ->
        Error
          (sprintf
             "No subagent %s: give its number (1-%d) or id; /agents lists them."
             target
             (List.length (Agents.listed m.agents)))
      (* The backend knows the subagents we have not listed yet. *)
      | None -> Ok target
    in
    (match agent with
     | Error e -> error m e
     | Ok agent ->
       message_agent
         m
         ~agent
         ~text
         ~follow_up:(String.equal verb "follow-up")
         ~draft:(String.strip ("/agents " ^ verb ^ " " ^ rest)))
;;

(* Undoing the trim before a message (its divider's Undo): that range, even
   when a trim after a message came later. *)

let trim_state (m : Model.t) = Option.bind m.state ~f:(fun s -> s.trim)

let hidden_state (m : Model.t) =
  Option.value_map m.state ~default:[] ~f:(fun (s : State.t) -> s.hidden)
;;

let untrim (m : Model.t) =
  match trim_state m with
  | None ->
    error
      m
      "Nothing is trimmed from the context: /trim picks a message to trim \
       before."
  | Some _ when m.trimmed.restoring -> m, []
  | Some _ ->
    let method_ : Request.Method.t =
      match
        List.find (hidden_state m) ~f:(fun (h : Hidden.t) ->
          Hidden.Kind.equal h.kind Trim_before)
      with
      | Some h -> Restore h
      | None -> Untrim
    in
    ( { m with trimmed = { m.trimmed with restoring = true } }
    , [ call method_ ~tag:Untrimmed ] )
;;

let untrimmed (m : Model.t) result =
  let m = { m with trimmed = { m.trimmed with restoring = false } } in
  match result with
  | Ok json ->
    decode m json Trim.Freed.of_json ~f:(fun (freed : Trim.Freed.t) ->
      context_toast
        m
        (sprintf
           "Back in the context: %d message%s (%s)"
           freed.messages
           (if freed.messages = 1 then "" else "s")
           (Trim.tokens_label freed.tokens)))
  | Error e -> error m (String.capitalize e)
;;

(* ---- deleting messages, trimming the context after a message *)

let running_error = function
  | `Delete ->
    "Cannot delete a message while a run is in progress: wait for the run to \
     end or abort it (Esc)."
  | `Trim ->
    "Cannot trim while a run is in progress: wait for the run to end or abort \
     it (Esc)."
  | `Undo_trim ->
    "Cannot undo a trim while a run is in progress: wait for the run to end or \
     abort it (Esc)."
  | `Undo_delete ->
    "Cannot undo a deletion while a run is in progress: wait for the run to \
     end or abort it (Esc)."
;;

let entry_at (m : Model.t) n =
  match List.nth (Chat.entries m.chat) (n - Chat.first m.chat) with
  | Some (User u) -> Some u.at
  | Some (Assistant { message; _ }) -> Some message.at
  | Some (Shell _ | Compaction _) | None -> None
;;

let not_in_context what =
  sprintf
    "That message is not in the model's context any more: %s lists those you \
     can %s."
    (match what with
     | `Delete -> "/delete"
     | `Trim -> "/trim after")
    (match what with
     | `Delete -> "delete"
     | `Trim -> "trim after")
;;

let undelete_entry (m : Model.t) entry =
  { m with
    trimmed =
      { m.trimmed with
        deleting = Option.fold entry ~init:m.trimmed.deleting ~f:Set.remove
      }
  }
;;

(* Hidden at once; the backend says where it is in the context. *)
let ask_delete_message (m : Model.t) n =
  match Message_index.span (Model.message_index m) n, entry_at m n with
  | None, _ | _, None -> m, []
  | Some _, Some _ when Model.running m -> error m (running_error `Delete)
  | Some span, Some at ->
    ( { m with
        trimmed = { m.trimmed with deleting = Set.add m.trimmed.deleting n }
      }
    , [ call
          Get_message_points
          ~tag:(Message_points (Delete { entry = n; index = span.first; at }))
      ] )
;;

let delete_point (m : Model.t) ~entry (point : Message_point.t) =
  let m =
    { m with
      trimmed =
        { m.trimmed with
          deleting = Option.fold entry ~init:m.trimmed.deleting ~f:Set.add
        }
    }
  in
  ( m
  , [ call (Delete_message { message = point.message }) ~tag:(Deleted { entry })
    ] )
;;

let delete_chosen (m : Model.t) (point : Message_point.t) =
  if Model.running m
  then error m (running_error `Delete)
  else
    delete_point
      m
      ~entry:(Message_index.entry_of_index (Model.message_index m) point.index)
      point
;;

(* Where the page goes once the transcript is reloaded without what this
   page took out. *)
let reveal_hidden (m : Model.t) =
  match m.trimmed.reveal_hidden, m.state with
  | Some (entry, target), Some state
    when List.exists state.hidden ~f:(fun h -> String.equal h.entry entry)
         && not (Chat_cache.loading m.chat_cache state.session_path) ->
    let m = { m with trimmed = { m.trimmed with reveal_hidden = None } } in
    (match target with
     | None -> m, [ Command.Scroll_to_hidden entry ]
     | Some index ->
       ( m
       , Option.to_list
           (Option.map
              (Message_index.entry_at (Model.message_index m) index)
              ~f:(fun n -> Command.Scroll_to_message n)) ))
  | _ -> m, []
;;

let deleted (m : Model.t) ~entry result =
  match result with
  | Ok json ->
    decode m json Hidden.of_json ~f:(fun (h : Hidden.t) ->
      let index =
        Option.bind entry ~f:(fun n ->
          Message_index.span (Model.message_index m) n)
        |> Option.map ~f:(fun (span : Message_index.Span.t) ->
          Int.max 0 (span.first - 1))
      in
      let m, cmds = toast m ~undo:h (Hidden.deleted_label h) in
      let m, reveal =
        reveal_hidden
          { m with
            trimmed =
              { m.trimmed with
                reveal_hidden = Option.map index ~f:(fun i -> h.entry, Some i)
              }
          }
      in
      m, cmds @ reveal)
  | Error e -> error (undelete_entry m entry) (String.capitalize e)
;;

let restore_hidden (m : Model.t) (h : Hidden.t) =
  if Model.running m
  then
    error
      m
      (running_error (if Hidden.is_trim h then `Undo_trim else `Undo_delete))
  else if Set.mem m.trimmed.restoring_ranges h.entry
  then m, []
  else
    ( { m with
        trimmed =
          { m.trimmed with
            restoring_ranges = Set.add m.trimmed.restoring_ranges h.entry
          }
      ; toasts =
          List.filter m.toasts ~f:(fun t ->
            not ([%equal: Hidden.t option] t.undo (Some h)))
      }
    , [ call (Restore h) ~tag:(Range_restored h.entry) ] )
;;

let restore_range (m : Model.t) entry =
  match
    List.find (hidden_state m) ~f:(fun (h : Hidden.t) ->
      String.equal h.entry entry)
  with
  | Some h -> restore_hidden m h
  | None ->
    error
      m
      "That is back in the context already: /trim undo brings back the latest \
       trim."
;;

let restored (m : Model.t) entry result =
  let m =
    { m with
      trimmed =
        { m.trimmed with
          restoring_ranges = Set.remove m.trimmed.restoring_ranges entry
        }
    }
  in
  match result with
  | Ok json ->
    decode m json Hidden.of_json ~f:(fun (h : Hidden.t) ->
      context_toast
        m
        (sprintf
           "Back in the context: %d message%s (%s)"
           h.messages
           (if h.messages = 1 then "" else "s")
           (Trim.tokens_label h.tokens)))
  | Error e -> error m (String.capitalize e)
;;

(* [/delete undo]. *)
let undelete (m : Model.t) =
  match
    List.rev (hidden_state m)
    |> List.find ~f:(fun (h : Hidden.t) -> Hidden.Kind.equal h.kind Deleted)
  with
  | None -> error m "Nothing is deleted: /delete picks a message to delete."
  | Some h -> restore_hidden m h
;;

(* [/trim undo]: the latest trim, before or after a message. *)
let untrim_latest (m : Model.t) =
  match List.rev (hidden_state m) |> List.find ~f:Hidden.is_trim with
  | Some ({ kind = Trim_after _; _ } as h) -> restore_hidden m h
  | Some { kind = Trim_before | Deleted; _ } | None -> untrim m
;;

let trim_after_title = "Trim the context after"

(* "Trim everything after this": after its tool calls' results too. *)
let ask_trim_after (m : Model.t) n =
  match Message_index.span (Model.message_index m) n, entry_at m n with
  | None, _ | _, None -> m, []
  | Some _, Some _ when Model.running m -> error m (running_error `Trim)
  | Some span, Some at ->
    loading
      m
      trim_after_title
      [ call
          Get_message_points
          ~tag:
            (Message_points
               (Trim_after { index = span.first; last = span.next - 1; at }))
      ]
;;

let message_picker (m : Model.t) ~title ~freed (points : Message_point.t list) =
  let times = Model.message_time m in
  Picker.create
    ~title
    (List.rev_map points ~f:(fun (p : Message_point.t) ->
       let text = Jump.headline p.text in
       let who =
         match p.role with
         | User -> "you"
         | Assistant -> "reply"
         | Tool_result -> "result"
       in
       Picker.Item.create
         ~id:p.message
         ~detail:
           (String.concat
              ~sep:"  "
              ([ who ]
               @ Option.to_list (Option.map p.at ~f:(Message_time.short times))
               @ Option.to_list
                   (Option.map (freed p) ~f:(fun (f : Trim.Freed.t) ->
                      sprintf
                        "%d message%s · %s"
                        f.messages
                        (if f.messages = 1 then "" else "s")
                        (Trim.tokens_label f.tokens)))))
         ~search:text
         (if String.is_empty text then "(an image)" else text)))
;;

let trim_after_confirm (m : Model.t) point =
  open_dialog m (Trim_after_confirm { point; busy = false; error = None })
;;

let same_time at (point : Message_point.t) =
  Option.is_none at
  || Option.is_none point.at
  || [%equal: Time_ns.t option] at point.at
;;

let message_points_arrived
      (m : Model.t)
      (purpose : Points_purpose.t)
      (points : Message_point.t list)
  =
  let at index =
    List.find points ~f:(fun (p : Message_point.t) -> p.index = index)
  in
  match purpose with
  | Delete_picker ->
    (match
       List.filter points ~f:(fun (p : Message_point.t) ->
         Option.is_some p.delete)
     with
     | [] -> toast m "Nothing to delete: the model's context has no messages."
     | points ->
       open_picker
         m
         (Picker
            { kind = Delete_message points
            ; picker =
                message_picker
                  m
                  ~title:"Delete a message"
                  ~freed:(fun p -> p.delete)
                  points
            }))
  | Trim_after_picker ->
    (match
       List.filter points ~f:(fun (p : Message_point.t) ->
         Option.is_some p.trim_after)
     with
     | [] ->
       toast
         m
         "Nothing to trim: no message of the model's context has anything \
          after it that can go."
     | points ->
       open_picker
         m
         (Picker
            { kind = Trim_after points
            ; picker =
                message_picker
                  m
                  ~title:"Trim the context after"
                  ~freed:(fun p -> p.trim_after)
                  points
            }))
  | Trim_before { index; at = time } ->
    (match at index with
     | Some ({ trim_before = Some freed; _ } as point) when same_time time point
       ->
       open_dialog
         m
         (Trim_confirm
            { point =
                { before = point.message
                ; index = point.index
                ; reply =
                    (match point.role with
                     | Assistant -> true
                     | User | Tool_result -> false)
                ; text = point.text
                ; at = point.at
                ; messages = freed.messages
                ; tokens = freed.tokens
                }
            ; busy = false
            ; error = None
            })
     | Some point when same_time time point ->
       error
         m
         (if List.exists points ~f:(fun p -> p.index < index)
          then
            "Trimming there would part a tool call from its result: /trim \
             lists where you can trim."
          else
            "That is the first message in the model's context: there is \
             nothing before it to trim.")
     | _ ->
       error
         m
         "That message is not in the model's context any more: /trim lists \
          those you can trim before.")
  | Delete { entry; index; at = time } ->
    (match at index with
     | Some point when same_time time point && Option.is_some point.delete ->
       delete_point m ~entry:(Some entry) point
     | _ -> error (undelete_entry m (Some entry)) (not_in_context `Delete))
  | Trim_after { index; last; at = time } ->
    (match at index, at last with
     | Some point, Some ({ trim_after = Some _; _ } as last)
       when same_time time point ->
       (* What it says is the message's; where it cuts, after its results. *)
       trim_after_confirm
         m
         { point with message = last.message; trim_after = last.trim_after }
     | Some point, Some _ when same_time time point ->
       error
         m
         (if List.for_all points ~f:(fun p -> p.index <= last)
          then
            "That is the newest message in the model's context: there is \
             nothing after it to trim."
          else
            "Trimming there would part a tool call from its result: /trim \
             after lists where you can trim.")
     | _ -> error m (not_in_context `Trim))
;;

let trim_after (m : Model.t) =
  match m.dialog with
  | Some (Trim_after_confirm ({ busy = false; point; _ } as d)) ->
    ( { m with
        dialog = Some (Trim_after_confirm { d with busy = true; error = None })
      }
    , [ call (Trim_after { after = point.message }) ~tag:Trimmed_after ] )
  | _ -> m, []
;;

let trimmed_after (m : Model.t) result =
  match result with
  | Ok json ->
    decode m json Hidden.of_json ~f:(fun (h : Hidden.t) ->
      let m, close =
        match m.dialog with
        | Some (Trim_after_confirm { busy = true; _ }) -> close_dialog m
        | _ -> m, []
      in
      let m, cmds =
        context_toast
          m
          (sprintf
             "Trimmed %d message%s (%s): Undo is on the divider"
             h.messages
             (if h.messages = 1 then "" else "s")
             (Trim.tokens_label h.tokens))
      in
      let m, reveal =
        reveal_hidden
          { m with
            trimmed = { m.trimmed with reveal_hidden = Some (h.entry, None) }
          }
      in
      m, close @ cmds @ reveal)
  | Error e ->
    (match m.dialog with
     | Some (Trim_after_confirm ({ busy = true; _ } as d)) ->
       ( { m with
           dialog =
             Some
               (Trim_after_confirm
                  { d with busy = false; error = Some (String.capitalize e) })
         }
       , [] )
     | _ -> error m (String.capitalize e))
;;

let show_hidden (m : Model.t) ~entry ~show =
  match session_path m, show, Trimmed.range m.trimmed entry with
  | Some path, true, Folded ->
    ( { m with
        trimmed =
          { m.trimmed with
            ranges = Map.set m.trimmed.ranges ~key:entry ~data:Loading
          }
      }
    , [ call (Get_hidden { entry }) ~tag:(Hidden_messages { path; entry }) ] )
  | _, false, _ ->
    ( { m with
        trimmed = { m.trimmed with ranges = Map.remove m.trimmed.ranges entry }
      }
    , [] )
  | _ -> m, []
;;

let hidden_messages (m : Model.t) ~path ~entry result =
  match Trimmed.range m.trimmed entry, result with
  | Loading, Ok json when is_current m path ->
    decode m json Trim.trimmed_of_json ~f:(fun messages ->
      ( { m with
          trimmed =
            { m.trimmed with
              ranges =
                Map.set
                  m.trimmed.ranges
                  ~key:entry
                  ~data:(Shown (Chat.of_messages messages))
            }
        }
      , [] ))
  | Loading, Error e when is_current m path ->
    error
      { m with
        trimmed = { m.trimmed with ranges = Map.remove m.trimmed.ranges entry }
      }
      (String.capitalize e)
  | _ -> m, []
;;

let run_command (m : Model.t) ({ name; rest } : Slash.Parsed.t) =
  let none = "" in
  let words =
    String.split rest ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
  in
  match name, rest with
  | "help", "" -> open_dialog m Help
  | "help", name -> help_command m name
  | "hotkeys", _ -> open_dialog m Hotkeys
  | "settings", _ -> open_settings m
  | "nix-gc", rest when superuser m -> nix_gc_command m rest
  | "new", _ -> starting m, [ rpc "new_session" [] ~tag:Reload_state ]
  | "model", "" -> open_picker m (model_picker m)
  | "model", query ->
    (match Prigh_ui.Model_match.resolve m.models query with
     | Found model -> set_model m model.key
     | Ambiguous _ -> open_picker m (model_picker ~query m)
     | Not_found suggestions ->
       error
         m
         (sprintf
            "No model matches “%s”.%s Ctrl+L lists them all."
            query
            (match suggestions with
             | [] -> none
             | l ->
               " Did you mean "
               ^ String.concat
                   ~sep:", "
                   (List.map l ~f:(fun (s : Llm.t) -> s.name))
               ^ "?")))
  | "scoped-models", _ ->
    if List.is_empty m.models
    then error m "No models yet: /login logs in to a provider."
    else open_picker m (scoped_models_dialog m)
  | "fallback", "" -> with_config m ~f:(fun c -> toast m (fallback_summary m c))
  | "fallback", "off" -> set_fallback m []
  | "fallback", _ -> set_fallback m words
  | "thinking", "" -> open_thinking_picker m
  | "thinking", level ->
    if List.mem thinking_levels level ~equal:String.equal
    then set_thinking m level
    else
      error
        m
        (sprintf
           "Unknown thinking level “%s”: use %s."
           level
           (String.concat ~sep:", " thinking_levels))
  | "change_default", _ -> m, [ rpc "change_default" [] ~tag:Default_saved ]
  | "theme", "" -> open_picker m (theme_picker m)
  | "theme", name -> theme_named m name
  | "accent", "" -> open_picker m (accent_picker m)
  | "accent", name -> accent_named m name
  | "filter", "" -> open_filter m
  | "filter", args ->
    (match Transcript_filter.parse m.transcript_filter args with
     | Ok filter -> set_filter m filter
     | Error e -> error m e)
  | "verbosity", "" -> open_picker m (verbosity_picker m)
  | "verbosity", name ->
    (match
       List.find Prigh_ui.Verbosity.all ~f:(fun v ->
         String.equal (Prigh_ui.Verbosity.name v) name)
     with
     | Some v -> set_verbosity m v
     | None ->
       error
         m
         (sprintf "Unknown verbosity “%s”: use quiet, normal or verbose." name))
  | "confirm", "" -> open_picker m (confirm_picker m)
  | "mode", "" -> mode_picker m
  | "mode", name -> mode_by_name m name
  | "plan", arg -> plan_mode m arg
  | "todos", _ -> show_plan m
  | "permissions", _ -> show_permissions m
  | "confirm", ("on" | "true") -> set_confirm m true
  | "confirm", ("off" | "false") -> set_confirm m false
  | "confirm", other ->
    error m (sprintf "Unknown setting “%s”: use on or off." other)
  | "budget", "" ->
    current_state m ~f:(fun s ->
      toast m (String.capitalize (Prigh_ui.Loop_status.describe_budget s)))
  | "budget", arg ->
    (match Prigh_ui.Loop_status.parse_budget arg with
     | Error e -> error m (String.capitalize e)
     | Ok usd ->
       ( m
       , [ rpc
             "set_budget"
             [ "usd", Option.value_map usd ~default:`Null ~f:Json.float ]
             ~tag:
               (Notice
                  (match usd with
                   | None -> "Budget removed: turns start whatever it costs"
                   | Some usd ->
                     sprintf
                       "Budget set to $%.2f: no turn starts once the session \
                        has cost that"
                       usd))
         ] ))
  | "goal", "" ->
    current_state m ~f:(fun s ->
      toast m (String.capitalize (Prigh_ui.Loop_status.describe_goal s)))
  | "goal", "off" -> m, [ rpc "clear_goal" [] ~tag:(Notice "Goal cleared") ]
  | "goal", "resume" -> m, [ rpc "resume_goal" [] ~tag:(Notice "Goal resumed") ]
  | "goal", text ->
    (* Refused (the budget is spent), it goes back into the editor. *)
    ( m
    , [ rpc
          "set_goal"
          [ "text", str text ]
          ~tag:(Sent { text = "/goal " ^ text; images = [] })
      ] )
  | "resume", "" ->
    current_state m ~f:(fun s ->
      toast
        m
        (String.capitalize
           (Prigh_ui.Loop_status.describe_resume s ~utc_offset:m.utc_offset)))
  | "resume", "now" -> m, [ rpc "resume_now" [] ~tag:(Notice "Resuming now") ]
  | "resume", "cancel" ->
    ( m
    , [ rpc
          "cancel_resume"
          []
          ~tag:(Notice "Automatic resume cancelled: send a message to retry")
      ] )
  | "resume", other ->
    error
      m
      (sprintf
         "Unknown /resume argument “%s”: /resume now retries at once, /resume \
          cancel stays stopped."
         other)
  | "compact", instructions ->
    let m, cmds = toast m "Compacting the conversation…" in
    ( m
    , cmds
      @ [ rpc
            "compact"
            (if String.is_empty instructions
             then []
             else [ "instructions", str instructions ])
            ~tag:(Notice "Compacted the conversation")
        ] )
  | "diff", path ->
    let m, cmds = toast m reading_changes in
    ( m
    , cmds
      @ [ rpc
            "git_diff"
            (if String.is_empty path then [] else [ "path", str path ])
            ~tag:Git_diff
        ] )
  | "review", "" -> review_worktree m ~dir:None ~base:None
  | "review", args ->
    review_action
      m
      ~command:(String.strip ("/review " ^ args))
      (Review_command.parse args)
  | "init", _ -> m, [ rpc "init" [] ~tag:(Command_prompt "/init") ]
  | "notify", "" -> toast m (Page_notify.describe m.notifications)
  | "notify", "on" -> set_notifications m true
  | "notify", "off" -> set_notifications m false
  | "notify", other ->
    error
      m
      (sprintf
         "Unknown /notify argument “%s”: /notify on or /notify off (/notify \
          alone says what is on)"
         other)
  | "name", "" -> open_rename m
  | "name", name ->
    m, [ rpc "set_session_name" [ "name", str name ] ~tag:Refresh_sessions ]
  | "session", _ ->
    loading m "Session" [ rpc "session_stats" [] ~tag:Session_stats ]
  | "sessions", _ | "switch", "" -> open_sessions m
  | (("archive" | "unarchive") as name), key ->
    let archived = String.equal name "archive" in
    let session =
      if String.is_empty key
      then (
        let path =
          Option.value_map m.state ~default:"" ~f:(fun s -> s.session_path)
        in
        match find_session_by_path m path with
        | Some s -> Ok s
        | None -> Error "This session isn't saved yet: send a message first.")
      else find_session m key
    in
    (match session with
     | Ok s -> set_archived m s ~archived
     | Error e -> prompt_failed m e)
  | "delete", "" ->
    if Model.running m
    then error m (running_error `Delete)
    else
      loading
        m
        "Delete a message"
        [ call Get_message_points ~tag:(Message_points Delete_picker) ]
  | "delete", "undo" -> undelete m
  | "delete", keys ->
    (match
       String.split keys ~on:' '
       |> List.filter ~f:(Fn.non String.is_empty)
       |> List.map ~f:(find_session m)
       |> Result.all
     with
     | Ok sessions -> ask_delete m (List.map sessions ~f:(fun s -> s.path))
     | Error e -> prompt_failed m e)
  | "delete-archived", _ -> ask_delete_archived m
  | "switch", path -> switch_session m path
  | "clone", _ -> starting m, [ rpc "clone" [] ~tag:Reload_state ]
  | "fork", _ ->
    loading m "Fork from" [ rpc "get_entries" [] ~tag:(Entries Fork) ]
  | "rewind", _ ->
    loading m "Rewind to" [ rpc "get_entries" [] ~tag:(Entries Rewind) ]
  | "trim", "" ->
    loading
      m
      "Trim the context"
      [ call Get_trim_points ~tag:(Trim_points Picker) ]
  | "trim", "undo" -> untrim_latest m
  | "trim", "after" ->
    if Model.running m
    then error m (running_error `Trim)
    else
      loading
        m
        trim_after_title
        [ call Get_message_points ~tag:(Message_points Trim_after_picker) ]
  | "trim", _ ->
    error
      m
      "Usage: /trim (pick the message to trim before), /trim after (pick the \
       message to trim after) or /trim undo"
  | "tree", _ ->
    loading
      m
      "Session tree"
      [ rpc "get_entries" [ "all", Json.bool true ] ~tag:(Entries Tree) ]
  | "cd", "" ->
    open_prompt
      m
      (Prompt.create
         ~input:(Option.value_map m.state ~default:"" ~f:(fun s -> s.cwd))
         Cd)
  | "cd", path -> m, [ change_dir ~target:Cwd path ]
  (* Typing names a folder in the working directory. *)
  | "mkdir", "" ->
    open_prompt
      m
      (Prompt.create
         ~input:
           (Option.value_map m.state ~default:"" ~f:(fun s ->
              if String.is_suffix s.cwd ~suffix:"/" then s.cwd else s.cwd ^ "/"))
         Mkdir)
  | "mkdir", path -> m, [ change_dir ~if_missing:Create ~target:Cwd path ]
  | "worktree", "new" -> new_worktree_dialog m
  | "worktree", rest -> worktree_command m rest
  | "default-dir", "" ->
    with_config m ~f:(fun c -> toast m (default_dir_summary c))
  | "default-dir", "off" -> set_default_dir m None
  | "default-dir", dir ->
    (match default_dir m dir with
     | Ok dir -> set_default_dir m (Some dir)
     | Error message -> error m message)
  | "compaction", rest -> compaction_command m rest
  | "host", "" -> hosts_picker m
  | "host", arg -> switch_host m arg
  | "export", "" -> open_prompt m (Prompt.create Export)
  | "export", path -> export m path
  | "import", "" -> open_prompt m (Prompt.create Import)
  | "import", path -> m, [ rpc "import" [ "path", str path ] ~tag:Imported ]
  | ("skills" | "skill:"), query -> open_skills m ~query
  | "remind", rest ->
    remind_action
      m
      ~command:(String.strip ("/remind " ^ rest))
      (Prigh_ui.Remind_command.run ~reminders:(reminders m) rest)
  | "snippet", rest ->
    snippet_action
      m
      (Prigh_ui.Snippet_command.run
         ~snippets:(snippets m)
         ~last_prompt:(last_prompt m)
         rest)
  | "mcp", "" ->
    loading
      m
      "MCP servers"
      [ call (List_mcp { reconnect = false }) ~tag:(Mcp Picker) ]
  | "mcp", "reconnect" ->
    let m, cmds = toast m "Restarting the failed MCP servers…" in
    m, cmds @ [ call (List_mcp { reconnect = true }) ~tag:(Mcp Reconnect) ]
  | "mcp", other ->
    error
      m
      (sprintf
         "Unknown /mcp argument “%s”: /mcp lists the servers, /mcp reconnect \
          restarts the failed ones."
         other)
  | "copy", _ -> copy_last m
  | "btw", "" -> error m "Usage: /btw <question> (asked aside; the run goes on)"
  | "btw", question -> start_btw m question
  | "abort", _ -> m, [ rpc "abort" [] ~tag:Restored ]
  | "files", "" -> with_files m (Files.update m.files (Open None))
  | "files", path -> with_files m (Files.update m.files (Open (Some path)))
  | "terminal", "" -> open_terminal (with_terminal_dir m None)
  | "terminal", dir -> open_terminal (with_terminal_dir m (Some dir))
  | "agents", "" -> open_agents m None
  | "agents", _ ->
    (match words with
     | ("steer" | "follow-up") :: _ -> agents_message m rest
     | [ "cancel"; arg ] -> with_item m arg ~f:stop_command
     | [ "cancel" ] ->
       error m "Usage: /agents cancel <n|id> (/agents lists them)"
     | _ -> open_agents m (Some rest))
  | "jobs", "" -> open_agents m None
  | "jobs", _ ->
    (match words with
     | [ "kill"; arg ] -> with_job m arg ~f:stop_command
     | [ arg ] when not (String.equal arg "kill") -> with_job m arg ~f:select
     | _ -> error m "Usage: /jobs [id | kill <id>] (/jobs lists them)")
  | "login", "" ->
    loading m "Log in" [ rpc "auth_status" [] ~tag:(Auth_status Login_picker) ]
  | "login", _ ->
    (match Forges.parse_login m.forges words with
     | Provider { provider; method_ } -> start_login m ~provider ~method_
     | Forge { host; kind; method_; again } ->
       forge_login m ~host ~kind ~method_ ~again
     | Ask_host -> forge_host_dialog m
     | Bad usage -> error m usage)
  | "logout", "" ->
    loading
      m
      "Log out"
      [ rpc "auth_status" [] ~tag:(Auth_status Logout_picker) ]
  | "logout", arg ->
    (match Forges.logout_host m.forges arg with
     | Some host -> forge_logout m host
     | None -> logout m arg)
  | "auth", _ ->
    loading
      m
      "Providers"
      [ rpc "auth_status" [] ~tag:(Auth_status Show)
      ; call Forge_status ~tag:(Forge Status)
      ]
  | "pr", rest -> pr_command m rest
  | "setusr", "" ->
    loading m "Act as" [ rpc "list_users" [] ~tag:(Users Picker) ]
  | "setusr", user ->
    (match words with
     | [ user ] -> act_as m user
     | _ -> error m (sprintf "Usage: /setusr [user] (not “%s”)" user))
  | "signout", _ -> m, [ Command.Sign_out ]
  | "retry-backend-connection", _ -> retry_connection m
  | "state", _ ->
    current_state m ~f:(fun state ->
      open_dialog
        m
        (Text
           { title = "Session state"
           ; text = Sexp.to_string_hum (State.sexp_of_t state)
           }))
  | "clear", _ ->
    toast
      { m with chat = Chat.empty; paging = Paging.all }
      "Cleared the view; the conversation is kept (a reload shows it, /new \
       starts afresh)"
  | "quit", _ ->
    toast
      m
      "Close the tab to leave: the session stays in the backend (/signout \
       signs out)."
  | name, _ ->
    error
      m
      (match Slash.closest ~superuser:(superuser m) name with
       | Some spec ->
         sprintf
           "Unknown command /%s. Did you mean /%s? (/help lists them)"
           name
           spec.name
       | None -> sprintf "Unknown command /%s: /help lists the commands." name)
;;

(* What the agent waits on the user for: confirmations first, then the
   model's questions, one dialog at a time. What is shown or queued already
   is not added again ([get_pending] after the events). *)
let same_confirm (a : Pending.Confirm.t) (b : Pending.Confirm.t) =
  String.equal a.call_id b.call_id
  && Option.equal String.equal a.agent_id b.agent_id
;;

let focus_waiting (m : Model.t) =
  match m.confirms, m.questions with
  | _ :: _, _ ->
    Command.Focus
      (if Option.is_some m.confirm_reason then "confirm-reason" else "confirm")
  | [], q :: _ -> Command.Focus (Question_dialog.focus q)
  | [], [] ->
    (match m.dialog with
     | Some _ -> Command.Focus "dialog"
     | None -> focus_editor)
;;

let add_waiting (m : Model.t) ~confirms ~questions =
  let before = focus_waiting m in
  let m =
    { m with
      confirms =
        m.confirms
        @ List.filter confirms ~f:(fun c ->
          not (List.exists m.confirms ~f:(same_confirm c)))
    ; questions =
        m.questions
        @ List.filter_map questions ~f:(fun (q : Pending.Ask.t) ->
          if
            List.exists m.questions ~f:(fun (shown : Question_dialog.t) ->
              String.equal shown.call_id q.call_id)
          then None
          else Some (Question_dialog.create q))
    }
  in
  let after = focus_waiting m in
  m, if Command.equal before after then [] else [ after ]
;;

(* Answered (here, in another frontend) or withdrawn. *)
let remove_confirm (m : Model.t) ~call_id ~agent_id =
  let is_it (c : Pending.Confirm.t) =
    String.equal c.call_id call_id
    && Option.equal String.equal c.agent_id agent_id
  in
  match m.confirms with
  | first :: _ when is_it first ->
    let m =
      { m with
        confirms = List.filter m.confirms ~f:(Fn.non is_it)
      ; confirm_reason = None
      }
    in
    m, [ focus_waiting m ]
  | _ -> { m with confirms = List.filter m.confirms ~f:(Fn.non is_it) }, []
;;

let remove_question (m : Model.t) call_id =
  let is_it (q : Question_dialog.t) = String.equal q.call_id call_id in
  match m.questions with
  | first :: _ when is_it first && List.is_empty m.confirms ->
    let m = { m with questions = List.filter m.questions ~f:(Fn.non is_it) } in
    m, [ focus_waiting m ]
  | _ -> { m with questions = List.filter m.questions ~f:(Fn.non is_it) }, []
;;

let answer_confirm (m : Model.t) ~call_id ~agent_id answer ~reason =
  match
    List.find m.confirms ~f:(fun c ->
      String.equal c.call_id call_id
      && Option.equal String.equal c.agent_id agent_id)
  with
  | None -> m, []
  | Some c ->
    let m, focus = remove_confirm m ~call_id ~agent_id in
    let m, toast_cmds =
      toast m (String.capitalize (Confirm_prompt.acknowledgement c answer))
    in
    ( m
    , [ rpc
          "tool_confirm_respond"
          (Confirm_prompt.params c answer ~reason)
          ~tag:Answer
      ]
      @ focus
      @ toast_cmds )
;;

let question (m : Model.t) (action : Question_dialog.Action.t) =
  match m.confirms, m.questions with
  | [], q :: rest ->
    (match Question_dialog.update q action with
     | `Show shown ->
       let focus =
         if String.equal (Question_dialog.focus q) (Question_dialog.focus shown)
         then []
         else [ Command.Focus (Question_dialog.focus shown) ]
       in
       { m with questions = shown :: rest }, focus
     | `Answer answers ->
       let m = { m with questions = rest } in
       let m, toast_cmds = toast m "Answered" in
       ( m
       , [ rpc
             "ask_user_respond"
             (Question_dialog.answer_params q answers)
             ~tag:Answer
         ; focus_waiting m
         ]
         @ toast_cmds )
     | `Decline ->
       let m = { m with questions = rest } in
       let m, toast_cmds =
         toast m "Declined to answer: the model goes on without one"
       in
       ( m
       , [ rpc "ask_user_respond" (Question_dialog.decline_params q) ~tag:Answer
         ; focus_waiting m
         ]
         @ toast_cmds ))
  | _ -> m, []
;;

(* The agents panel says so, with a retry. *)
let transcript_failed (m : Model.t) id e =
  if Map.mem m.agents.fetches id
  then
    with_agents m ~f:(fun a ->
      { a with fetches = Map.set a.fetches ~key:id ~data:(Failed e) })
  else m
;;

(* A [get_subagent] reply. *)
let subagent_of_json json =
  Or_error.map (Subagent_detail.of_json json) ~f:(fun (d : Subagent_detail.t) ->
    ( { Chat.Subagent.agent_id = d.id
      ; task = d.task
      ; model = d.model
      ; forked = false
      ; chat = Chat.of_messages ~running:(Option.is_none d.result) d.messages
      ; turns = d.turns
      ; cost_usd = None
      ; result = d.result
      }
    , `Parent d.parent
    , `Call d.call_id ))
;;

(* ---- pages of the transcript, cached transcripts, whole outputs *)

let prefetch_next (m : Model.t) =
  let chat_cache, next = Chat_cache.next_prefetch m.chat_cache in
  ( { m with chat_cache }
  , Option.to_list (Option.map next ~f:(fun path -> page_rpc path ~before:None))
  )
;;

(* A cached transcript on show stays (with the older pages loaded and what
   events added since): a newer page than the cached one is merged into it
   like after reconnecting. *)
let newest_page (m : Model.t) path (page : Message_page.t) =
  let current = is_current m path in
  (* Not asked for by this page any more (e.g. by the user we were). *)
  if not (current || Chat_cache.loading m.chat_cache path)
  then m, []
  else (
    let chat_cache, entry, cached =
      Chat_cache.add_page
        (Chat_cache.set_loading m.chat_cache path false)
        path
        page
        ~running:(current && Model.running m)
    in
    let m, prefetch = prefetch_next { m with chat_cache } in
    let refreshing = m.pending.refreshing in
    let m =
      if current
      then { m with pending = { m.pending with refreshing = false } }
      else m
    in
    match current, m.state, cached with
    | false, _, _ | true, None, _ -> m, prefetch
    | true, Some _, `Unchanged when refreshing -> m, prefetch
    | true, Some state, `Changed when refreshing ->
      m, resync_rpc m state :: prefetch
    | true, Some _, _
      when phys_equal entry.chat m.chat && not m.pending.messages -> m, prefetch
    | true, Some _, (`Unchanged | `Changed | `New) ->
      let m, shown =
        refetch_shown
          { m with
            chat = entry.chat
          ; paging = entry.paging
          ; scrolled_up = false
          ; pending = { m.pending with messages = false }
          ; trimmed = { m.trimmed with deleting = Int.Set.empty }
          }
      in
      ( m
      , (Command.Scroll_to_bottom
         :: List.map (Chat.subagents_to_load entry.chat) ~f:subagent_rpc)
        @ shown
        @ prefetch ))
;;

(* ---- jumping around the transcript *)

(* For the pages not loaded, whenever some are not. *)
let fetch_outline (m : Model.t) =
  match session_path m with
  | Some path
    when m.paging.older > 0
         && (not (Option.equal String.equal m.jump.fetching (Some path)))
         && not
              (Option.exists m.jump.outline ~f:(fun (of_, _) ->
                 String.equal of_ path)) ->
    ( { m with jump = { m.jump with fetching = Some path } }
    , [ call (Get_outline { session = path }) ~tag:(Outline path) ] )
  | _ -> m, []
;;

let jump_points (m : Model.t) =
  Jump.points
    ~deleting:m.trimmed.deleting
    m.jump
    ~path:(session_path m)
    ~paging:m.paging
    ~trim:(trim_state m)
    m.chat
;;

let no_jump (m : Model.t) = { m with jump = { m.jump with pending = None } }

(* A message not loaded: the pages from it down to those loaded, in one go,
   then there. While a page is on its way, once it is in. The divider of what
   is trimmed is above the first message. *)
let jump_to (m : Model.t) (target : Jump.Target.t) =
  let load ~index =
    match session_path m, m.paging.before with
    | Some path, Some before when index < m.paging.older ->
      let m = { m with jump = { m.jump with pending = Some target } } in
      if m.paging.loading
      then m, []
      else
        ( { m with paging = { m.paging with loading = true } }
        , [ call
              (Get_messages_page
                 { session = path
                 ; before = Some before
                 ; from = None
                 ; limit = m.paging.older - index
                 })
              ~tag:(Page { path; before = Some before })
          ] )
    | _ -> no_jump m, []
  in
  match target with
  | Entry n -> no_jump m, [ Command.Scroll_to_entry n ]
  | Message index -> load ~index
  | Trimmed when m.paging.older > 0 -> load ~index:0
  | Trimmed -> no_jump m, [ Command.Scroll_to_trimmed ]
;;

(* An older page is in: there, if it was what a jump waited for. *)
let continue_jump (m : Model.t) (page : Message_page.t) =
  match m.jump.pending with
  | None | Some (Entry _) -> m, []
  | Some (Message index) ->
    (match Jump.entry_of_index ~page ~first:(Chat.first m.chat) index with
     | Some n -> no_jump m, [ Command.Scroll_to_entry n ]
     | None -> jump_to m (Message index))
  | Some Trimmed -> jump_to m Trimmed
;;

let older_page (m : Model.t) path ~before (page : Message_page.t) =
  if
    is_current m path
    && m.paging.loading
    && Option.equal String.equal m.paging.before (Some before)
  then (
    let loaded = String.Set.of_list (Chat.subagents_to_load m.chat) in
    let chat = Chat.prepend m.chat page.messages in
    let m, jump =
      continue_jump { m with chat; paging = Paging.of_page page } page
    in
    ( m
    , (Command.Keep_scroll_position
       :: List.filter_map (Chat.subagents_to_load chat) ~f:(fun call_id ->
         Option.some_if (not (Set.mem loaded call_id)) (subagent_rpc call_id)))
      @ jump ))
  else m, []
;;

let load_older (m : Model.t) =
  match session_path m, Paging.next m.paging with
  | Some path, Some before ->
    ( { m with paging = { m.paging with loading = true } }
    , [ page_rpc path ~before:(Some before) ] )
  | _ -> m, []
;;

(* Ctrl+↑ above the first of the user's messages loaded: the one before it,
   loading it; without the outline, the page before. *)
let jump_older (m : Model.t) =
  match
    List.rev (jump_points m)
    |> List.find ~f:(fun (p : Jump.Point.t) ->
      match p.target, p.kind with
      | Message _, User -> true
      | _ -> false)
  with
  | Some p -> jump_to m p.target
  | None -> load_older m
;;

(* ---- trimming the context before a message *)

let trim_title = "Trim the context"

(* "Trim everything before this": the backend says what that drops. A
   message on the page is found by its place in the context; one in the
   outline, not loaded, by its place among the user's messages. *)
let first_in_context m =
  toast
    m
    "That is the first message in the model's context: there is nothing before \
     it to trim."
;;

let ask_trim (m : Model.t) (target : Jump.Target.t) =
  let points = jump_points m in
  let on_page =
    match target with
    | Entry n ->
      Option.both (Message_index.span (Model.message_index m) n) (entry_at m n)
    | Message _ | Trimmed -> None
  in
  match on_page with
  | Some ({ first = 0; _ }, _) -> first_in_context m
  | Some ((span : Message_index.Span.t), at) ->
    loading
      m
      trim_title
      [ call
          Get_message_points
          ~tag:(Message_points (Trim_before { index = span.first; at }))
      ]
  | None ->
    (match Jump.from_end points target with
     | Some (from_end, p) when Jump.trimmable ~paging:m.paging m.chat p ->
       loading
         m
         trim_title
         [ call
             Get_trim_points
             ~tag:(Trim_points (Before { from_end; at = p.at }))
         ]
     | Some _ -> first_in_context m
     | None -> m, [])
;;

let trim_picker (m : Model.t) (points : Trim.Point.t list) =
  let times = Model.message_time m in
  Picker.create
    ~title:"Trim the context before"
    (List.rev_map points ~f:(fun (p : Trim.Point.t) ->
       let text = Jump.headline p.text in
       Picker.Item.create
         ~id:p.before
         ~detail:
           (String.concat
              ~sep:"  "
              ([ (if p.reply then "reply" else "you") ]
               @ Option.to_list (Option.map p.at ~f:(Message_time.short times))
               @ [ Trim.label ~messages:p.messages ~tokens:p.tokens ]))
         ~search:text
         (if String.is_empty text then "(an image)" else text)))
;;

let trim_confirm (m : Model.t) point =
  open_dialog m (Trim_confirm { point; busy = false; error = None })
;;

let trim_points_arrived (m : Model.t) (purpose : Trim_purpose.t) points =
  match purpose, points with
  | Picker, [] ->
    toast
      m
      "Nothing to trim: the context has no message after its first. /trim \
       drops what comes before a later one."
  | Picker, points ->
    open_picker m (Picker { kind = Trim points; picker = trim_picker m points })
  | Before { from_end; at }, points ->
    (match
       List.nth
         (List.rev
            (List.filter points ~f:(fun (p : Trim.Point.t) -> not p.reply)))
         from_end
     with
     | Some (point : Trim.Point.t)
       when Option.is_none at
            || Option.is_none point.at
            || [%equal: Time_ns.t option] at point.at -> trim_confirm m point
     | _ ->
       error
         m
         "That message is not in the model's context any more: /trim lists \
          those you can trim before.")
;;

let trim (m : Model.t) =
  match m.dialog with
  | Some (Trim_confirm ({ busy = false; point; _ } as d)) ->
    ( { m with
        dialog = Some (Trim_confirm { d with busy = true; error = None })
      ; trimmed = { m.trimmed with reveal = Some point.before }
      }
    , [ call (Trim { before = point.before }) ~tag:Trimmed ] )
  | _ -> m, []
;;

let trimmed (m : Model.t) result =
  match result with
  | Ok json ->
    decode m json Trim.Freed.of_json ~f:(fun (freed : Trim.Freed.t) ->
      let m, close =
        match m.dialog with
        | Some (Trim_confirm { busy = true; _ }) -> close_dialog m
        | _ -> m, []
      in
      let m, cmds =
        context_toast
          m
          (sprintf
             "Trimmed %d message%s (%s): Undo is on the divider"
             freed.messages
             (if freed.messages = 1 then "" else "s")
             (Trim.tokens_label freed.tokens))
      in
      m, close @ cmds)
  | Error e ->
    let m = { m with trimmed = { m.trimmed with reveal = None } } in
    (match m.dialog with
     | Some (Trim_confirm ({ busy = true; _ } as d)) ->
       ( { m with
           dialog =
             Some
               (Trim_confirm
                  { d with busy = false; error = Some (String.capitalize e) })
         }
       , [] )
     | _ -> error m (String.capitalize e))
;;

let show_trimmed (m : Model.t) show =
  match session_path m, trim_state m, show, m.trimmed.shown with
  | Some path, Some _, true, Folded ->
    ( { m with trimmed = { m.trimmed with shown = Loading } }
    , [ call Get_trimmed ~tag:(Trimmed_messages path) ] )
  | _, _, false, _ -> { m with trimmed = { m.trimmed with shown = Folded } }, []
  | _ -> m, []
;;

let trimmed_messages (m : Model.t) path result =
  match m.trimmed.shown, result with
  | Loading, Ok json when is_current m path ->
    decode m json Trim.trimmed_of_json ~f:(fun messages ->
      ( { m with
          trimmed = { m.trimmed with shown = Shown (Chat.of_messages messages) }
        }
      , [ Command.Keep_scroll_position ] ))
  | Loading, Error e when is_current m path ->
    error
      { m with trimmed = { m.trimmed with shown = Folded } }
      (String.capitalize e)
  | _ -> m, []
;;

(* The transcript reloaded after this page trimmed the context: the divider
   it starts with, the trimmed message just below it. *)
let reveal_trimmed (m : Model.t) path =
  match m.trimmed.reveal, trim_state m with
  | Some before, Some trim
    when is_current m path && String.equal before trim.before ->
    jump_to { m with trimmed = { m.trimmed with reveal = None } } Trimmed
  | _ -> m, []
;;

let prefetch_session (m : Model.t) path =
  if is_current m path
  then m, []
  else (
    let chat_cache, fetch = Chat_cache.prefetch m.chat_cache path in
    ( { m with chat_cache }
    , Option.to_list
        (Option.map fetch ~f:(fun path -> page_rpc path ~before:None)) ))
;;

(* A fold's key is the call id, after the subagent's id and a slash in a
   subagent's transcript. *)
let output_key ~agent_id ~call_id =
  match agent_id with
  | None -> call_id
  | Some agent_id -> Fold.key (Fold.scoped Fold.default agent_id) call_id
;;

let load_output (m : Model.t) key =
  let agent_id, call_id =
    match String.rsplit2 key ~on:'/' with
    | Some (agent_id, call_id) -> Some agent_id, call_id
    | None -> None, key
  in
  match Chat.find_tool m.chat ~agent_id ~call_id with
  | Some { result = Some { elided = Some _; _ }; _ }
    when not (Set.mem m.outputs_loading key) ->
    ( { m with outputs_loading = Set.add m.outputs_loading key }
    , [ call
          (Get_tool_output { call_id; agent_id })
          ~tag:(Tool_output { agent_id; call_id })
      ] )
  | _ -> m, []
;;

let start_commands (m : Model.t) =
  let probe =
    (* Only superusers may list the users: the account menu offers them. *)
    match own_user m with
    | Some _ -> [ rpc "list_users" [] ~tag:(Users Probe) ]
    | None -> []
  in
  (if m.narrow then [] else [ focus_editor ]) @ startup @ fetch_prefs m @ probe
;;

(* The state of a session clicked before the one being opened (A, then B
   and C at once: B's arrives first). *)
let superseded (m : Model.t) (state : State.t) =
  match m.pending.session with
  | Some (Switching path) ->
    not (String.equal path state.session_path || is_current m state.session_path)
  | Some Starting | None -> false
;;

(* The user's messages on show that start with [text] (the backend adds
   the attached files after it). *)
let user_messages chat text =
  List.count (Chat.entries chat) ~f:(function
    | User u -> String.is_prefix u.text ~prefix:text
    | Assistant _ | Shell _ | Compaction _ -> false)
;;

(* Back after the connection went while sending: what did not reach the
   agent comes back to the editor. *)
let restore_unsure (m : Model.t) ~delivered =
  let lost =
    List.filter m.pending.unsure ~f:(fun u -> not (delivered u))
    |> List.map ~f:(fun (u : In_flight.Unsure.t) -> u.prompt)
  in
  let m = { m with pending = { m.pending with unsure = [] } } in
  match lost with
  | [] -> m, []
  | { text; images } :: _ ->
    if String.is_empty m.draft && List.is_empty m.images
    then
      error
        (set_draft { m with images } text)
        "The connection went before your message reached the agent: it is back \
         in the editor."
    else
      error
        m
        "The connection went before your message reached the agent: ↑ brings \
         it back."
;;

(* Back in touch with the backend. Nothing on show is rebuilt: the session's
   messages are fetched again and merged into the transcript (which keeps its
   place), the rest is asked for again. *)
module Rejoin = struct
  let refused e = String.is_prefix e ~prefix:"unauthorised"

  let refused_message e =
    sprintf
      "The backend no longer accepts this sign-in (%s): sign in again to go \
       back to the session."
      e
  ;;

  let connected (m : Model.t) json =
    let m = { m with connection = Connected } in
    match Hello_reply.of_json json with
    | Ok hello -> { m with hello = Some hello }
    | Error _ -> m
  ;;

  (* "Reconnected" waits for the session to be back ([settle]). *)
  let rejoined (m : Model.t) json =
    let m = connected m json in
    let m =
      { m with
        drops = Drops.rejoined m.drops ~resyncing:(Option.is_some m.state)
      }
    in
    match m.state with
    | None -> m, start_commands m
    | Some state ->
      ( m
      , startup
        @ fetch_prefs m
        @ [ resync_rpc m state
          ; rpc "get_pending" [] ~tag:Resync_pending
          ; rpc "list_sessions" [] ~tag:Sessions
          ; list_subagents
          ; list_jobs
          ] )
  ;;

  (* An empty session was never saved: the new one the backend gave us does
     as well. *)
  let gone (m : Model.t) json ~session =
    let m = connected m json in
    (* The banner says what became of the session: no "Reconnected". *)
    let m =
      { m with
        drops =
          { (Drops.rejoined m.drops ~resyncing:false) with rejoined = false }
      }
    in
    let m, restored = restore_unsure m ~delivered:(fun _ -> false) in
    if List.is_empty (Chat.entries m.chat)
    then { m with state = None }, restored @ start_commands m
    else
      (* Nothing of it is live any more: what it waited for, queued, ran. *)
      ( { m with
          session_gone = Some session
        ; state =
            Option.map m.state ~f:(fun s ->
              { s with running = false; subagents = []; jobs = [] })
        ; chat = Chat.set_running m.chat false
        ; confirms = []
        ; confirm_reason = None
        ; questions = []
        ; queue = Queued.empty
        ; agent_queues = []
        ; recipient = Main
        ; dialog = Option.filter m.dialog ~f:(Fn.non Dialog.per_session)
        ; btw = None
        ; agents = { Agents.empty with open_ = m.agents.open_ }
        ; pending = { m.pending with prompts = []; sending = 0 }
        }
      , restored
        @ [ rpc "list_models" [] ~tag:Models
          ; rpc "auth_status" [] ~tag:(Auth_status Refresh)
          ; rpc "get_config" [] ~tag:Config
          ; rpc "list_sessions" [] ~tag:Sessions
          ] )
  ;;

  (* The network is back or the page is on show again: try now (the backoff
     starts over), or check that the socket still works. *)
  let now (m : Model.t) =
    match m.connection with
    | Connected -> m, [ Command.Probe_connection ]
    (* It said that it stopped: the user chooses. *)
    | Reconnecting _ when Drops.gave_up m.drops -> m, []
    | Reconnecting _ ->
      let generation = m.generation + 1 in
      ( { m with
          connection = Reconnecting { attempt = m.drops.lost; generation }
        ; generation
        }
      , [ Command.Reconnect
            { generation
            ; delay_ms = 0
            ; session = Option.map m.state ~f:(fun s -> s.session_id)
            }
        ] )
  ;;
end

(* The backend's word for a page cursor no longer in the session. *)
let gone_from_session = "is not in the session any more"

(* Git hosting's errors as the backend says them: they start with what
   failed (often a host name) and say what to do. *)
let said = function
  | "backend closed" ->
    "The connection went before the backend answered: try again once it is \
     back."
  | e -> e
;;

let with_forges (m : Model.t) ~f = { m with forges = f m.forges }

let forge_reply (m : Model.t) (tag : Forge_reply.t) result =
  let here session = String.equal session (session_id m) in
  match tag, result, m.dialog with
  | Status, Ok json, _ ->
    decode m json Forge_account.list_of_json ~f:(fun accounts ->
      with_forges m ~f:(fun f -> { f with accounts = Loaded accounts }), [])
  | Status, Error "backend closed", _ -> m, []
  | Status, Error e, _ ->
    ( with_forges m ~f:(fun f ->
        match f.accounts with
        | Loaded _ -> f
        | Loading | Failed _ -> { f with accounts = Failed e })
    , [] )
  | Logout _, Ok _, _ -> m, []
  | Logout host, Error e, _ ->
    error
      (with_forges
         (drop_toast m (signing_out host))
         ~f:(fun f -> { f with signing_out = Set.remove f.signing_out host }))
      (said e)
  | List { session; filter }, result, Some (Pulls l)
    when here session && Pull_requests.Filter.equal l.filter filter ->
    let l =
      match result with
      | Ok json ->
        (match Pr_list.of_json json with
         | Ok list -> Pull_requests.loaded l list
         | Error e -> Pull_requests.failed l (Error.to_string_hum e))
      | Error e -> Pull_requests.failed l (said e)
    in
    { m with dialog = Some (Pulls l) }, []
  | View { session; number }, result, Some (Pull p)
    when here session && p.number = number ->
    let status : Pr_page.Status.t =
      match result with
      | Ok json ->
        (match Pr_detail.of_json json with
         | Ok d -> Loaded d
         | Error e -> Failed (Error.to_string_hum e))
      | Error e -> Failed (said e)
    in
    { m with dialog = Some (Pull { p with status }) }, []
  | Defaults session, result, Some (Pr_create f) when here session ->
    let f, focus =
      match result with
      | Ok json ->
        (match Pr_defaults.of_json json with
         | Ok d -> Pr_create.set_defaults f d, [ Command.Focus "pr-title" ]
         | Error e -> { f with failed = Some (Error.to_string_hum e) }, [])
      | Error e -> { f with failed = Some (said e) }, []
    in
    { m with dialog = Some (Pr_create f) }, focus
  | Created, Ok json, dialog ->
    decode m json Pr_detail.of_json ~f:(fun d ->
      let m, cmds =
        match dialog with
        | Some (Pr_create { busy = true; back = Some l; _ }) ->
          list_prs
            m
            (Pull_requests.create ~query:(Picker.query l.picker) l.filter)
        | Some (Pr_create { busy = true; back = None; _ }) -> close_dialog m
        | _ -> m, []
      in
      let m, toast_cmds =
        toast m (sprintf "#%d created: %s" d.pr.number d.pr.url)
      in
      m, cmds @ toast_cmds)
  | Created, Error e, Some (Pr_create ({ busy = true; _ } as f)) ->
    ( { m with
        dialog = Some (Pr_create { f with busy = false; error = Some (said e) })
      }
    , [] )
  | Checkout { number; _ }, result, Some (Pull p) when p.number = number ->
    let checkout : Pr_page.Checkout.t =
      match result with
      | Ok json ->
        (match Pr_checkout.of_json json with
         | Ok c -> Done c
         | Error e -> Failed (Error.to_string_hum e))
      | Error e -> Failed (said e)
    in
    { m with dialog = Some (Pull { p with checkout }) }, []
  | Checkout { number; session }, Ok json, dialog ->
    decode m json Pr_checkout.of_json ~f:(fun checkout ->
      match dialog with
      | None when here session ->
        open_dialog m (Pr_checked_out { number; checkout })
      | _ ->
        toast
          m
          (sprintf
             "Checked out pull request #%d at %s: /cd %s works there"
             number
             checkout.path
             checkout.path))
  | (List _ | View _ | Defaults _), _, _ -> m, []
  | (Created | Checkout _), Error e, _ -> error m (said e)
;;

let is_current_path (m : Model.t) path =
  Option.exists m.state ~f:(fun s -> String.equal s.session_path path)
;;

(* A context menu's item, from the step the backend has reached: switch to
   the session, move it into the directory, then run the command. *)
let run_plan (m : Model.t) (plan : Context_menu.Plan.t) =
  match plan.switch with
  | Some path when not (is_current_path m path) ->
    let m, fetch = fetch_newest m path in
    ( { m with
        pending = { m.pending with session = Some (Switching path) }
      ; sidebar_open = m.sidebar_open && not m.narrow
      }
    , rpc "switch_session" [ "path", str path ] ~tag:(Plan_switched plan)
      :: fetch )
  | _ ->
    let here dir =
      Option.exists m.state ~f:(fun s ->
        String.equal s.cwd dir || String.is_prefix s.cwd ~prefix:(dir ^ "/"))
    in
    (match plan.enter, plan.run, plan.notice with
     | Some dir, _, _ when not (here dir) ->
       ( with_entering m (Some dir)
       , [ call
             (Enter_worktree dir)
             ~tag:(Plan_entered { plan with enter = None })
         ] )
     | _, Some command, _ ->
       (match Slash.parse command with
        | Some parsed -> run_command { m with dialog = None } parsed
        | None -> m, [])
     | _, None, Some notice -> toast m notice
     | _, None, None -> m, [ focus_editor ])
;;

(* ---- drafting fields with the model (Generate) *)

(* The target's field, while it is on show: its text (a pull request's
   title and body). *)
let field_texts (m : Model.t) (target : Generate.Target.t) =
  match target, m.dialog with
  | Pr, Some (Pr_create f) -> Some [ f.title; f.body ]
  | Commit_message, _ -> Some [ m.git_bar.message ]
  | Worktree_branch, Some (New_worktree d) -> Some [ d.branch ]
  | Session_title, Some (Rename name) -> Some [ name ]
  | (Pr | Worktree_branch | Session_title), _ -> None
;;

let set_field_texts (m : Model.t) (target : Generate.Target.t) texts =
  match target, texts, m.dialog with
  | Pr, [ title; body ], Some (Pr_create f) ->
    { m with dialog = Some (Pr_create { f with title; body; error = None }) }
  | Commit_message, [ message ], _ ->
    { m with git_bar = { m.git_bar with message } }
  | Worktree_branch, [ branch ], Some (New_worktree d) ->
    { m with dialog = Some (New_worktree (New_worktree.set d Branch branch)) }
  | Session_title, [ name ], Some (Rename _) ->
    { m with dialog = Some (Rename name) }
  | _ -> m
;;

let streamed_texts (target : Generate.Target.t) text =
  match target with
  | Pr ->
    let title, body = Generated.split_pr text in
    [ title; body ]
  | Commit_message | Worktree_branch | Session_title -> [ text ]
;;

let generate_request (m : Model.t) (target : Generate.Target.t) ~gen_id
  : (Request.Method.t, string) Result.t
  =
  let kind = Generate.Target.kind target in
  let request ?dir ?base ?task () =
    Ok (Request.Method.Generate_text { gen_id; kind; dir; base; task })
  in
  match target, m.dialog with
  | Pr, Some (Pr_create { defaults = Some d; base; dir; _ }) ->
    let base = String.strip base in
    request ?dir ~base:(if String.is_empty base then d.base else base) ()
  | Pr, _ -> Error "Wait for the branch's details to load."
  | Commit_message, _ -> request ?dir:m.git_bar.dir ()
  | Worktree_branch, Some (New_worktree d) ->
    let task = String.strip d.branch in
    if String.is_empty task then request () else request ~task ()
  | Worktree_branch, _ | Session_title, _ -> request ()
;;

let start_generate (m : Model.t) (target : Generate.Target.t) =
  match field_texts m target with
  | None -> m, []
  | Some before ->
    let cancel = cancel_generate m in
    let generate, _ = Generate.stop m.generate in
    let generate, gen_id = Generate.start generate target ~before in
    (match generate_request m target ~gen_id with
     | Error e -> error m e
     | Ok method_ ->
       let m =
         match target, m.dialog with
         | Pr, Some (Pr_create f) ->
           { m with
             generate
           ; dialog = Some (Pr_create { f with error = None })
           }
         | Commit_message, _ ->
           { m with generate; git_bar = { m.git_bar with failure = None } }
         | _ -> { m with generate }
       in
       m, cancel @ [ call method_ ~tag:(Generated gen_id) ])
;;

let stop_generate (m : Model.t) =
  let cancel = cancel_generate m in
  let generate, _ = Generate.stop m.generate in
  { m with generate }, cancel
;;

let undo_generate (m : Model.t) target =
  match Generate.undo_for m.generate target with
  | None -> m, []
  | Some before ->
    let m = set_field_texts m target before in
    { m with generate = Generate.forget m.generate target }, []
;;

let generated (m : Model.t) id result =
  let generate, running = Generate.finish m.generate ~id in
  let m = { m with generate } in
  match running, result with
  | None, _ -> m, []
  | Some r, Error e ->
    let m =
      { (set_field_texts m r.target r.before) with
        generate = Generate.forget generate r.target
      }
    in
    if String.equal e "cancelled" then m, [] else error m e
  | Some r, Ok json ->
    (match Generated.of_json json with
     | Error e -> error m (Error.to_string_hum e)
     | Ok g ->
       let texts =
         match r.target, g.title, g.body with
         | Pr, Some title, Some body -> [ title; body ]
         | _ -> streamed_texts r.target g.text
       in
       set_field_texts m r.target texts, [])
;;

(* ---- the review page's git bar *)

let review_files (m : Model.t) =
  List.map m.review.files ~f:(fun f ->
    ( f.path
    , Prigh_ui.File_approval.Status.equal
        (Code_review.approval m.review f)
        Approved ))
;;

let git_out (m : Model.t) (outs : Git_bar.Out.t list) =
  List.fold outs ~init:(m, []) ~f:(fun (m, cmds) (out : Git_bar.Out.t) ->
    let m, more =
      match out with
      | Rpc { method_; reply } -> m, [ call method_ ~tag:(Git reply) ]
      | Draft_message -> start_generate m Commit_message
      | Changed -> with_review m (Code_review.refresh m.review)
      | Create_pr { dir } -> new_pr m ?dir ~title:""
    in
    m, cmds @ more)
;;

let with_git (m : Model.t) (git_bar, outs) = git_out { m with git_bar } outs

let git_action (m : Model.t) action =
  match Code_review.read_only m.review with
  | Some _ -> m, []
  | None -> with_git m (Git_bar.update m.git_bar ~files:(review_files m) action)
;;

let rec reply_to (m : Model.t) (tag : Reply_tag.t) result =
  match tag, result with
  | Reconnect generation, _ when generation <> m.generation -> m, []
  | Forge tag, result -> forge_reply m tag result
  | Generated id, result -> generated m id result
  | Git reply, result -> with_git m (Git_bar.reply m.git_bar reply result)
  | Files (Previewed path as reply), (Ok json as result) ->
    let m =
      match File_preview.of_json json with
      | Ok { content = Text { text; truncated = false; md5 }; _ } ->
        { m with
          review = Code_review.preview_text m.review ~abs:path ~text ~md5
        }
      | Ok _ | Error _ -> m
    in
    with_files m (Files.reply m.files reply result)
  | Files reply, result -> with_files m (Files.reply m.files reply result)
  | Review reply, result ->
    let m, cmds = with_review m (Code_review.reply m.review reply result) in
    (match reply with
     | Diff ->
       let m, more = remember_review (refresh_review_pick m) in
       m, cmds @ more
     | _ -> m, cmds)
  | Sent_with_files { text; images; attachments }, result ->
    let m, cmds = reply_to m (Sent { text; images }) result in
    (match result with
     | Ok _ -> m, cmds
     | Error _ -> { m with files = Files.restore m.files attachments }, cmds)
  | Reconnect _, Error e when Rejoin.refused e ->
    m, [ Command.Auth_failed (Rejoin.refused_message e) ]
  (* The banner says that we are reconnecting: a toast per attempt would
     pile up over the page. *)
  | Reconnect _, Error _ ->
    (match m.connection with
     | Connected -> m, []
     | Reconnecting { attempt; generation } ->
       let attempt = attempt + 1 in
       ( { m with connection = Reconnecting { attempt; generation } }
       , [ Command.Reconnect
             { generation
             ; delay_ms = Connection.delay_ms ~attempt
             ; session = Option.map m.state ~f:(fun s -> s.session_id)
             }
         ] ))
  | Session_gone { generation; _ }, _ when generation <> m.generation -> m, []
  | Session_gone { generation; _ }, Error e ->
    reply_to m (Reconnect generation) (Error e)
  | Reconnect _, Ok json -> Rejoin.rejoined m json
  | Session_gone { session; _ }, Ok json -> Rejoin.gone m json ~session
  | Resync { session; _ }, _ when not (String.equal session (session_id m)) ->
    m, []
  | Resync _, Ok json ->
    decode m json Message_page.of_json ~f:(fun page ->
      let chat =
        Chat.reconcile
          ~old:m.chat
          (Chat.of_messages ~running:(Model.running m) page.messages)
      in
      (* The session was still loading when the connection went. *)
      let loading = m.pending.messages in
      let m, outline =
        fetch_outline
          { m with
            chat
          ; paging = Paging.of_page page
          ; pending = { m.pending with messages = false }
          ; jump = Jump.empty
          }
      in
      ( m
      , (if loading then [ Command.Scroll_to_bottom ] else [])
        @ List.map (Chat.subagents_to_load chat) ~f:subagent_rpc
        @ outline ))
  | Resync _, Error "backend closed" -> m, []
  (* What was on show is not in the session any more (rewound meanwhile):
     its newest page instead. *)
  | Resync _, Error _ ->
    (match session_path m with
     | Some path ->
       fetch_newest
         { m with chat_cache = Chat_cache.remove m.chat_cache path }
         path
     | None -> m, [])
  (* It is still waiting: shown again, from what the backend waits for. *)
  | Answer, Error e when not (String.equal e "backend closed") ->
    let m, cmds =
      error m (sprintf "Couldn't send your answer: %s. It is asked again." e)
    in
    m, cmds @ [ rpc "get_pending" [] ~tag:Pending ]
  | Resync_pending, Ok json ->
    decode m json Pending.of_json ~f:(fun pending ->
      (* The backend's word on each confirmation (e.g. the rules to always
         allow), in the order on show. *)
      let confirms =
        List.filter_map m.confirms ~f:(fun c ->
          List.find pending.confirms ~f:(same_confirm c))
      in
      let questions =
        List.filter m.questions ~f:(fun (q : Question_dialog.t) ->
          List.exists pending.questions ~f:(fun (p : Pending.Ask.t) ->
            String.equal p.call_id q.call_id))
      in
      let confirm_reason =
        match m.confirms, confirms with
        | first :: _, first' :: _ when same_confirm first first' ->
          m.confirm_reason
        | _ -> None
      in
      let m, restored =
        restore_unsure m ~delivered:(fun u ->
          user_messages m.chat u.prompt.text > u.seen
          || List.mem
               (pending.steer_texts @ pending.follow_up_texts)
               u.prompt.text
               ~equal:String.equal)
      in
      let m, cmds =
        add_waiting
          { m with
            confirms
          ; questions
          ; confirm_reason
          ; queue =
              Queued.update
                Queued.empty
                ~steer:pending.steer_texts
                ~follow_up:pending.follow_up_texts
          ; agent_queues = pending.agents
          }
          ~confirms:pending.confirms
          ~questions:pending.questions
      in
      m, restored @ cmds)
  | Subagent id, Error e when Map.mem m.agents.fetches id ->
    transcript_failed m id e, []
  (* Subagents from before the backend restarted are gone: the report stays. *)
  | ( ( Ignore | Paths _
      | Auth_status Refresh
      | Subagent _ | Subagents | Jobs | Job_output _ | Config | Prompt_paths _
      | Users Probe )
    , Error _ ) -> m, []
  | Btw id, Error e -> update_btw m id ~f:(fun b -> Btw.fail b e), []
  (* Lost with the connection: listed again once back. *)
  | Worktrees { purpose = Sidebar _; _ }, Error e
    when String.equal e "backend closed" || String.equal e not_connected ->
    { m with worktrees = Worktrees.stale m.worktrees }, []
  (* Not a repository, say: the sidebar shows nothing. *)
  | Worktrees { purpose = Sidebar seq; _ }, Error _ ->
    { m with worktrees = Worktrees.failed m.worktrees ~seq }, []
  | Worktrees { purpose = Picker; _ }, Error e ->
    error m (sprintf "Couldn't list the worktrees: %s." e)
  | ( Worktrees { purpose = Review_target { command; model = Some args; _ }; _ }
    , Error _ ) ->
    (* Not a repository: no worktree by that name, the model reviews. *)
    review_action m ~command (Model args)
  | ( Worktrees
        { purpose = Target { command; _ } | Review_target { command; _ }; _ }
    , Error e ) ->
    error
      (back_in_editor m command)
      (sprintf "Couldn't list the worktrees: %s." e)
  | Review_checked_out _, Error e ->
    error m (sprintf "Couldn't check it out: %s." e)
  | Worktree_created { command; _ }, Error e ->
    worktree_create_failed m ~command e
  | Worktree_entered _, Error e ->
    error (with_entering m None) (sprintf "Couldn't open the worktree: %s." e)
  | Worktree_went_back { command; _ }, Error e ->
    error (back_in_editor m command) (sprintf "Couldn't switch back: %s." e)
  | Worktree_session _, Error e ->
    reply_to (with_entering m None) Reload_state (Error e)
  | Plan_switched _, Error e -> reply_to m Reload_state (Error e)
  | Plan_entered _, Error e ->
    error (with_entering m None) (sprintf "Couldn't move there: %s." e)
  | Repo_worktrees root, Error _ ->
    { m with repo_worktrees = Repo_listings.failed m.repo_worktrees ~root }, []
  | Worktree_removed { target; force }, Error e ->
    worktree_remove_failed m ~target ~force e
  (* A backend that keeps none: this browser keeps them. *)
  | Prefs, Error _ -> { m with prefs = { m.prefs with remote = Local } }, []
  | Skills { place; purpose = Complete }, Error _ ->
    (* Complete nothing rather than ask again at every key. *)
    if Option.equal String.equal (Skills.place m.state) (Some place)
    then { m with skills = Skills.loaded ~place [] }, []
    else m, []
  | Skills { purpose = Picker _; _ }, Error e ->
    error m (sprintf "Couldn't list the skills: %s" e)
  | Mcp (Refreshed _), Error e ->
    error
      m
      (sprintf "Couldn't start the MCP server: %s. /mcp reconnect retries." e)
  | Mcp (Picker | Reconnect), Error e ->
    error m (sprintf "Couldn't list the MCP servers: %s" e)
  | (Exported | Imported), Error e -> prompt_failed m e
  | Dir_changed _, Error e -> dir_change_failed m e
  | Config_saved _, Error e ->
    (* The change shown at once did not happen: show the backend's. *)
    let m, cmds = error m (sprintf "Couldn't save the settings: %s" e) in
    m, cmds @ [ rpc "get_config" [] ~tag:Config ]
  | Fallback_saved, Error e ->
    error m (sprintf "Couldn't set the fallback models: %s" e)
  | Default_dir_saved, Error e ->
    error m (sprintf "Couldn't set the default directory: %s" e)
  | Compaction_saved, Error e ->
    error m (sprintf "Couldn't set compaction: %s" e)
  | Sessions_deleted, Error e -> error m (sprintf "Couldn't delete: %s" e)
  | Archived_set { title; archived }, Error e ->
    let m, cmds =
      error
        m
        (sprintf
           "Couldn't %s “%s”: %s"
           (if archived then "archive" else "unarchive")
           title
           e)
    in
    m, cmds @ [ rpc "list_sessions" [] ~tag:Sessions ]
  | Login_started, Error e ->
    (match m.dialog with
     | Some (Login flow) ->
       { m with dialog = Some (Login { flow with failed = Some e }) }, []
     | _ -> error m e)
  | (Dequeued | Unqueued _), Error e ->
    (* Shown removed at once: the backend's queue is the truth. *)
    let m, cmds = error m (String.capitalize e) in
    m, cmds @ [ rpc "get_pending" [] ~tag:Pending ]
  | Mode_set _, Error e ->
    (* The mode shown at once was not taken. *)
    let m, cmds =
      error
        { m with pending = { m.pending with mode = None } }
        (sprintf "Couldn't set the mode: %s" e)
    in
    m, cmds @ [ rpc "get_state" [] ~tag:State ]
  | Reminder { command; _ }, Error e ->
    if String.is_empty m.draft && List.is_empty m.images
    then
      error
        (set_draft m command)
        (sprintf "%s. %s is back in the editor." (String.capitalize e) command)
    else error m (String.capitalize e)
  | Reminder { notice; _ }, Ok json ->
    toast
      m
      (String.capitalize
         (match notice with
          | Some notice -> notice
          | None -> Prigh_ui.Remind_command.added json))
  | Command_prompt text, Error e ->
    let m = drop_toast m reading_review in
    if String.is_empty m.draft && List.is_empty m.images
    then
      error
        (set_draft m text)
        (sprintf "%s. %s is back in the editor." (String.capitalize e) text)
    else error m (String.capitalize e)
  | Command_prompt _, Ok _ ->
    let m = drop_toast m reading_review in
    if Model.running m
    then toast m "Queued: it starts when the current run ends"
    else m, []
  | Git_diff, Error e ->
    error (drop_toast m reading_changes) (String.capitalize e)
  | Git_diff, Ok json ->
    let m = drop_toast m reading_changes in
    decode m json Git_diff.of_json ~f:(fun diff ->
      if String.is_empty diff.diff
      then
        toast
          m
          (sprintf
             "No changes in %s against %s"
             diff.root
             (Prigh_ui.Diff_files.against diff))
      else open_dialog m (Diff diff))
  | Steered { agent; draft }, Error e ->
    let m = { m with pending = In_flight.answered m.pending ~failed:None } in
    let e = String.chop_suffix e ~suffix:"." |> Option.value ~default:e in
    if String.is_empty m.draft && List.is_empty m.images
    then
      error
        (set_draft m draft)
        (sprintf
           "Couldn't send it to %s: %s. Your message is back in the editor."
           agent
           e)
    else
      error
        m
        (sprintf
           "Couldn't send it to %s: %s. ↑ brings your message back."
           agent
           e)
  | Steered _, Ok _ ->
    { m with pending = In_flight.answered m.pending ~failed:None }, []
  | Sent { text; images }, Error "backend closed" ->
    let prompt = { In_flight.Prompt.text; images } in
    let pending = In_flight.answered m.pending ~failed:(Some prompt) in
    ( { m with
        pending =
          { pending with
            unsure =
              pending.unsure @ [ { prompt; seen = user_messages m.chat text } ]
          }
      }
    , [] )
  | Sent { text; images }, Error e ->
    let m =
      { m with
        pending =
          In_flight.answered
            m.pending
            ~failed:(Some { In_flight.Prompt.text; images })
      }
    in
    (* Back in the editor, unless something else is being written there. *)
    if String.is_empty m.draft && List.is_empty m.images
    then
      error
        (set_draft { m with images } text)
        (sprintf "Couldn't send: %s. Your message is back in the editor." e)
    else error m (sprintf "Couldn't send: %s. ↑ brings your message back." e)
  (* The backend may be on a session clicked before: its state says. *)
  | Reload_state, Error e ->
    let m, cmds =
      error
        { m with pending = { m.pending with session = None } }
        (sprintf "Couldn't open the session: %s" e)
    in
    m, cmds @ [ rpc "get_state" [] ~tag:State ]
  (* Lost with the connection: the reconnect's [Resync] brings the messages
     (and older pages are asked for again on scrolling up). *)
  | Page { path; before = None }, Error "backend closed" ->
    { m with chat_cache = Chat_cache.set_loading m.chat_cache path false }, []
  | Page { path; before = Some _ }, Error "backend closed"
    when is_current m path ->
    no_jump { m with paging = { m.paging with loading = false } }, []
  | Page { path; before = None }, Error e ->
    let m =
      { m with chat_cache = Chat_cache.set_loading m.chat_cache path false }
    in
    if is_current m path
    then
      error
        { m with pending = { m.pending with messages = false } }
        (sprintf
           "Couldn't load the session's messages: %s. Reload the page to try \
            again."
           e)
    else prefetch_next m
  (* Compacted since: what is on show is not in the session any more, its
     newest page (after the summary) replaces it. *)
  | Page { path; before = Some _ }, Error e
    when is_current m path && String.is_substring e ~substring:gone_from_session
    ->
    let m, fetch =
      fetch_newest
        (no_jump
           { m with
             paging = { m.paging with loading = false }
           ; chat_cache = Chat_cache.remove m.chat_cache path
           })
        path
    in
    let m, toast_cmds =
      toast
        m
        "Earlier messages were compacted: showing the session from its summary"
    in
    m, fetch @ toast_cmds
  | Page { path; before = Some _ }, Error e when is_current m path ->
    error
      (no_jump { m with paging = { m.paging with loading = false } })
      (sprintf "Couldn't load earlier messages: %s. Scroll up to try again." e)
  | Tool_output { agent_id; call_id }, Error e ->
    error
      { m with
        outputs_loading =
          Set.remove m.outputs_loading (output_key ~agent_id ~call_id)
      }
      (sprintf "Couldn't load the whole output: %s. Load them tries again." e)
  | Filter_saved session_id, result ->
    if
      not
        (Option.exists m.state ~f:(fun (s : State.t) ->
           String.equal s.session_id session_id))
    then m, []
    else (
      let m =
        { m with
          pending = { m.pending with filters = max 0 (m.pending.filters - 1) }
        }
      in
      (* The backend has answered all ours: what it keeps is the filter. *)
      let m =
        match m.state with
        | Some state when m.pending.filters = 0 ->
          let filter = session_filter state in
          if Transcript_filter.equal filter m.transcript_filter
          then m
          else filter_changed m filter
        | _ -> m
      in
      match result with
      | Ok _ -> m, []
      | Error e -> error m (String.capitalize e))
  | Model_set, Error e ->
    error
      { m with pending = { m.pending with model = None } }
      (String.capitalize e)
  | Thinking_set, Error e ->
    error
      { m with pending = { m.pending with thinking = None } }
      (String.capitalize e)
  (* The connection went with the call: the banner says so. *)
  (* Only the older pages' landmarks are missing (from an older backend
     too). *)
  | Outline path, Error _ ->
    ( (if Option.equal String.equal m.jump.fetching (Some path)
       then { m with jump = { m.jump with fetching = None } }
       else m)
    , [] )
  | Nix_status _, result ->
    let result =
      Result.bind result ~f:(fun json ->
        Result.map_error (Nix_store_status.of_json json) ~f:Error.to_string_hum)
    in
    let m = { m with nix_gc = Nix_gc.status m.nix_gc result } in
    (match tag, result with
     | Nix_status { open_settings = true }, Ok _ -> open_settings ~fetch:false m
     | Nix_status { open_settings = true }, Error e ->
       error m (String.capitalize e)
     | _ -> m, [])
  | Nix_collected, result ->
    let result =
      Result.bind result ~f:(fun json ->
        Result.map_error (Nix_gc_report.of_json json) ~f:Error.to_string_hum)
    in
    let m = { m with nix_gc = Nix_gc.finished m.nix_gc result } in
    let m, cmds =
      match result with
      | Ok report ->
        toast
          m
          ("Nix garbage collected: " ^ Prigh_ui.Nix_gc_command.report report)
      | Error e -> error m ("Nix garbage collection failed: " ^ e)
    in
    ( m
    , cmds
      @ [ call Nix_store_status ~tag:(Nix_status { open_settings = false }) ] )
  | Trimmed, result -> trimmed m result
  | Untrimmed, result -> untrimmed m result
  | Trimmed_messages path, result -> trimmed_messages m path result
  | Message_points purpose, Ok json ->
    decode
      m
      json
      Message_point.list_of_json
      ~f:(message_points_arrived m purpose)
  | Message_points (Delete { entry; _ }), Error e ->
    error (undelete_entry m (Some entry)) (String.capitalize e)
  | Deleted { entry }, result -> deleted m ~entry result
  | Trimmed_after, result -> trimmed_after m result
  | Range_restored entry, result -> restored m entry result
  | Hidden_messages { path; entry }, result ->
    hidden_messages m ~path ~entry result
  | _, Error "backend closed" -> m, []
  | _, Error e -> error m (String.capitalize e)
  | Sent _, Ok _ ->
    { m with pending = In_flight.answered m.pending ~failed:None }, []
  | Model_set, Ok _ -> { m with pending = { m.pending with model = None } }, []
  | Thinking_set, Ok _ ->
    { m with pending = { m.pending with thinking = None } }, []
  | (Ignore | Show_error | Login_started | Answer), Ok _ -> m, []
  | Notice text, Ok _ -> toast m text
  (* The state we asked for, also after switching session: what is shown
     next is up to date. *)
  | State, Ok json ->
    decode m json State.of_json ~f:(fun state ->
      if superseded m state
      then m, []
      else
        set_state { m with pending = { m.pending with session = None } } state)
  (* The backend answers a change of session with the new state; an older
     one only with [{}]. *)
  | Reload_state, Ok json ->
    (match field json "state" with
     | Some state ->
       decode m state State.of_json ~f:(fun state ->
         if superseded m state
         then m, []
         else
           set_state
             { m with pending = { m.pending with session = None } }
             state)
     | None -> m, [ rpc "get_state" [] ~tag:State ])
  | Page { path; before = None }, Ok json ->
    decode m json Message_page.of_json ~f:(fun page ->
      let m, cmds = newest_page m path page in
      let m, outline = if is_current m path then fetch_outline m else m, [] in
      let m, reveal = reveal_trimmed m path in
      let m, hidden = if is_current m path then reveal_hidden m else m, [] in
      m, cmds @ outline @ reveal @ hidden)
  | Trim_points purpose, Ok json ->
    decode m json Trim.Point.list_of_json ~f:(trim_points_arrived m purpose)
  | Outline path, Ok json ->
    let m =
      if Option.equal String.equal m.jump.fetching (Some path)
      then { m with jump = { m.jump with fetching = None } }
      else m
    in
    if is_current m path
    then
      decode m json Outline.of_json ~f:(fun outline ->
        { m with jump = { m.jump with outline = Some (path, outline) } }, [])
    else m, []
  | Page { path; before = Some before }, Ok json ->
    decode m json Message_page.of_json ~f:(older_page m path ~before)
  | Tool_output { agent_id; call_id }, Ok json ->
    let m =
      { m with
        outputs_loading =
          Set.remove m.outputs_loading (output_key ~agent_id ~call_id)
      }
    in
    (match Json.string_field json "text" with
     | Ok text ->
       { m with chat = Chat.set_output m.chat ~agent_id ~call_id text }, []
     | Error e -> error m (Error.to_string_hum e))
  | Pending, Ok json ->
    decode m json Pending.of_json ~f:(fun pending ->
      add_waiting
        { m with
          queue =
            Queued.update
              Queued.empty
              ~steer:pending.steer_texts
              ~follow_up:pending.follow_up_texts
        ; agent_queues = pending.agents
        }
        ~confirms:pending.confirms
        ~questions:pending.questions)
  | Mode_set notice, Ok json ->
    decode m json State.of_json ~f:(fun state ->
      let m, cmds = set_state m state in
      let m, toast_cmds = toast m ~replaces:[ "Mode: " ] notice in
      m, cmds @ toast_cmds)
  | Job_started, Ok json ->
    (match Json.string_field json "job_id" with
     | Ok id ->
       toast
         m
         (sprintf
            "Started job %s: its result reaches the agent when it exits."
            id)
     | Error _ -> m, [])
  | Subagent id, Ok json ->
    (match subagent_of_json json with
     | Ok (subagent, `Parent parent, `Call call_id) ->
       ( { m with
           chat =
             Chat.set_nested_subagent
               m.chat
               ~parent
               ~call_id
               ~detach:(Map.mem m.agents.fetches id)
               subagent
         ; agents =
             { m.agents with
               fetches =
                 Map.remove (Map.remove m.agents.fetches id) subagent.agent_id
             }
         }
       , [] )
     | Error e -> transcript_failed m id (Error.to_string_hum e), [])
  | Subagents, Ok json ->
    decode m json (decode_list Agents.Agent.of_json) ~f:(fun agents ->
      with_agents m ~f:(fun a -> Agents.set_agents a agents), [])
  | Jobs, Ok json ->
    decode m json (decode_list Job_info.of_json) ~f:(fun jobs ->
      let before = m.agents in
      let m =
        with_agents m ~f:(fun a -> Agents.set_jobs a ~now:(now_of m) jobs)
      in
      (* The shown job's last lines, once it has exited. *)
      ( m
      , match m.agents.selected with
        | Some (Job id as job)
          when Agents.running before job && not (Agents.running m.agents job) ->
          [ job_output id ]
        | _ -> [] ))
  | Job_output id, Ok json ->
    (match field json "text", m.agents.selected with
     | Some (`String text), Some (Job shown) when String.equal id shown ->
       with_agents m ~f:(fun a -> { a with output = Some (id, text) }), []
     | _ -> m, [])
  | Sessions, Ok json ->
    decode m json (decode_list Session_summary.of_json) ~f:(fun sessions ->
      let paths = List.map sessions ~f:(fun s -> s.path) in
      ( { m with
          sessions
        ; marks = Session_marks.prune m.marks ~paths
        ; chat_cache =
            Chat_cache.retain m.chat_cache ~paths:(String.Set.of_list paths)
        }
      , [] ))
  | Refresh_sessions, Ok _ -> m, [ rpc "list_sessions" [] ~tag:Sessions ]
  | Models, Ok json ->
    decode m json (decode_list Llm.of_json) ~f:(fun models ->
      { m with models }, [])
  | Auth_status purpose, Ok json ->
    decode m json (decode_list Auth_status.of_json) ~f:(fun auth ->
      let m = { m with auth } in
      match purpose with
      | Refresh ->
        (match m.dialog with
         | Some (Auth _) -> { m with dialog = Some (Auth auth) }, []
         | _ -> m, [])
      | Show -> open_dialog m (Auth auth)
      | Login_picker -> open_picker m (login_picker auth ~forges:m.forges)
      | Logout_picker ->
        (match logout_picker auth ~forges:m.forges with
         | Some dialog -> open_picker m dialog
         | None ->
           toast
             m
             "No provider or git host is logged in: /login logs in to one."))
  | Paths prefix, Ok json ->
    (match m.completion, strings json with
     | Some c, Ok paths ->
       { m with completion = Some (Completion.set_results c ~prefix paths) }, []
     | _ -> m, [])
  | Restored, Ok json ->
    (match Option.map (field json "restored") ~f:strings with
     | Some (Ok (_ :: _ as texts)) ->
       let draft =
         String.concat
           ~sep:"\n\n"
           (texts @ List.filter [ m.draft ] ~f:(Fn.non String.is_empty))
       in
       let m = set_draft m draft in
       let m, cmds =
         toast
           m
           (sprintf
              "Stopped; %d queued message%s back in the editor"
              (List.length texts)
              (if List.length texts = 1 then "" else "s"))
       in
       m, cmds @ [ focus_editor ]
     | _ -> m, [])
  | Unqueued text, Ok _ ->
    toast m (sprintf "Removed from the queue: %s" (clamp text))
  | Dequeued, Ok json ->
    (match field json "text" with
     | Some (`String text) ->
       let draft =
         if String.is_empty m.draft then text else text ^ "\n\n" ^ m.draft
       in
       set_draft m draft, [ focus_editor ]
     | _ -> toast m "Nothing is queued")
  | Sessions_deleted, Ok json ->
    decode m json Delete_result.of_json ~f:(fun result ->
      let gone =
        String.Set.of_list
          (List.map result.deleted ~f:(fun (d : Delete_result.Deleted.t) ->
             d.path))
      in
      let m =
        { m with
          sessions =
            List.filter m.sessions ~f:(fun s -> not (Set.mem gone s.path))
        ; marks =
            (* What was kept (refused, or not asked for) stays selected. *)
            (let kept = Set.diff m.marks.marked gone in
             if Set.is_empty kept
             then Session_marks.empty
             else { m.marks with marked = kept })
        }
      in
      let m, cmds =
        toast
          m
          ~error:(not (List.is_empty result.refused))
          (Delete_result.summary result)
      in
      m, cmds @ [ rpc "list_sessions" [] ~tag:Sessions ])
  | Archived_set { title; archived }, Ok _ ->
    toast
      m
      (if archived
       then sprintf "Archived “%s” (a new message unarchives it)" title
       else sprintf "Unarchived “%s”" title)
  | Config, Ok json ->
    decode m json Config.of_json ~f:(fun config ->
      { m with config = Some config }, [])
  | Config_saved notice, Ok json ->
    decode m json Config.of_json ~f:(fun config ->
      toast { m with config = Some config } notice)
  | Fallback_saved, Ok json ->
    decode m json Config.of_json ~f:(fun config ->
      fallback_saved { m with config = Some config } config)
  | Default_dir_saved, Ok json ->
    decode m json Config.of_json ~f:(fun config ->
      toast
        { m with config = Some config }
        (match config.default_cwd with
         | Some _ -> default_dir_summary config
         | None ->
           "Default directory cleared: new sessions start in the backend's \
            working directory"))
  | Compaction_saved, Ok json ->
    decode m json Config.of_json ~f:(fun config ->
      toast { m with config = Some config } (compaction_summary config))
  | Default_saved, Ok json ->
    decode m json Config.of_json ~f:(fun config ->
      let show = Option.value ~default:"?" in
      toast
        { m with config = Some config }
        (sprintf
           "New sessions start with %s, thinking %s"
           (show config.default_model)
           (show config.default_thinking)))
  | Session_stats, Ok json ->
    decode m json Session_stats.of_json ~f:(fun stats ->
      open_dialog m (Session stats))
  | Entries purpose, Ok json -> entries_picker m purpose json
  | Reload_messages, Ok _ ->
    let m, fetch =
      match session_path m with
      | Some path ->
        fetch_newest
          { m with chat_cache = Chat_cache.remove m.chat_cache path }
          path
      | None -> m, []
    in
    m, rpc "get_state" [] ~tag:State :: fetch
  | Exported, Ok json ->
    (match Json.string_field json "path" with
     | Ok path ->
       let m, close = prompt_succeeded m in
       let m, cmds = toast m ("Exported to " ^ path) in
       m, close @ cmds
     | Error _ -> prompt_succeeded m)
  | Imported, Ok json ->
    let m, close = prompt_succeeded m in
    let m, cmds =
      match Json.string_field json "path" with
      | Ok path -> toast m ("Imported as " ^ path)
      | Error _ -> m, []
    in
    m, close @ cmds @ [ rpc "get_state" [] ~tag:State ]
  | Dir_changed target, Ok json ->
    (match Dir_change.Outcome.of_json json with
     | Ok outcome -> dir_changed m target outcome
     | Error e -> error m (Error.to_string_hum e))
  | Prompt_paths prefix, Ok json ->
    (match m.dialog, strings json with
     | Some (Prompt prompt), Ok paths ->
       ( { m with
           dialog = Some (Prompt (Prompt.set_suggestions prompt ~prefix paths))
         }
       , [] )
     | _ -> m, [])
  | Btw id, Ok json ->
    let answer =
      Json.string_field json "text" |> Result.ok |> Option.value ~default:""
    in
    update_btw m id ~f:(Btw.finish ~answer), []
  | Users Probe, Ok json ->
    (match strings json with
     | Ok users -> { m with users = Some users }, []
     | Error _ -> m, [])
  | Users Picker, Ok json ->
    decode m json strings ~f:(fun users -> users_picker m users)
  | Prefs, Ok json -> decode m json Prefs.of_reply ~f:(prefs_loaded m)
  | User_switched, Ok json ->
    decode m json Hello_reply.of_json ~f:(user_switched m)
  (* Listed for another session, directory or host. *)
  | Skills { place; _ }, Ok _
    when not (Option.equal String.equal (Skills.place m.state) (Some place)) ->
    m, []
  | Skills { place; purpose }, Ok json ->
    decode
      m
      json
      (fun json -> Json.list_field json "skills" ~f:Skill.of_json)
      ~f:(fun skills ->
        let m = { m with skills = Skills.loaded ~place skills } in
        match purpose, skills with
        | Complete, _ -> refresh_completion { m with completion = None }
        | Picker _, [] -> toast m Skills.none
        | Picker query, skills -> open_picker m (Skills.picker ~query skills))
  | Mcp purpose, Ok json ->
    decode m json Mcp_list.of_json ~f:(mcp_listed m purpose)
  | Worktrees { place; purpose }, Ok json ->
    decode m json Worktree.Listing.of_json ~f:(fun listing ->
      let seq =
        match purpose with
        | Sidebar seq -> Some seq
        | Picker | Review_pick | Target _ | Review_target _ -> None
      in
      let m =
        { m with worktrees = Worktrees.loaded m.worktrees ~place ?seq listing }
      in
      match purpose with
      | Sidebar _ ->
        if String.is_prefix m.draft ~prefix:"/worktree "
        then refresh_completion { m with completion = None }
        else m, []
      | Picker -> worktrees_picker m listing
      | Review_pick -> refresh_review_pick m ~worktrees:listing.worktrees, []
      | Target { command; action } -> worktree_target m ~command action listing
      | Review_target { command; name; base; direct; model } ->
        review_target m ~command ~name ~base ~direct ~model listing)
  | Review_checked_out target, Ok json ->
    decode m json Worktree.Created.of_json ~f:(review_checked_out m target)
  | Worktree_created _, Ok json ->
    decode m json Worktree.Created.of_json ~f:(worktree_created m)
  | Worktree_entered notice, Ok json ->
    let m = with_entering m None in
    decode m json Dir_change.Outcome.of_json ~f:(function
      | Changed _ -> toast m notice
      | Missing path ->
        error m (sprintf "%s is gone: /worktree lists the others." path))
  | Worktree_went_back { home; _ }, Ok json ->
    decode m json Dir_change.Outcome.of_json ~f:(function
      | Changed { cwd; _ } ->
        toast
          m
          (String.capitalize
             (Worktree_command.back_notice
                home
                ~cwd
                ~tilde:
                  (Tilde.of_path
                     ?host_home:(Option.bind m.state ~f:Tilde.active_host_home))))
      | Missing path -> error m (sprintf "%s is gone." path))
  | Worktree_session { path; notice }, Ok json ->
    let m, cmds = reply_to m Reload_state (Ok json) in
    ( with_entering m (Some path)
    , cmds @ [ enter_worktree ~new_session:true ~notice path ] )
  | Plan_switched plan, Ok json ->
    let m, cmds = reply_to m Reload_state (Ok json) in
    (* Not if the user went on to another session meanwhile. *)
    (match plan.switch with
     | Some path when is_current_path m path ->
       let m, more = run_plan m plan in
       m, cmds @ more
     | Some _ | None -> m, cmds)
  | Plan_entered plan, Ok json ->
    let m = with_entering m None in
    decode m json Dir_change.Outcome.of_json ~f:(function
      | Changed _ -> run_plan m plan
      | Missing path ->
        error m (sprintf "%s is gone: /worktree lists the others." path))
  | Repo_worktrees root, Ok json ->
    decode m json Worktree.Listing.of_json ~f:(fun listing ->
      ( { m with
          repo_worktrees = Repo_listings.loaded m.repo_worktrees ~root listing
        }
      , [] ))
  | Worktree_removed _, Ok json ->
    decode m json Worktree.removed_of_json ~f:(fun w ->
      let m, close =
        match m.dialog with
        | Some (Remove_worktree { busy = true; _ }) -> close_dialog m
        | _ -> m, []
      in
      let m, cmds =
        toast
          { m with
            worktrees = Worktrees.stale m.worktrees
          ; repo_worktrees = Repo_listings.stale m.repo_worktrees
          }
          (String.capitalize (Worktree_command.removed w))
      in
      m, close @ cmds)
;;

(* The replies that a [loading] dialog waits for. *)
let loads : Reply_tag.t -> bool = function
  | Auth_status (Login_picker | Logout_picker | Show)
  | Session_stats | Entries _
  | Users Picker
  | Skills { purpose = Picker _; _ }
  | Worktrees { purpose = Picker | Target _ | Review_target _; _ }
  | Mcp Picker
  | Trim_points _ -> true
  | Message_points
      (Delete_picker | Trim_after_picker | Trim_before _ | Trim_after _) -> true
  | _ -> false
;;

(* Back: a dialog's error from while it was down no longer applies. *)
let clear_not_connected (m : Model.t) =
  let stale = Option.equal String.equal (Some not_connected) in
  match m.dialog with
  | Some (Prompt p) when stale p.error ->
    { m with dialog = Some (Prompt { p with error = None }) }
  | Some (Create_dir d) when stale d.error ->
    { m with dialog = Some (Create_dir { d with error = None }) }
  | _ -> m
;;

let reply (m : Model.t) (tag : Reply_tag.t) result =
  let result =
    match result with
    | Error "not connected" -> Error not_connected
    | result -> result
  in
  let m =
    match tag, result with
    | Reconnect generation, Ok _ when generation = m.generation ->
      clear_not_connected m
    | _ -> m
  in
  if not (loads tag)
  then reply_to m tag result
  else (
    match m.dialog with
    | Some (Loading _) ->
      let m, cmds = reply_to { m with dialog = None } tag result in
      (* A toast instead, or an error: back to the editor. *)
      m, if Option.is_none m.dialog then cmds @ [ focus_editor ] else cmds
    (* Closed while it was loading. *)
    | _ -> m, [])
;;

let auth_event (m : Model.t) (e : Auth_event.t) =
  match e, m.dialog with
  | Done { provider; method_ }, dialog ->
    let custom =
      is_custom m.auth provider
      ||
      match dialog with
      | Some (Login { provider = "custom"; _ }) -> true
      | _ -> false
    in
    let m =
      match dialog with
      | Some (Login _) -> { m with dialog = None }
      | _ -> m
    in
    let m, cmds =
      toast
        m
        (if custom
         then
           sprintf
             "Saved %s: /model lists its models as %s/<id>"
             provider
             provider
         else sprintf "Logged in to %s (%s)" provider method_)
    in
    ( m
    , cmds
      @ [ rpc "auth_status" [] ~tag:(Auth_status Refresh)
        ; rpc "list_models" [] ~tag:Models
        ] )
  | Logged_out provider, dialog ->
    let m =
      match dialog with
      | Some (Login { purpose = Logout; _ }) -> { m with dialog = None }
      | _ -> m
    in
    let m, cmds = toast m (sprintf "Logged out of %s" provider) in
    ( m
    , cmds
      @ [ rpc "auth_status" [] ~tag:(Auth_status Refresh)
        ; rpc "list_models" [] ~tag:Models
        ] )
  | Forge_done { host; user; method_ }, dialog ->
    let m, close =
      match dialog with
      | Some (Login _) -> close_dialog m
      | _ -> m, []
    in
    let m, cmds =
      toast
        m
        (sprintf
           "Signed in to %s%s (%s)"
           host
           (Option.value_map user ~default:"" ~f:(fun u -> " as " ^ u))
           method_)
    in
    m, close @ cmds @ [ call Forge_status ~tag:(Forge Status) ]
  | Forge_logged_out host, _ ->
    let m =
      with_forges m ~f:(fun f ->
        { f with signing_out = Set.remove f.signing_out host })
    in
    let m, cmds =
      toast m ~replaces:[ "Signing out of " ] (sprintf "Signed out of %s" host)
    in
    m, cmds @ [ call Forge_status ~tag:(Forge Status) ]
  | Prompt { prompt = Secret _ | Text _ | Manual_code _; _ }, Some (Login flow)
    ->
    ( { m with dialog = Some (Login (Login_flow.apply flow e)) }
    , [ Command.Focus "dialog-input" ] )
  | _, Some (Login flow) ->
    { m with dialog = Some (Login (Login_flow.apply flow e)) }, []
  | Failed { provider; _ }, _
    when Option.equal String.equal m.cancelled_login (Some provider) ->
    { m with cancelled_login = None }, []
  | Failed { provider; error = e }, _ ->
    error m (sprintf "Login to %s failed: %s. /login tries again." provider e)
  | (Auth_url _ | Prompt _), _ ->
    open_dialog m (Login (Login_flow.apply (Login_flow.start "") e))
  | (Prompt_cancelled _ | Progress _), _ -> m, []
;;

(* A message the user sent a subagent ([steer_agent]), as its transcript has
   it: the note in front says so. *)
let user_steer (u : Message.User.t) =
  Option.bind u.origin ~f:(fun (o : Origin.t) ->
    List.find o.notes ~f:(fun n ->
      match n.kind with
      | Other "steer" -> String.is_prefix n.title ~prefix:"You"
      | _ -> false))
;;

let main_event (m : Model.t) (event : Event.t) =
  match event with
  | State state when superseded m state -> m, []
  | State state -> set_state m state
  | Notice text -> toast m text
  | Masked { masked; tokens_saved } ->
    toast m (Event.masked_notice ~masked ~tokens_saved)
  | Queue_update { steer; follow_up } ->
    { m with queue = Queued.update m.queue ~steer ~follow_up }, []
  | Agent_queues agent_queues -> { m with agent_queues }, []
  | Subagent { agent_id; event = Message_start (User user); _ } ->
    (match user_steer user with
     | Some (note : Origin.Note.t) ->
       toast
         m
         (sprintf
            "%s delivered to %s: %s"
            (if String.is_substring note.title ~substring:"follow-up"
             then "Follow-up"
             else "Steer")
            agent_id
            (clamp
               (String.tr
                  ~target:'\n'
                  ~replacement:' '
                  (Message.User.body user))))
     | None -> m, [])
  | Agent_end _ -> { m with queue = Queued.forget_delivering m.queue }, []
  | Message_start (User user) ->
    (match Queued.delivered m.queue ~text:(Injected_note.typed user) with
     | queue, Some q ->
       toast
         { m with queue }
         (sprintf
            "%s delivered: %s"
            (match q.kind with
             | Steer -> "Steer"
             | Follow_up -> "Follow-up")
            (clamp (Queued.Message.one_line q)))
     | _, None -> m, [])
  | Tool_confirm c -> add_waiting m ~confirms:[ c ] ~questions:[]
  | Tool_confirm_done { call_id; agent_id } ->
    if
      List.exists m.confirms ~f:(fun c ->
        String.equal c.call_id call_id
        && Option.equal String.equal c.agent_id agent_id)
    then (
      let m, cmds = remove_confirm m ~call_id ~agent_id in
      let m, toast_cmds = toast m "Confirmation answered elsewhere" in
      m, cmds @ toast_cmds)
    else m, []
  | Ask_user ask -> add_waiting m ~confirms:[] ~questions:[ ask ]
  | Ask_user_done { call_id; timed_out } ->
    if List.exists m.questions ~f:(fun q -> String.equal q.call_id call_id)
    then (
      let m, cmds = remove_question m call_id in
      (* On a timeout the backend's notice says so. *)
      if timed_out
      then m, cmds
      else (
        let m, toast_cmds =
          toast m "Question answered elsewhere, or the run stopped"
        in
        m, cmds @ toast_cmds))
    else m, []
  | Tool_end { call; _ } ->
    (* An older backend's way to say a confirmation is no longer waited for. *)
    remove_confirm m ~call_id:call.id ~agent_id:None
  | Sessions_changed -> m, [ rpc "list_sessions" [] ~tag:Sessions ]
  | Nix_gc_progress p -> { m with nix_gc = Nix_gc.progress m.nix_gc p }, []
  (* Another of the user's frontends changed their preferences. *)
  | Prefs_changed changes ->
    apply_prefs m ~startup:false changes, [ Command.Cache_prefs changes ]
  (* Another client's /confirm, /scoped-models or defaults. *)
  | Config_changed config -> { m with config = Some config }, []
  | Review review -> with_review m (Code_review.set_review m.review review)
  | Auth e -> auth_event m e
  | Btw_delta { btw_id; delta } ->
    update_btw m btw_id ~f:(fun b -> Btw.append b delta), []
  | Generate_delta { gen_id; delta } ->
    (match Generate.delta m.generate ~id:gen_id delta with
     | generate, Some (target, text) ->
       ( set_field_texts { m with generate } target (streamed_texts target text)
       , [] )
     | generate, None -> { m with generate }, [])
  | _ -> m, []
;;

let event (m : Model.t) (e : Event.t) =
  let pending =
    match e with
    | Message_start (User _) -> In_flight.prompt_shown m.pending
    | Agent_end _ -> { m.pending with prompts = [] }
    | _ -> m.pending
  in
  let m =
    { m with
      chat = Chat.apply m.chat e
    ; agents = Agents.record m.agents ~now:(now_of m) e
    ; pending
    ; paging =
        (match e with
         (* The session starts at the summary now: nothing older to load. *)
         | Compacted _ -> Paging.all
         | _ -> m.paging)
    ; jump =
        (match e with
         | Compacted _ -> Jump.empty
         | _ -> m.jump)
    }
  in
  let m, cmds = main_event m e in
  let m, review =
    if touches_files e
    then with_review m (Code_review.changed m.review ~watching:m.files.open_)
    else m, []
  in
  m, cmds @ review @ if Agents.starts_or_ends e then [ list_subagents ] else []
;;

let image_json (image : Image.t) =
  `Object [ "mime_type", str image.mime_type; "data", str image.data ]
;;

(* [!cmd] runs a command and adds it and its output to the context, [!!cmd]
   only shows it, [!&cmd] starts a background job (also while running). *)
let shell (m : Model.t) text =
  let run ~prefix =
    String.strip (String.drop_prefix text (String.length prefix))
  in
  if String.is_prefix text ~prefix:"!&"
  then (
    match run ~prefix:"!&" with
    | "" -> error m "Type a command after !& to start it as a background job."
    | command ->
      ( m
      , [ rpc
            "shell"
            [ "command", str command; "background", Json.bool true ]
            ~tag:Job_started
        ] ))
  else if Model.running m
  then
    error
      m
      "Wait for the agent to finish (or Esc to stop it) before running \
       !commands; !&command starts a background job now."
  else (
    let add_to_context = not (String.is_prefix text ~prefix:"!!") in
    match run ~prefix:(if add_to_context then "!" else "!!") with
    | "" -> error m "Type a command after ! to run it."
    | command ->
      ( m
      , [ rpc
            "shell"
            [ "command", str command
            ; "add_to_context", Json.bool add_to_context
            ]
        ] ))
;;

(* [/skill:NAME args] is a prompt: the backend expands it. *)
let invokes_skill ({ name; _ } : Slash.Parsed.t) =
  match String.chop_prefix name ~prefix:"skill:" with
  | Some skill -> not (String.is_empty skill)
  | None -> false
;;

let send (m : Model.t) ~follow_up =
  let text = String.strip m.draft in
  (* The errors were about what was sent before. *)
  let m = { m with toasts = List.filter m.toasts ~f:(fun t -> not t.error) } in
  let sent (m : Model.t) =
    let history = History.add m.history text in
    let m = { (set_draft m "") with history; completion = None } in
    (* The backend's is the one that counts: once it has answered, ours
       waits for the next page load. *)
    ( m
    , (match m.prefs.remote, acting_as m with
       | Kept, None -> []
       | (Unknown | Local), _ | Kept, Some _ ->
         [ Command.Cache_prefs
             [ { key = Key.history
               ; value =
                   Some
                     (`Array
                         (List.map (History.to_list history) ~f:(fun s ->
                            `String s)))
               }
             ]
         ])
      @
      if saves_prefs m
      then
        [ rpc
            "push_pref"
            (Prefs.push_params
               ~key:Key.history
               (`String text)
               ~max:Pref_sync.history_max)
            ~tag:Ignore
        ]
      else [] )
  in
  let attached = not (List.is_empty m.files.attachments) in
  match Slash.parse text with
  | _
    when String.is_prefix text ~prefix:"!"
         && List.is_empty m.images
         && not attached ->
    let m, save = sent m in
    let m, cmds = shell m text in
    m, save @ (Command.Scroll_to_bottom :: cmds)
  | Some parsed
    when List.is_empty m.images && (not attached) && not (invokes_skill parsed)
    ->
    let first_toast = m.next_toast in
    let m, save = sent m in
    let m, cmds = run_command m parsed in
    (* A command refused without doing anything stays to be corrected. *)
    let refused =
      List.exists m.toasts ~f:(fun t -> t.error && t.id >= first_toast)
      && List.for_all cmds ~f:(function
        | Expire_toast _ -> true
        | _ -> false)
    in
    (if refused then set_draft m text else m), save @ cmds
  | _ when Option.is_some m.session_gone ->
    error
      m
      "This session is no longer on the backend: start a new one (/new) or \
       pick another in the sidebar. Your message stays here."
  | _ when not (Connection.equal m.connection Connected) ->
    error
      m
      "Not connected to the backend: your message stays here until it is back \
       (/retry-backend-connection tries now)."
  | _ when not (Recipient.equal m.recipient Recipient.Main) ->
    (match m.recipient with
     | Main -> m, []
     | Agent agent when attached || not (List.is_empty m.images) ->
       error
         m
         (sprintf
            "%s takes text only: remove the images and files, or Esc to send \
             them to the main agent."
            agent)
     | Agent _ when String.is_empty text -> m, []
     | Agent agent ->
       let m, save = sent m in
       let m, cmds = message_agent m ~agent ~text ~follow_up ~draft:text in
       m, save @ cmds)
  | _ when attached && Or_error.is_error (Files.prompt_attachments m.files) ->
    (match Files.prompt_attachments m.files with
     | Error e -> error m (Error.to_string_hum e)
     | Ok _ -> m, [])
  | _ ->
    if String.is_empty text && List.is_empty m.images && not attached
    then m, []
    else (
      let method_ =
        if follow_up
        then "follow_up"
        else if Model.running m
        then "steer"
        else "prompt"
      in
      let images =
        match m.images with
        | [] -> []
        | images -> [ "images", `Array (List.map images ~f:image_json) ]
      in
      let m, save =
        if String.is_empty text
        then { m with completion = None }, []
        else sent m
      in
      let prompt = String.equal method_ "prompt" in
      let agents = if prompt then Agents.prompt_sent m.agents else m.agents in
      let pending =
        In_flight.sent
          m.pending
          (Option.some_if prompt { In_flight.Prompt.text; images = m.images })
      in
      let attachments =
        match Files.prompt_attachments m.files with
        | Ok (_ :: _ as paths) ->
          [ "attachments", `Array (List.map paths ~f:str) ]
        | Ok [] | Error _ -> []
      in
      let tag : Reply_tag.t =
        if attached
        then
          Sent_with_files
            { text; images = m.images; attachments = m.files.attachments }
        else Sent { text; images = m.images }
      in
      ( { m with images = []; agents; pending; files = Files.sent m.files }
      , save
        @ [ Command.Scroll_to_bottom
          ; rpc method_ ((("text", str text) :: images) @ attachments) ~tag
          ] ))
;;

let accept_completion (m : Model.t) ~run =
  match Model.popup m with
  | None -> m, []
  (* Model keys complete word by word; Enter before another one is typed or
     picked sends the command as it is. *)
  | Some c
    when run
         && Completion.Source.equal (Completion.source c) (Argument Models)
         && String.is_empty (Completion.prefix c)
         && Completion.selected c = 0 -> send m ~follow_up:false
  | Some c when Completion.Source.equal (Completion.source c) Snippet ->
    (match
       Option.bind (Completion.selected_item c) ~f:(fun item ->
         Snippet.find (Option.value (snippets m) ~default:[]) item.id)
     with
     | Some snippet ->
       let draft, cursor = Completion.insert_snippet c ~text:m.draft snippet in
       edit_at_caret m ~cursor draft
     | None -> m, [])
  | Some c ->
    let draft, cursor = Completion.accept c ~text:m.draft in
    let runs =
      run
      &&
      match Completion.source c, Completion.selected_item c with
      | Command, Some item ->
        (* Optional free text (e.g. /compact's instructions) can be left out;
           an argument with completions is offered next. *)
        (match Slash.find ~superuser:(superuser m) item.id with
         | Some { args; argument = None; _ } ->
           String.is_empty args || String.is_prefix args ~prefix:"["
         (* [/review] alone is the common case: it runs at once. *)
         | Some { argument = Some Review; _ } -> true
         | Some { argument = Some _; _ } | None -> false)
      | Argument (Directory | Backend_directory | Path | Skill), _ -> false
      (* Their number comes next. *)
      | Argument Pr, Some { id = "view" | "checkout"; _ } -> false
      | Argument (Worktree | Review), _ when Completion.takes_more c -> false
      | Argument _, _ -> true
      | (Command | Path | Snippet), _ -> false
    in
    if runs
    then send (set_draft m (String.strip draft)) ~follow_up:false
    else edit_at_caret m ~cursor draft
;;

let message_gone =
  "That message is not in the model's context any more: /trim and /delete list \
   those that are."
;;

(* A message's menu's item: what its trim and delete do from /trim, /trim
   after and /delete. *)
let message_chosen (m : Model.t) n (item : Context_menu.Item.t) =
  match item, Model.message_menu m n with
  | _, None -> error m message_gone
  | Copy_message, Some msg ->
    let m, cmds =
      toast
        m
        (if msg.reply
         then "Copied the reply to the clipboard"
         else "Copied the message to the clipboard")
    in
    m, Command.Copy msg.text :: cmds
  | Trim_before, Some _ -> ask_trim m (Entry n)
  | Trim_after, Some _ -> ask_trim_after m n
  | Delete, Some _ -> ask_delete_message m n
  | ( ( Open
      | Open_tab
      | Rename
      | Fork
      | New_worktree
      | Switch_back
      | Switch_here
      | New_session_here
      | Review
      | Files
      | Terminal
      | Create_pr
      | Copy_path
      | Copy_id
      | Archive
      | Unarchive
      | Remove )
    , Some _ ) -> m, []
;;

(* A context menu's item: on the message, session or worktree it was opened
   on, switching to that session first when it is not the one on show. *)
let menu_chosen (m : Model.t) (menu : Context_menu.t) id =
  match
    ( menu.target
    , Option.bind (Context_menu.session menu) ~f:(find_session_by_path m)
    , Context_menu.Item.of_id id )
  with
  | _, _, None -> m, []
  | Message n, _, Some item -> message_chosen m n item
  | (Session _ | Worktree _), None, _ ->
    error m "That session is gone: the sidebar lists the others."
  | (Session _ | Worktree _), Some s, Some item ->
    let worktree = Context_menu.worktree menu in
    let cwd =
      match m.state with
      | Some state when String.equal state.session_path s.path -> state.cwd
      | _ -> s.cwd
    in
    let dir = Option.value worktree ~default:cwd in
    let plan ?enter ?run ?notice () =
      run_plan m { switch = Some s.path; enter; run; notice }
    in
    let copy what text =
      let m, cmds = toast m (sprintf "Copied %s: %s" what text) in
      m, Command.Copy text :: cmds
    in
    (match item with
     | Open -> switch_session m s.path
     | Open_tab -> m, [ Command.Open_tab s.id ]
     | Rename -> plan ~run:"/name" ()
     | Fork -> plan ~run:"/fork" ()
     | New_worktree -> plan ~run:"/worktree new" ()
     | Switch_back -> plan ~run:"/worktree back" ()
     | Switch_here ->
       let label =
         List.find s.worktrees ~f:(fun w -> String.equal w.path dir)
         |> Option.value_map
              ~default:(Filename.basename dir)
              ~f:Session_worktree.label
       in
       plan
         ~enter:dir
         ~notice:(sprintf "Working in the worktree %s (%s)" label dir)
         ()
     | New_session_here -> new_session_in m dir
     | Review ->
       plan ~run:(Prigh_ui.Review_command.command ~target:(Dir worktree) ()) ()
     | Files -> plan ~run:("/files " ^ dir) ()
     | Terminal ->
       plan
         ~run:
           (Option.value_map worktree ~default:"/terminal" ~f:(fun w ->
              "/terminal " ^ w))
         ()
     | Create_pr -> plan ?enter:worktree ~run:"/pr new" ()
     | Copy_path -> copy "the path" dir
     | Copy_id -> copy "the session id" s.id
     | Archive -> set_archived m s ~archived:true
     | Unarchive -> set_archived m s ~archived:false
     | Delete -> ask_delete m [ s.path ]
     | Remove -> ask_remove_session_worktree m dir
     | Copy_message | Trim_before | Trim_after -> m, [])
;;

let picker_accepted (m : Model.t) ~kind (item : Picker.Item.t) =
  let m = { m with dialog = None } in
  let m, cmds =
    match (kind : Dialog.Picker_kind.t) with
    | Models -> set_model m item.id
    | Thinking -> set_thinking m item.id
    | Login ->
      (match String.chop_prefix item.id ~prefix:"forge ", item.id with
       | Some host, _ -> forge_account_login m host
       | None, "forge-host" -> forge_host_dialog m
       | None, _ ->
         (match String.lsplit2 item.id ~on:' ' with
          | Some (provider, method_) ->
            start_login m ~provider ~method_:(Some method_)
          | None -> start_login m ~provider:item.id ~method_:None))
    | Logout ->
      (match String.chop_prefix item.id ~prefix:"forge " with
       | Some host -> forge_logout m host
       | None -> logout m item.id)
    | Verbosity ->
      (match
         List.find Prigh_ui.Verbosity.all ~f:(fun v ->
           String.equal (Prigh_ui.Verbosity.name v) item.id)
       with
       | Some v -> set_verbosity m v
       | None -> m, [])
    | Confirm_tools -> set_confirm m (String.equal item.id "on")
    | Themes -> theme_named m item.id
    | Accents -> accent_named m item.id
    | Modes -> mode_by_name m item.id
    | Fork entries ->
      let m =
        match Session_tree.user_text entries item.id with
        | Some text -> set_draft m text
        | None -> m
      in
      let m, cmds =
        toast m "Forked into a new session: edit the message and send it"
      in
      starting m, cmds @ [ rpc "fork" [ "at", str item.id ] ~tag:Reload_state ]
    | Rewind entries ->
      open_dialog
        m
        (Rewind_confirm
           { id = item.id
           ; text =
               Option.value
                 (Session_tree.user_text entries item.id)
                 ~default:item.label
           })
    | Tree -> m, [ rpc "rewind" [ "to", str item.id ] ~tag:Reload_messages ]
    | Hosts ->
      (match
         Option.bind m.state ~f:(fun s ->
           List.find s.hosts ~f:(fun h -> String.equal h.id item.id))
       with
       | Some host -> host_prompt m host
       | None ->
         error m (sprintf "Tool host %s has gone: /host lists them." item.id))
    | Users -> act_as m item.id
    | Accounts -> account_chosen m item.id
    | Skills -> edit m ("/skill:" ^ item.id ^ " ")
    | Snippets ->
      (match Snippet.find (Option.value (snippets m) ~default:[]) item.id with
       | Some snippet -> insert_snippet m snippet
       | None ->
         error
           m
           (sprintf "Snippet %s has gone: /snippet lists the others." item.id))
    | Reminders ->
      (match Reminder.find (Option.value (reminders m) ~default:[]) item.id with
       | Some r ->
         toast
           m
           (sprintf
              "%s (%s): %s · /remind rm %s removes it"
              r.name
              (Reminder.Trigger.describe r.trigger)
              r.text
              r.name)
       | None ->
         error
           m
           (sprintf "Reminder %s has gone: /remind lists the others." item.id))
    | Mcp l -> mcp_chosen m l item.id
    | Jump ->
      (match Jump.Target.of_string item.id with
       | Some target -> jump_to m target
       | None -> m, [])
    | Trim points ->
      (match
         List.find points ~f:(fun (p : Trim.Point.t) ->
           String.equal p.before item.id)
       with
       | Some point -> trim_confirm m point
       | None -> m, [])
    | Delete_message points ->
      (match
         List.find points ~f:(fun (p : Message_point.t) ->
           String.equal p.message item.id)
       with
       | Some point -> delete_chosen m point
       | None -> m, [])
    | Trim_after points ->
      (match
         List.find points ~f:(fun (p : Message_point.t) ->
           String.equal p.message item.id)
       with
       | Some _ when Model.running m -> error m (running_error `Trim)
       | Some point -> trim_after_confirm m point
       | None -> m, [])
    | Worktrees listing ->
      (match
         List.find listing.worktrees ~f:(fun w -> String.equal w.path item.id)
       with
       | Some w -> open_worktree m w ~new_session:false
       | None -> m, [])
    | Menu menu -> menu_chosen m menu item.id
  in
  m, (if Option.is_none m.dialog then [ focus_editor ] else []) @ cmds
;;

let picker_accept (m : Model.t) ~kind (item : Picker.Item.t) =
  match (kind : Dialog.Picker_kind.t) with
  (* Disabled: the menu stays, saying why. *)
  | Menu _ when item.dimmed -> m, []
  | _ -> picker_accepted m ~kind item
;;

let with_picker (m : Model.t) ~f =
  match m.dialog with
  | Some (Picker { kind; picker }) ->
    { m with dialog = Some (Picker { kind; picker = f picker }) }, []
  | Some (Popover { chip; picker }) ->
    { m with dialog = Some (Popover { chip; picker = f picker }) }, []
  | Some (Scoped_models { picker; checked }) ->
    { m with dialog = Some (Scoped_models { picker = f picker; checked }) }, []
  | Some (Pulls l) ->
    { m with dialog = Some (Pulls { l with picker = f l.picker }) }, []
  | _ -> m, []
;;

let toggle_scoped (m : Model.t) key =
  match m.dialog with
  | Some (Scoped_models { picker; checked }) ->
    let checked =
      if Set.mem checked key
      then Set.remove checked key
      else Set.add checked key
    in
    { m with dialog = Some (Scoped_models { picker; checked }) }, []
  | _ -> m, []
;;

let respond_login (m : Model.t) (flow : Login_flow.t) =
  match Login_flow.answer flow with
  | None -> m, []
  | Some (id, value) ->
    let respond = rpc "auth_respond" [ "id", str id; "value", str value ] in
    (match flow.purpose with
     | Login ->
       ( { m with dialog = Some (Login { flow with prompt = None; input = "" }) }
       , [ respond ] )
     (* Its one question answered, a logout ends with [Logged_out] or
        nothing (kept). *)
     | Logout ->
       let m, cmds = close_dialog m in
       m, cmds @ [ respond ])
;;

let dialog_accept (m : Model.t) =
  match m.dialog with
  | None -> m, []
  | Some (Picker { kind = Accents; picker })
    when Option.is_some (Theme.Accent.of_string (Picker.query picker)) ->
    accent_named { m with dialog = None } (Picker.query picker)
    |> fun (m, cmds) -> m, focus_editor :: cmds
  | Some (Picker { kind; picker }) ->
    (match Picker.selected_item picker with
     | Some item -> picker_accept m ~kind item
     | None -> m, [])
  | Some (Popover { chip; picker }) ->
    (match Picker.selected_item picker with
     | Some item -> picker_accept m ~kind:(chip_kind chip) item
     | None -> m, [])
  | Some (Review_pick p) ->
    (match Review_picker.selected_item p with
     | Some item -> review_picked m item
     | None -> m, [])
  | Some (Filter _) -> open_filter m
  | Some (Rename name) ->
    let name = String.strip name in
    if String.is_empty name
    then error m "Type a name for the session (Esc keeps the current one)."
    else (
      let m, cmds = close_dialog m in
      ( m
      , cmds
        @ [ rpc "set_session_name" [ "name", str name ] ~tag:Refresh_sessions ]
      ))
  | Some (Delete target) ->
    let m, cmds = close_dialog m in
    (* What the backend would keep is not asked for. *)
    let deletable paths =
      List.filter paths ~f:(fun path ->
        not
          (List.exists m.sessions ~f:(fun s ->
             String.equal s.path path
             && Option.is_some (Model.kept_from_deletion m s))))
    in
    (match target with
     | Sessions paths ->
       (match deletable paths with
        | [] -> m, cmds
        | paths ->
          m, cmds @ [ call (Delete_sessions paths) ~tag:Sessions_deleted ])
     | Archived -> m, cmds @ [ call Delete_archived ~tag:Sessions_deleted ])
  | Some (Login flow) ->
    (match flow.failed, flow.prompt with
     | None, Some _ -> respond_login m flow
     | Some _, _ -> close_dialog m
     | None, None -> m, [])
  | Some
      ( Help
      | Hotkeys
      | Settings
      | Auth _
      | Session _
      | Text _
      | Mcp_tools _
      | Diff _ ) -> close_dialog m
  | Some (Loading _) -> m, []
  | Some (Scoped_models { checked; _ }) ->
    let m, cmds = save_scoped m checked in
    m, focus_editor :: cmds
  | Some (Prompt prompt) -> submit_prompt m prompt
  | Some (Create_dir { busy = true; _ }) -> m, []
  | Some (Create_dir ({ target; path; _ } as d)) ->
    ( { m with dialog = Some (Create_dir { d with busy = true; error = None }) }
    , [ change_dir ~if_missing:Create ~target path ] )
  | Some (New_worktree d) -> create_worktree m d
  | Some (Remove_worktree _) -> remove_worktree m ~force:false
  | Some (Nix_gc_confirm generations) ->
    let m, cmds = close_dialog m in
    let m, start = start_nix_gc m (Some generations) in
    m, cmds @ start
  | Some (Trim_confirm _) -> trim m
  | Some (Trim_after_confirm _) -> trim_after m
  | Some (Rewind_confirm { id; _ }) ->
    let m, cmds = close_dialog m in
    let m, toast_cmds = toast m "Rewound: later messages stay in /tree" in
    ( m
    , cmds @ toast_cmds @ [ rpc "rewind" [ "to", str id ] ~tag:Reload_messages ]
    )
  | Some (Pulls l) ->
    (match Pull_requests.selected l with
     | Some pr -> open_pr ~back:l m pr.number
     | None -> m, [])
  | Some (Pull _) -> close_dialog m
  | Some (Pr_create { busy = true; _ }) -> m, []
  | Some (Pr_create f) ->
    (match Pr_create.request f with
     | Error e ->
       { m with dialog = Some (Pr_create { f with error = Some e }) }, []
     | Ok request ->
       ( { m with
           dialog = Some (Pr_create { f with busy = true; error = None })
         }
       , [ call request ~tag:(Forge Created) ] ))
  | Some (Pr_checked_out { checkout; _ }) ->
    let m, cmds = close_dialog m in
    m, cmds @ [ call (Enter_worktree checkout.path) ~tag:(Dir_changed Cwd) ]
  | Some (Forge_host { input; _ }) ->
    (match Forges.normalize_host input with
     | "" ->
       ( { m with
           dialog =
             Some
               (Forge_host
                  { input
                  ; error =
                      Some "Type the host, e.g. codeberg.org (Esc cancels)."
                  })
         }
       , [] )
     | host ->
       let back = m.return_to_settings in
       let m, cmds =
         forge_login
           m
           ~host
           ~kind:(Some "forgejo")
           ~method_:None
           ~again:("forgejo " ^ host)
       in
       { m with return_to_settings = back }, cmds)
;;

(* Following new output again: the jump button, or sending something. *)
let to_bottom (m : Model.t) =
  { m with scrolled_up = false }, [ Command.Scroll_to_bottom ]
;;

(* Sending something goes back to the end of the chat. *)
let sent_to_bottom ((m : Model.t), cmds) =
  if List.mem cmds Command.Scroll_to_bottom ~equal:Command.equal
  then { m with scrolled_up = false }, cmds
  else m, cmds
;;

let update_connected (m : Model.t) (action : Action.t) =
  match action with
  | Start -> m, start_commands m
  | Hello hello -> { m with hello = Some hello; nix_gc = Nix_gc.empty }, []
  | Saved_login -> { m with saved_login = true }, []
  (* What another session does is not this one's. *)
  | Event e
    when Option.is_some m.session_gone
         &&
         match e with
         | Sessions_changed | Nix_gc_progress _ -> false
         | _ -> true -> m, []
  | Event e ->
    let m, cmds = event m e in
    let notifications, notification =
      Page_notify.on_event
        m.notifications
        e
        ~where:(Prigh_ui.Notify.where m.state)
    in
    ( { m with notifications }
    , cmds
      @ (if Page_notify.equal notifications m.notifications
         then []
         else [ Command.Set_title (Page_notify.title notifications) ])
      @ Option.value_map notification ~default:[] ~f:(fun n ->
        [ Command.Notify n ]) )
  | Page_hidden hidden ->
    let notifications = Page_notify.set_hidden m.notifications hidden in
    let title =
      if notifications.unseen = m.notifications.unseen
      then []
      else [ Command.Set_title (Page_notify.title notifications) ]
    in
    let m = { m with notifications } in
    let m, reconnect = if hidden then m, [] else Rejoin.now m in
    m, title @ reconnect
  | Network true -> Rejoin.now m
  | Network false ->
    (match m.connection with
     | Connected -> m, [ Command.Drop_connection ]
     | Reconnecting _ -> m, [])
  | Restore_prefs { prefs; legacy } ->
    let m = apply_prefs m ~startup:true prefs in
    ( { m with
        prefs =
          { m.prefs with legacy = (if legacy then prefs else m.prefs.legacy) }
      }
    , [ Command.Apply_theme (Model.look m) ] )
  | Set_notification_permission permission ->
    ( { m with
        notifications =
          { m.notifications with
            permission = Page_notify.Permission.of_string permission
          }
      }
    , [] )
  | Notification_permission permission ->
    let permission = Page_notify.Permission.of_string permission in
    let m = { m with notifications = { m.notifications with permission } } in
    (match permission with
     | Granted | Default -> toast m (Page_notify.describe m.notifications)
     | Denied | Unsupported -> error m (Page_notify.describe m.notifications))
  | Protocol_error e -> error m ("Protocol error: " ^ e)
  | Backend_closed ->
    (match m.connection with
     | Reconnecting _ -> m, []
     | Connected ->
       let generation = m.generation + 1 in
       (* Lost before the session came back: waiting longer each time, then
          stopping, rather than reconnecting at once over and over. *)
       let drops = Drops.lost m.drops in
       let attempt = drops.lost in
       (* Everything on show stays, also what the agent waits for: once back,
          [get_pending] says what still does. *)
       let m =
         { m with
           connection = Reconnecting { attempt; generation }
         ; generation
         ; drops
         ; chat = Chat.missed_output m.chat
         }
       in
       if Drops.gave_up drops
       then m, []
       else
         ( m
         , [ Command.Reconnect
               { generation
               ; delay_ms = Connection.delay_ms ~attempt
               ; session = Option.map m.state ~f:(fun s -> s.session_id)
               }
           ] ))
  | Reply ((Resync _ as tag), result) ->
    reply { m with drops = Drops.resynced m.drops } tag result
  | Reply (tag, result) -> reply m tag result
  (* The ages in the sidebar; the list itself follows [sessions_changed]. *)
  | Tick now -> { m with now = Some now }, []
  | Set_utc_offset utc_offset -> { m with utc_offset }, []
  | Set_narrow narrow ->
    if Bool.equal narrow m.narrow
    then m, []
    else { m with narrow; sidebar_open = not narrow }, []
  | Set_draft draft -> edit m draft
  | Edit { text; cursor } -> edit m ~cursor text
  | Send -> sent_to_bottom (send m ~follow_up:false)
  | Send_follow_up -> sent_to_bottom (send m ~follow_up:true)
  | Abort -> m, [ rpc "abort" [] ~tag:Restored ]
  | History_older ->
    (match History.older m.history ~draft:m.draft with
     | None -> m, []
     | Some (history, text) -> edit { m with history } ~cursor:0 text)
  | History_newer ->
    (match History.newer m.history with
     | None -> m, []
     | Some (history, text) -> edit { m with history } text)
  | Complete_move delta ->
    ( { m with
        completion =
          Option.map m.completion ~f:(fun c -> Completion.move c delta)
      }
    , [] )
  | Complete_accept { run } -> accept_completion m ~run
  | Complete_choose i ->
    (match m.completion with
     | None -> m, []
     | Some c ->
       let c = Completion.move c (i - Completion.selected c) in
       accept_completion { m with completion = Some c } ~run:true)
  | Complete_close -> { m with completion = None }, []
  | New_session ->
    ( { (starting m) with sidebar_open = m.sidebar_open && not m.narrow }
    , [ rpc "new_session" [] ~tag:Reload_state; focus_editor ] )
  | Switch_session path -> switch_session m path
  | Ask_delete path ->
    if Set.mem m.marks.marked path
    then
      ask_delete
        m
        (List.map (Session_marks.marked m.marks m.sessions) ~f:(fun s -> s.path))
    else ask_delete m [ path ]
  | Select_sessions on ->
    ( { m with
        marks = (if on then Session_marks.start else Session_marks.empty)
      }
    , [] )
  | Mark_session { path; range } ->
    ( { m with
        marks =
          Session_marks.click m.marks ~path ~range ~order:(session_order m)
      }
    , [] )
  | Delete_marked ->
    ask_delete
      m
      (List.map (Session_marks.marked m.marks m.sessions) ~f:(fun s -> s.path))
  | Ask_delete_archived -> ask_delete_archived m
  | Set_archived { path; archived } ->
    (match find_session_by_path m path with
     | Some s -> set_archived m s ~archived
     | None -> m, [])
  | Set_session_query session_query -> { m with session_query }, []
  | Toggle_archived -> { m with archived_open = not m.archived_open }, []
  | Open_sessions -> open_sessions m
  | Session_nav { from; by } -> session_nav m ~from ~by
  | Open_first_session ->
    (match shown_sessions m with
     | [] -> m, []
     | first :: _ -> switch_session m first.path)
  | Leave_sidebar ->
    { m with sidebar_open = m.sidebar_open && not m.narrow }, [ focus_editor ]
  | Set_model key -> set_model m key
  | Set_thinking level -> set_thinking m level
  | Open_chip chip -> open_chip m chip
  | Open_filter -> open_filter m
  | Filter_press row -> filter_press m row
  | Chip_step delta -> step_chip m delta
  | Open_snippets cursor -> open_snippets m ~cursor
  | Open_help -> open_dialog m Help
  | Open_settings -> open_settings m
  | Open_rename -> open_rename m
  | Open_subagents arg -> open_agents m arg
  | Toggle_subagents ->
    if m.agents.open_ then close_agents m else open_agents m None
  | Select_item item -> select m item
  | Agents_back -> agents_back m
  | Focus_agent n ->
    (match List.nth (Agents.listed m.agents) (n - 1) with
     | Some (item, _) -> select m item
     | None -> m, [])
  | Cycle_agent delta ->
    (match Agents.cycle m.agents delta with
     | Some item -> select m item
     | None -> m, [])
  | Show_in_chat id -> show_in_chat m id
  | Clock now -> clock m now
  | Picker_query query -> with_picker m ~f:(fun p -> Picker.set_query p query)
  | Picker_move delta -> with_picker m ~f:(fun p -> Picker.move p delta)
  | Picker_accept -> dialog_accept m
  | Picker_choose id ->
    (match m.dialog with
     | Some (Pulls l) ->
       (match Int.of_string_opt id with
        | Some number -> open_pr ~back:l m number
        | None -> m, [])
     | Some (Picker { kind; picker }) ->
       (match
          List.find (Picker.visible picker) ~f:(fun i -> String.equal i.id id)
        with
        | Some item -> picker_accept m ~kind item
        | None -> m, [])
     | Some (Popover { chip; picker }) ->
       (match
          List.find (Picker.visible picker) ~f:(fun i -> String.equal i.id id)
        with
        | Some item -> picker_accept m ~kind:(chip_kind chip) item
        | None -> m, [])
     | Some (Review_pick p) ->
       (match Review_picker.find p ~id with
        | Some item -> review_picked m item
        | None -> m, [])
     | _ -> m, [])
  | Dialog_input text ->
    (match m.dialog with
     | Some (Rename _) -> { m with dialog = Some (Rename text) }, []
     | Some (Login flow) ->
       { m with dialog = Some (Login { flow with input = text }) }, []
     | Some (Picker _ | Popover _ | Scoped_models _) ->
       with_picker m ~f:(fun p -> Picker.set_query p text)
     | Some (Prompt prompt) -> prompt_input m (Prompt.set_input prompt text)
     | Some (Forge_host _) ->
       { m with dialog = Some (Forge_host { input = text; error = None }) }, []
     | Some (Pulls _) -> with_picker m ~f:(fun p -> Picker.set_query p text)
     | Some (Review_pick p) ->
       ( { m with dialog = Some (Review_pick (Review_picker.set_query p text)) }
       , [] )
     | _ -> m, [])
  | Dialog_move delta ->
    (match m.dialog with
     | Some (Review_pick p) ->
       { m with dialog = Some (Review_pick (Review_picker.move p delta)) }, []
     | Some (Login flow) ->
       { m with dialog = Some (Login (Login_flow.move flow delta)) }, []
     | Some (Filter menu) ->
       { m with dialog = Some (Filter (Filter_menu.move menu delta)) }, []
     | Some (Picker _ | Popover _ | Scoped_models _ | Pulls _) ->
       with_picker m ~f:(fun p -> Picker.move p delta)
     | Some (Prompt prompt) ->
       { m with dialog = Some (Prompt (Prompt.move prompt delta)) }, []
     | _ -> m, [])
  | Dialog_accept -> dialog_accept m
  | Close_dialog ->
    (match m.dialog, m.generate.running with
     (* Esc stops a draft streaming into the dialog before closing it. *)
     | Some (Pr_create _), Some { target = Pr; _ }
     | Some (New_worktree _), Some { target = Worktree_branch; _ }
     | Some (Rename _), Some { target = Session_title; _ } -> stop_generate m
     | _ ->
       (match m.dialog with
        | Some (Login { failed = None; provider; _ }) ->
          let m, cmds =
            close_dialog { m with cancelled_login = Some provider }
          in
          m, cmds @ [ rpc "auth_cancel" [] ]
        | Some (Pull { back = Some l; _ } | Pr_create { back = Some l; _ }) ->
          open_picker m (Pulls l)
        | Some (Filter menu) ->
          close_dialog { m with transcript_filter = menu.original }
        | Some _ -> close_dialog m
        | None -> m, []))
  | Login_choose i ->
    (match m.dialog with
     | Some (Login flow) ->
       let flow = Login_flow.move flow (i - flow.selected) in
       respond_login m flow
     | _ -> m, [])
  | Start_login provider -> start_login m ~provider ~method_:None
  | Logout provider -> logout m provider
  | Cancel_subagent id -> stop m (Agent id)
  | Kill_job id -> stop m (Job id)
  | Dequeue ->
    ( m
    , [ (match m.recipient with
         | Main -> rpc "dequeue" [] ~tag:Dequeued
         | Agent agent ->
           rpc "dequeue_agent" [ "agent_id", str agent ] ~tag:Dequeued)
      ] )
  | Edit_queued (recipient, q) -> unqueue m recipient q ~tag:Dequeued
  | Set_recipient recipient -> set_recipient m recipient
  | Remove_reminder name ->
    remind_action
      m
      ~command:("/remind rm " ^ name)
      (Prigh_ui.Remind_command.Action.Remove name)
  | Toggle_folded dir ->
    let folded =
      if Set.mem m.folded dir
      then Set.remove m.folded dir
      else Set.add m.folded dir
    in
    ( { m with folded }
    , save_pref
        m
        Key.folded
        (if Set.is_empty folded
         then None
         else
           Some (`Array (List.map (Set.to_list folded) ~f:(fun d -> `String d))))
    )
  | New_session_in path -> new_session_in m path
  | Generate Commit_message -> git_action m Draft
  | Generate target -> start_generate m target
  | Generate_stop -> stop_generate m
  | Generate_undo target -> undo_generate m target
  | Git action -> git_action m action
  | New_worktree -> new_worktree_dialog m
  | New_worktree_input { field; text } ->
    (match m.dialog with
     | Some (New_worktree d) ->
       ( { m with dialog = Some (New_worktree (New_worktree.set d field text)) }
       , [] )
     | _ -> m, [])
  | Open_worktree { path; new_session } ->
    with_worktree m path ~f:(fun w ->
      let m = { m with dialog = None } in
      let m, cmds = open_worktree m w ~new_session in
      m, (if Option.is_none m.dialog then [ focus_editor ] else []) @ cmds)
  | Ask_remove_worktree path -> ask_remove_session_worktree m path
  | Remove_worktree { force } -> remove_worktree m ~force
  | Open_menu ({ target = Message n; _ } as menu) ->
    (match Model.message_menu m n with
     | None -> error m message_gone
     | Some msg ->
       let m, cmds =
         open_dialog
           m
           (Picker
              { kind = Menu menu; picker = Context_menu.message_picker msg })
       in
       m, cmds @ [ Command.Place_menu menu.anchor ])
  | Open_menu menu ->
    (match
       Option.bind (Context_menu.session menu) ~f:(find_session_by_path m)
     with
     | None -> m, []
     | Some s ->
       let m, cmds =
         open_dialog
           m
           (Picker
              { kind = Menu menu
              ; picker =
                  Context_menu.picker
                    ?host_home:(Tilde.session_host_home m.state s)
                    menu.target
                    (match m.state with
                     | Some state when is_current_path m s.path ->
                       { s with home = state.home }
                     | _ -> s)
                    ~current:(is_current_path m s.path)
              })
       in
       m, cmds @ [ Command.Place_menu menu.anchor ])
  | Remove_queued (recipient, q) -> unqueue m recipient q ~tag:(Unqueued q.text)
  | Toggle_sidebar -> { m with sidebar_open = not m.sidebar_open }, []
  | Answer_confirm { call_id; agent_id; answer = Deny_with_reason } ->
    (match m.confirms with
     | first :: _
       when String.equal first.call_id call_id
            && Option.equal String.equal first.agent_id agent_id ->
       { m with confirm_reason = Some "" }, [ Command.Focus "confirm-reason" ]
     | _ -> m, [])
  | Answer_confirm { call_id; agent_id; answer } ->
    answer_confirm m ~call_id ~agent_id answer ~reason:None
  | Confirm_reason_input text ->
    (match m.confirm_reason with
     | Some _ -> { m with confirm_reason = Some text }, []
     | None -> m, [])
  | Send_confirm_reason ->
    (match m.confirms, m.confirm_reason with
     | c :: _, Some reason ->
       answer_confirm
         m
         ~call_id:c.call_id
         ~agent_id:c.agent_id
         Deny_with_reason
         ~reason:(Some reason)
     | _ -> m, [])
  | Cancel_confirm_reason ->
    { m with confirm_reason = None }, [ Command.Focus "confirm" ]
  | Question action -> question m action
  | Cycle_mode -> cycle_mode m
  | Add_image image -> { m with images = m.images @ [ image ] }, []
  | Remove_image i ->
    { m with images = List.filteri m.images ~f:(fun j _ -> j <> i) }, []
  | Show_toast { text; error } -> toast m ~error text
  | Dismiss_toast id ->
    { m with toasts = List.filter m.toasts ~f:(fun t -> t.id <> id) }, []
  | Sign_out -> m, [ Command.Sign_out ]
  | Set_accounts { accounts; current } ->
    { m with accounts; account = current }, []
  | Open_accounts -> open_dialog m (account_menu m)
  | Cycle_verbosity -> set_verbosity m (Prigh_ui.Verbosity.next m.verbosity)
  | Toggle_fold key ->
    let folds =
      if Set.mem m.folds key
      then Set.remove m.folds key
      else Set.add m.folds key
    in
    let m = { m with folds } in
    if Fold.expanded (Model.fold m) key then load_output m key else m, []
  | Load_output key -> load_output m key
  | Load_older -> load_older m
  | Prefetch_session path -> prefetch_session m path
  | Cycle_model step -> cycle_model m step
  | Cycle_thinking -> cycle_thinking m
  | Copy_last -> copy_last m
  | Close_btw -> close_btw m
  | Dialog_toggle ->
    (match m.dialog with
     | Some (Scoped_models { picker; _ }) ->
       (match Picker.selected_item picker with
        | Some item -> toggle_scoped m item.id
        | None -> m, [])
     | _ -> m, [])
  | Toggle_scoped key -> toggle_scoped m key
  | Dialog_complete ->
    with_prompt m ~f:(fun prompt -> prompt_input m (Prompt.complete prompt))
  | Choose_suggestion i ->
    with_prompt m ~f:(fun prompt ->
      let prompt = { prompt with selected = Some i } in
      prompt_input m (Prompt.complete prompt))
  | Retry_connection -> retry_connection m
  | Reload_page { keep_session } -> m, [ Command.Reload_page { keep_session } ]
  | Scroll_chat pages -> m, [ Command.Scroll_chat pages ]
  | Chat_scrolled { at_bottom } -> { m with scrolled_up = not at_bottom }, []
  | Jump_to_bottom -> to_bottom m
  | Jump_to_user_message dir -> m, [ Command.Jump_to_user_message dir ]
  | Jump_to target -> jump_to m target
  | Jump_older -> jump_older m
  | Ask_trim target -> ask_trim m target
  | Show_trimmed show -> show_trimmed m show
  | Untrim -> untrim m
  | Ask_delete_message n -> ask_delete_message m n
  | Ask_trim_after n -> ask_trim_after m n
  | Restore_hidden h -> restore_hidden m h
  | Restore_range entry -> restore_range m entry
  | Show_hidden { entry; show } -> show_hidden m ~entry ~show
  | Open_jump_list ->
    (match jump_points m with
     | points
       when List.exists points ~f:(fun (p : Jump.Point.t) ->
              Jump.Kind.equal p.kind User) ->
       open_picker
         m
         (Picker
            { kind = Jump
            ; picker = Jump.picker points ~times:(Model.message_time m)
            })
     | _ -> toast m "No messages of yours to jump to yet: send one first")
  | Run command ->
    (match Slash.parse command with
     | Some parsed ->
       let m', cmds = run_command { m with dialog = None } parsed in
       (* A setting changed from the settings page leaves it open. *)
       (match m.dialog, m'.dialog with
        | Some (Settings | Auth _), None -> { m' with dialog = m.dialog }, cmds
        | Some Settings, Some (Login _ | Forge_host _ | Nix_gc_confirm _) ->
          { m' with return_to_settings = true }, cmds
        | _ -> m', cmds)
     | None -> m, [])
  | Toggle_terminal ->
    if m.terminal.open_ then close_terminal m else open_terminal m
  | Close_terminal -> close_terminal m
  | New_shell -> new_shell m
  | Terminal_status { key; status } ->
    with_terminal m ~f:(fun t -> { t with status = Some (key, status) }), []
  | Files action -> with_files m (Files.update m.files action)
  | Review action -> review_update m action
  | Review_pick which -> open_review_pick m which
  | Review_direct direct -> review_direct m direct
  | Review_checkout -> review_checkout m
  | Review_worktree { path; base } ->
    let m, close =
      match m.dialog with
      | Some (Picker { kind = Worktrees _; _ }) -> close_dialog m
      | _ -> m, []
    in
    let m, cmds = review_worktree m ~dir:(Some path) ~base in
    m, close @ cmds
  | Resize action ->
    let layout, saves = Resize.update m.layout (Model.shown m) action in
    ( { m with layout }
    , save_prefs
        m
        (List.map saves ~f:(fun ({ key; value } : Resize.Save.t) ->
           { Prefs.Change.key; value = Option.map value ~f:Json.int })) )
  | Pr_filter filter ->
    let query =
      match m.dialog with
      | Some (Pulls l) -> Picker.query l.picker
      | _ -> ""
    in
    list_prs m (Pull_requests.create ~query filter)
  | Pr_open number ->
    (match m.dialog with
     | Some (Pulls l) -> open_pr ~back:l m number
     | _ -> open_pr m number)
  | Pr_checkout number -> checkout_pr m number
  | New_pr ->
    (match m.dialog with
     | Some (Pulls l) -> new_pr ~back:l m ~title:""
     | _ -> new_pr m ~title:"")
  | Pr_form action ->
    (match m.dialog with
     | Some (Pr_create f) when not f.busy ->
       { m with dialog = Some (Pr_create (Pr_create.update f action)) }, []
     | _ -> m, [])
  | Preview_accent colour ->
    (match Theme.Accent.of_string colour with
     | Some accent ->
       { m with theme_preview = Some { m.theme with accent } }, []
     | None -> m, [])
;;

(* Once a connection has brought the session back (its state and messages),
   it counts as working again. *)
let settle (m : Model.t) cmds =
  match m.connection, m.state with
  | Connected, Some _
    when m.drops.settling && (not m.drops.resyncing) && not m.pending.messages
    ->
    let rejoined = m.drops.rejoined in
    let m = { m with drops = Drops.settled m.drops } in
    if rejoined
    then (
      let m, toast_cmds = toast m "Reconnected" in
      m, cmds @ toast_cmds)
    else m, cmds
  | _ -> m, cmds
;;

let update (m : Model.t) (action : Action.t) =
  let m', cmds = update_connected m action in
  let m', cmds = follow_worktrees m (m', cmds) in
  let m', cmds = follow_recipient (m', cmds) in
  let m', cmds = settle m' cmds in
  let look = Model.look m' in
  if
    Theme.equal look (Model.look m)
    || List.mem cmds (Command.Apply_theme look) ~equal:Command.equal
  then m', cmds
  else m', cmds @ [ Command.Apply_theme look ]
;;
