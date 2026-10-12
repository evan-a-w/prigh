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
    | Pending
    | Sent of
        { text : string
        ; images : Image.t list
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
    | Reconnect of int
    | Config
    | Config_saved of string
    | Fallback_saved
    | Default_dir_saved
    | Default_saved
    | Session_stats
    | Entries of Entries_purpose.t
    | Reload_messages
    | Exported
    | Imported
    | Prompt_done of string
    | Prompt_paths of string
    | Btw of string
    | Users of Users_purpose.t
    | User_switched
    | Skills of
        { place : string
        ; purpose : Skills_purpose.t
        }
    | Mcp of Mcp_purpose.t
    | Mode_set of string
    | Model_set
    | Thinking_set
    | Git_diff
    | Command_prompt of string (** [/review] or [/init] as typed *)
    | Files of Files.Reply.t
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
    | Session_gone of
        { generation : int
        ; session : string
        } (** reconnected, but the backend no longer has [session] *)
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
    | Save_history of string list
    | Sign_out
    | Reveal of string list
    (** scroll the main chat to a subagent's card: the call ids from the
        top-level one down *)
    | Copy of string
    | Switch_account of Accounts.Account.t
    | Add_account
    | Scroll_chat of int
    | Jump_to_user_message of int
    | Scroll_to_bottom
    | Focus_terminal
    | Remember_terminal of bool
    | Notify of Prigh_ui.Notify.t
    | Request_notification_permission
    | Save_notifications of bool
    | Set_title of string
    | Files of Files.Page.t
    | Save_size of Resize.Save.t
    | Keep_scroll_position
    (** older messages are being added above: what shows stays in place *)
    | Probe_connection
    (** the socket may have died unnoticed (a suspended page, a new network):
        drop it unless the backend answers soon *)
    | Drop_connection
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
    else Int.min 10_000 (250 * (1 lsl Int.min 10 (attempt - 1)))
  ;;
end

module Toast = Toast

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
    | Set_notifications of
        { enabled : bool
        ; permission : string
        }
    | Notification_permission of string
    | Load_history of string list
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
    | Open_model_picker
    | Open_thinking_picker
    | Open_help
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
    | Edit_queued of Queued.Message.t
    | Remove_queued of Queued.Message.t
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
    | Scroll_chat of int
    | Jump_to_user_message of int
    | Chat_scrolled of { at_bottom : bool }
    | Jump_to_bottom
    | Run of string
    | Toggle_terminal
    | Reopen_terminal
    | Close_terminal
    | New_shell
    | Terminal_status of
        { key : string
        ; status : Terminal.Status.t
        }
    | Files of Files.Action.t
    | Resize of Resize.Action.t
    | Load_older
    | Prefetch_session of string (** a path *)
    | Load_output of string (** a [Fold] key *)
    | Network of bool (** the browser's [online] and [offline] events *)
  [@@deriving sexp_of]
end

module Model = struct
  type t =
    { connection : Connection.t
    ; generation : int
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
    ; config : Config.t option
    ; btw : Btw.t option
    ; btw_seq : int
    ; accounts : Accounts.Account.t list
    ; account : Accounts.Account.t option
    ; users : string list option
    ; terminal : Terminal.t
    ; skills : Skills.t
    ; pending : In_flight.t
    ; marks : Session_marks.t
    ; notifications : Page_notify.t
    ; files : Files.t
    ; layout : Resize.t
    ; paging : Paging.t
    ; chat_cache : Chat_cache.t
    ; outputs_loading : String.Set.t (** [Fold] keys *)
    ; session_gone : string option
      (** reconnected to a backend that no longer has this session: its
          transcript stays on show, read-only *)
    }
  [@@deriving sexp_of]

  let shown t =
    { Resize.Shown.sidebar = t.sidebar_open
    ; files = t.files.open_
    ; agents = t.agents.open_
    ; narrow = t.narrow
    }
  ;;

  let running t =
    match t.state with
    | Some s -> s.running
    | None -> false
  ;;

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
    && Agents.running_agents t.agents + Agents.running_jobs t.agents > 0
  ;;

  let fold t = Fold.create ~verbosity:t.verbosity ~toggled:t.folds

  let message_time t =
    Message_time.create
      ~now:(Option.value t.now ~default:Time_ns.epoch)
      ~utc_offset:t.utc_offset
  ;;
end

let thinking_levels = Prigh_ui.Commands.thinking_levels

let init =
  { Model.connection = Connected
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
  ; config = None
  ; btw = None
  ; btw_seq = 0
  ; accounts = []
  ; account = None
  ; users = None
  ; terminal = Terminal.closed
  ; skills = Skills.empty
  ; pending = In_flight.none
  ; marks = Session_marks.empty
  ; notifications = Page_notify.init
  ; files = Files.closed
  ; layout = Resize.empty
  ; paging = Paging.all
  ; chat_cache = Chat_cache.empty
  ; outputs_loading = String.Set.empty
  ; session_gone = None
  }
;;

let rpc ?(tag = Reply_tag.Show_error) method_ params =
  Command.Rpc { method_; params; tag }
;;

let call ?tag (method_ : Request.Method.t) =
  rpc ?tag (Request.Method.name method_) (Request.Method.params method_)
;;

let str s = `String s

let toast (m : Model.t) ?(error = false) ?replaces text =
  let toasts, toast =
    Toast.push m.toasts ~id:m.next_toast ?replaces ~error text
  in
  ( { m with toasts; next_toast = m.next_toast + 1 }
  , match Toast.lifetime_ms toast with
    | None -> []
    | Some after_ms -> [ Command.Expire_toast { id = toast.id; after_ms } ] )
;;

let error m text = toast m ~error:true text

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

let reading_changes = "Reading the changes…"
let reading_review = "Reading the changes to review…"
let focus_editor = Command.Focus "editor"

let startup =
  [ rpc "get_state" [] ~tag:State
  ; rpc "list_models" [] ~tag:Models
  ; rpc "auth_status" [] ~tag:(Auth_status Refresh)
  ; rpc "get_config" [] ~tag:Config
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

let close_dialog (m : Model.t) = { m with dialog = None }, [ focus_editor ]
let list_subagents = rpc "list_subagents" [] ~tag:Subagents
let list_jobs = rpc "list_jobs" [] ~tag:Jobs

let cancel_btw (m : Model.t) =
  match m.btw with
  | Some { status = Streaming; id; _ } ->
    [ rpc "btw_cancel" [ "btw_id", str id ] ~tag:Ignore ]
  | _ -> []
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

let with_files (m : Model.t) (files, outs) = files_out { m with files } outs

let files_where (m : Model.t) =
  Option.map m.state ~f:(fun (state : State.t) ->
    let name, _ = Terminal.where state in
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
          Chat.set_running
            (Option.value_map cached ~default:Chat.empty ~f:(fun c -> c.chat))
            state.running
      ; paging =
          Option.value_map cached ~default:Paging.all ~f:(fun c -> c.paging)
      ; outputs_loading = String.Set.empty
      ; folds = String.Set.empty
      ; confirms = []
      ; confirm_reason = None
      ; questions = []
      ; queue = Queued.empty
      ; dialog = Option.filter m.dialog ~f:(Fn.non Dialog.per_session)
      ; agents = { Agents.empty with open_ = m.agents.open_ && not m.narrow }
      ; btw = None
      ; completion = None
      ; scrolled_up = false
      ; skills = Skills.empty
      ; toasts = List.filter m.toasts ~f:(fun t -> not t.error)
      ; session_gone = None
      }
    , cancel_btw m
      @ [ Command.Set_url_session state.session_id ]
      @ fetch
      @ (if Option.is_some cached then [ Command.Scroll_to_bottom ] else [])
      @ [ rpc "get_pending" [] ~tag:Pending
        ; rpc "list_sessions" [] ~tag:Sessions
        ; list_subagents
        ; list_jobs
        ] ))
  else { m with chat = Chat.set_running m.chat state.running }, background_changed
;;

let set_state (m : Model.t) (state : State.t) =
  let m, cmds = set_state_without_files m state in
  let m, more = with_files m (Files.set_where m.files (files_where m)) in
  m, cmds @ more
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

let model_picker ?(query = "") (m : Model.t) =
  let current = Option.map m.state ~f:(fun s -> s.model.key) in
  let known = not (List.is_empty m.auth) in
  let usable (model : Llm.t) = (not known) || logged_in m model.provider in
  let models =
    List.stable_sort m.models ~compare:(fun a b ->
      Bool.compare (usable b) (usable a))
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
  Dialog.Picker
    { kind = Models; picker = Picker.create ~query ~title:"Switch model" items }
;;

let thinking_picker (m : Model.t) =
  let current = Option.value_map m.state ~default:"" ~f:(fun s -> s.thinking) in
  let detail = function
    | "off" -> "answer straight away"
    | "low" -> "a little"
    | "on" -> "the provider's default budget"
    | "high" -> "more"
    | "max" -> "as much as the model allows"
    | _ -> ""
  in
  Dialog.Picker
    { kind = Thinking
    ; picker =
        Picker.create
          ~title:"Thinking level"
          (List.map thinking_levels ~f:(fun level ->
             Picker.Item.create
               ~id:level
               ~detail:(detail level)
               ~marked:(String.equal level current)
               level))
    }
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

let login_picker (statuses : Auth_status.t list) =
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
  in
  Dialog.Picker
    { kind = Login; picker = Picker.create ~title:"Log in to a provider" items }
;;

let logout_picker (statuses : Auth_status.t list) =
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
  |> function
  | [] -> None
  | items ->
    Some
      (Dialog.Picker
         { kind = Logout
         ; picker = Picker.create ~title:"Log out of a provider" items
         })
;;

let open_dialog (m : Model.t) ?(focus = "dialog") dialog =
  { m with dialog = Some dialog; completion = None }, [ Command.Focus focus ]
;;

let open_picker m dialog = open_dialog m ~focus:"picker-input" dialog

(* A dialog that needs a list from the backend shows at once, saying that it
   is loading; the reply replaces it (see [loads]). *)
let loading (m : Model.t) title cmds =
  let m, focus = open_dialog m (Loading title) in
  m, focus @ cmds
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
  | _ -> open_picker m (thinking_picker m)
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
      ~sessions:m.sessions
      ~hosts:(Option.value_map m.state ~default:[] ~f:(fun s -> s.hosts))
      ~users:(Option.value m.users ~default:[])
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

(* The sidebar's sessions in the order it shows them: active, then archived. *)
let shown_sessions (m : Model.t) =
  Session_list.shown
    m.sessions
    ~query:m.session_query
    ~archived_open:m.archived_open
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
         "No session %S: give its name, id or path (Ctrl+K searches them)."
         key)
  | several ->
    Error
      (sprintf
         "%S matches %d sessions (%s): give more of the id."
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
         "%S is running: wait for it or stop it (Esc in it) before archiving."
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

(* ---- the terminal panel *)

let with_terminal (m : Model.t) ~f = { m with terminal = f m.terminal }

(* The shell takes the keyboard (Ctrl+` gives it back); on a phone the panel
   covers the page, the sidebar's drawer included. *)
let open_terminal (m : Model.t) =
  ( { m with
      terminal = { m.terminal with open_ = true }
    ; sidebar_open = m.sidebar_open && not m.narrow
    }
  , (if m.terminal.open_ then [] else [ Command.Remember_terminal true ])
    @ [ Command.Focus_terminal ] )
;;

let close_terminal (m : Model.t) =
  ( with_terminal m ~f:(fun t -> { t with open_ = false })
  , [ Command.Remember_terminal false; focus_editor ] )
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

(* Top-level subagents' transcripts come with the chat (events, or
   [get_subagent] after a reload); nested ones are fetched when shown. *)
let fetch_transcript (m : Model.t) id =
  Agents.lineage m.agents id
  |> List.filter ~f:(fun (a : Agents.Agent.t) ->
    Option.is_none (Chat.find_subagent m.chat a.id))
  |> List.map ~f:(fun (a : Agents.Agent.t) -> subagent_rpc a.id)
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
  | Agent id -> m, fetch_transcript m id
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

let clock (m : Model.t) now =
  let m = { m with now = Some now } in
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
        toast m ~replaces:"Model: " (sprintf "Model: %s" next.name)
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
        toast m ~replaces:"Thinking: " (sprintf "Thinking: %s" next)
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
  toast
    { m with verbosity; folds = String.Set.empty }
    ~replaces:"Transcript: "
    (sprintf
       "Transcript: %s, %s (Ctrl+O cycles)"
       (Prigh_ui.Verbosity.name verbosity)
       (verbosity_detail verbosity))
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

let mode_picker (m : Model.t) =
  current_state m ~f:(fun s ->
    let current = Session_mode.of_state s in
    open_picker
      m
      (Dialog.Picker
         { kind = Modes
         ; picker =
             Picker.create
               ~title:"When tools ask first (Alt+M cycles)"
               (List.map (Session_mode.choices ~current) ~f:(fun mode ->
                  Picker.Item.create
                    ~id:(Session_mode.label mode)
                    ~detail:(Session_mode.describe mode)
                    ~search:
                      (Session_mode.label mode ^ " " ^ Session_mode.title mode)
                    ~marked:(Session_mode.equal mode current)
                    (Session_mode.title mode)))
         }))
;;

let mode_by_name (m : Model.t) name =
  current_state m ~f:(fun s ->
    match Session_mode.of_name ~current:(Session_mode.of_state s) name with
    | Some mode -> set_mode m mode
    | None ->
      error
        m
        (sprintf
           "Unknown mode %S: use %s (/mode picks one)."
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
           "Unknown argument %S: use /plan on, /plan off, or /plan to toggle."
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
           "No tool host %S: one of %s (/host picks one)."
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

let submit_prompt (m : Model.t) (prompt : Prompt.t) =
  let path = String.strip prompt.input in
  let busy = { m with dialog = Some (Prompt { prompt with busy = true }) } in
  match prompt.action, path with
  | Export, path ->
    let m, cmds = export busy path in
    m, cmds
  | (Cd | Host_cwd _ | Import), "" ->
    ( { m with
        dialog =
          Some
            (Prompt { prompt with error = Some "Type a path (Esc cancels)." })
      }
    , [] )
  | Cd, path ->
    ( busy
    , [ rpc
          "set_cwd"
          [ "path", str path ]
          ~tag:(Prompt_done ("Working directory: " ^ path))
      ] )
  | Host_cwd { host; name }, path ->
    ( busy
    , [ rpc
          "set_active_host"
          [ "host", str host; "cwd", str path ]
          ~tag:(Prompt_done (sprintf "Tools run on %s, in %s" name path))
      ] )
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

let prompt_succeeded (m : Model.t) =
  match m.dialog with
  | Some (Prompt { busy = true; _ }) -> close_dialog m
  | _ -> m, []
;;

let help_command (m : Model.t) name =
  let name = String.chop_prefix_if_exists name ~prefix:"/" in
  match Slash.find name with
  | Some spec -> toast m (sprintf "%s: %s" (Slash.Spec.usage spec) spec.help)
  | None ->
    error
      m
      (match Slash.closest name with
       | Some spec -> sprintf "No command /%s. Did you mean /%s?" name spec.name
       | None -> sprintf "No command /%s: /help lists them." name)
;;

let retry_connection (m : Model.t) =
  match m.connection with
  | Connected -> toast m "The backend is connected."
  | Reconnecting { attempt; _ } ->
    let generation = m.generation + 1 in
    let m =
      { m with connection = Reconnecting { attempt; generation }; generation }
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
  let save = Command.Save_notifications enabled in
  if not enabled
  then (
    let m, cmds = toast m "Notifications off: /notify on turns them on" in
    m, save :: cmds)
  else (
    match m.notifications.permission with
    | Default -> m, [ save; Command.Request_notification_permission ]
    | Granted ->
      let m, cmds = toast m (Page_notify.describe m.notifications) in
      m, save :: cmds
    | Denied | Unsupported ->
      let m, cmds = error m (Page_notify.describe m.notifications) in
      m, save :: cmds)
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
                    ~detail:"stop telling you what happens while the tab is hidden"
                    "Turn notifications off"
                else
                  Picker.Item.create
                    ~id:"notify on"
                    ~detail:"when a turn ends or something waits for you"
                    "Turn notifications on")
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
  let cancel = cancel_btw m in
  let m =
    { m with
      hello = Some hello
    ; state = None
    ; chat = Chat.empty
    ; paging = Paging.all
    ; chat_cache = Chat_cache.empty
    ; sessions = []
    ; marks = Session_marks.empty
    ; session_query = ""
    ; confirms = []
    ; confirm_reason = None
    ; questions = []
    ; queue = Queued.empty
    ; dialog = None
    ; completion = None
    ; btw = None
    ; images = []
    ; skills = Skills.empty
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
  m, cancel @ cmds @ [ focus_editor ] @ startup
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

let run_command (m : Model.t) ({ name; rest } : Slash.Parsed.t) =
  let none = "" in
  let words =
    String.split rest ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
  in
  match name, rest with
  | "help", "" -> open_dialog m Help
  | "help", name -> help_command m name
  | "hotkeys", _ -> open_dialog m Hotkeys
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
            "No model matches %S.%s Ctrl+L lists them all."
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
           "Unknown thinking level %S: use %s."
           level
           (String.concat ~sep:", " thinking_levels))
  | "change_default", _ -> m, [ rpc "change_default" [] ~tag:Default_saved ]
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
         (sprintf "Unknown verbosity %S: use quiet, normal or verbose." name))
  | "confirm", "" -> open_picker m (confirm_picker m)
  | "mode", "" -> mode_picker m
  | "mode", name -> mode_by_name m name
  | "plan", arg -> plan_mode m arg
  | "todos", _ -> show_plan m
  | "permissions", _ -> show_permissions m
  | "confirm", ("on" | "true") -> set_confirm m true
  | "confirm", ("off" | "false") -> set_confirm m false
  | "confirm", other ->
    error m (sprintf "Unknown setting %S: use on or off." other)
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
         "Unknown /resume argument %S: /resume now retries at once, /resume \
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
  | "review", args ->
    let m, cmds = toast m reading_review in
    ( m
    , cmds
      @ [ rpc
            "review"
            [ "args", str args ]
            ~tag:(Command_prompt (String.strip ("/review " ^ args)))
        ] )
  | "init", _ -> m, [ rpc "init" [] ~tag:(Command_prompt "/init") ]
  | "notify", "" -> toast m (Page_notify.describe m.notifications)
  | "notify", "on" -> set_notifications m true
  | "notify", "off" -> set_notifications m false
  | "notify", other ->
    error
      m
      (sprintf
         "Unknown /notify argument %S: /notify on or /notify off (/notify \
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
        let path = Option.value_map m.state ~default:"" ~f:(fun s -> s.session_path) in
        match find_session_by_path m path with
        | Some s -> Ok s
        | None -> Error "This session isn't saved yet: send a message first.")
      else find_session m key
    in
    (match session with
     | Ok s -> set_archived m s ~archived
     | Error e -> prompt_failed m e)
  | "delete", "" ->
    let m, cmds =
      toast m "Click the sessions to delete (Shift+click for a range), then Delete."
    in
    { m with sidebar_open = true; marks = { m.marks with selecting = true } }, cmds
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
  | "cd", path ->
    ( m
    , [ rpc
          "set_cwd"
          [ "path", str path ]
          ~tag:(Notice ("Working directory: " ^ path))
      ] )
  | "default-dir", "" ->
    with_config m ~f:(fun c -> toast m (default_dir_summary c))
  | "default-dir", "off" -> set_default_dir m None
  | "default-dir", dir ->
    (match default_dir m dir with
     | Ok dir -> set_default_dir m (Some dir)
     | Error message -> error m message)
  | "host", "" -> hosts_picker m
  | "host", arg -> switch_host m arg
  | "export", "" -> open_prompt m (Prompt.create Export)
  | "export", path -> export m path
  | "import", "" -> open_prompt m (Prompt.create Import)
  | "import", path -> m, [ rpc "import" [ "path", str path ] ~tag:Imported ]
  | ("skills" | "skill:"), query -> open_skills m ~query
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
         "Unknown /mcp argument %S: /mcp lists the servers, /mcp reconnect \
          restarts the failed ones."
         other)
  | "copy", _ -> copy_last m
  | "btw", "" -> error m "Usage: /btw <question> (asked aside; the run goes on)"
  | "btw", question -> start_btw m question
  | "abort", _ -> m, [ rpc "abort" [] ~tag:Restored ]
  | "files", "" -> with_files m (Files.update m.files (Open None))
  | "files", path -> with_files m (Files.update m.files (Open (Some path)))
  | "terminal", "" -> open_terminal m
  | "terminal", _ ->
    error m "Usage: /terminal (Ctrl+` opens and closes it; × in its header too)"
  | "agents", "" -> open_agents m None
  | "agents", _ ->
    (match words with
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
    loading
      m
      "Log in to a provider"
      [ rpc "auth_status" [] ~tag:(Auth_status Login_picker) ]
  | "login", args ->
    (match
       String.split args ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
     with
     | provider :: method_ :: _ ->
       start_login m ~provider ~method_:(Some method_)
     | _ -> start_login m ~provider:args ~method_:None)
  | "logout", "" ->
    loading
      m
      "Log out of a provider"
      [ rpc "auth_status" [] ~tag:(Auth_status Logout_picker) ]
  | "logout", provider -> logout m provider
  | "auth", _ ->
    loading m "Providers" [ rpc "auth_status" [] ~tag:(Auth_status Show) ]
  | "setusr", "" ->
    loading m "Act as" [ rpc "list_users" [] ~tag:(Users Picker) ]
  | "setusr", user ->
    (match words with
     | [ user ] -> act_as m user
     | _ -> error m (sprintf "Usage: /setusr [user] (not %S)" user))
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
      (match Slash.closest name with
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
    , [ rpc "tool_confirm_respond" (Confirm_prompt.params c answer ~reason) ]
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
       , [ rpc "ask_user_respond" (Question_dialog.answer_params q answers)
         ; focus_waiting m
         ]
         @ toast_cmds )
     | `Decline ->
       let m = { m with questions = rest } in
       let m, toast_cmds =
         toast m "Declined to answer: the model goes on without one"
       in
       ( m
       , [ rpc "ask_user_respond" (Question_dialog.decline_params q)
         ; focus_waiting m
         ]
         @ toast_cmds ))
  | _ -> m, []
;;

(* A [get_subagent] reply. *)
let subagent_of_json json =
  let open Or_error.Let_syntax in
  let%bind summary = Json.object_field json "subagent" in
  let%bind agent_id = Json.string_field summary "id" in
  let%bind call_id = Json.string_field summary "call_id" in
  let%bind parent = Json.string_opt_field summary "parent" in
  let%bind task = Json.string_field summary "task" in
  let%bind model = Json.string_field summary "model" in
  let%bind turns = Json.int_field summary "turns" in
  let%bind result =
    match Json.field summary "result" with
    | None -> Ok None
    | Some r -> Or_error.map (Event.Subagent_result.of_json r) ~f:Option.some
  in
  let%map messages = Json.list_field json "messages" ~f:Message.of_json in
  ( { Chat.Subagent.agent_id
    ; task
    ; model
    ; chat = Chat.of_messages ~running:(Option.is_none result) messages
    ; turns
    ; cost_usd = None
    ; result
    }
  , `Parent parent
  , `Call call_id )
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
    | true, Some _, _ when phys_equal entry.chat m.chat && not m.pending.messages
      -> m, prefetch
    | true, Some _, (`Unchanged | `Changed | `New) ->
      ( { m with
          chat = entry.chat
        ; paging = entry.paging
        ; scrolled_up = false
        ; pending = { m.pending with messages = false }
        }
      , (Command.Scroll_to_bottom
         :: List.map (Chat.subagents_to_load entry.chat) ~f:subagent_rpc)
        @ prefetch ))
;;

let older_page (m : Model.t) path ~before (page : Message_page.t) =
  if
    is_current m path
    && m.paging.loading
    && Option.equal String.equal m.paging.before (Some before)
  then (
    let loaded = String.Set.of_list (Chat.subagents_to_load m.chat) in
    let chat = Chat.prepend m.chat page.messages in
    ( { m with chat; paging = Paging.of_page page }
    , Command.Keep_scroll_position
      :: List.filter_map (Chat.subagents_to_load chat) ~f:(fun call_id ->
        Option.some_if (not (Set.mem loaded call_id)) (subagent_rpc call_id)) ))
  else m, []
;;

let load_older (m : Model.t) =
  match session_path m, Paging.next m.paging with
  | Some path, Some before ->
    ( { m with paging = { m.paging with loading = true } }
    , [ page_rpc path ~before:(Some before) ] )
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
  (if m.narrow then [] else [ focus_editor ]) @ startup @ probe
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

  let rejoined (m : Model.t) json =
    let m, toast_cmds = toast (connected m json) "Reconnected" in
    match m.state with
    | None -> m, toast_cmds @ start_commands m
    | Some state ->
      ( m
      , toast_cmds
        @ startup
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
    if List.is_empty (Chat.entries m.chat)
    then { m with state = None }, start_commands m
    else
      ( { m with session_gone = Some session }
      , [ rpc "list_models" [] ~tag:Models
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
    | Reconnecting _ ->
      let generation = m.generation + 1 in
      ( { m with
          connection = Reconnecting { attempt = 0; generation }
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

let rec reply_to (m : Model.t) (tag : Reply_tag.t) result =
  match tag, result with
  | Reconnect generation, _ when generation <> m.generation -> m, []
  | Files reply, result -> with_files m (Files.reply m.files reply result)
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
      ( { m with
          chat
        ; paging = Paging.of_page page
        ; pending = { m.pending with messages = false }
        }
      , (if loading then [ Command.Scroll_to_bottom ] else [])
        @ List.map (Chat.subagents_to_load chat) ~f:subagent_rpc ))
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
        }
        ~confirms:pending.confirms
        ~questions:pending.questions)
  (* Subagents from before the backend restarted are gone: the report stays. *)
  | ( ( Ignore | Paths _
      | Auth_status Refresh
      | Subagent _ | Subagents | Jobs | Job_output _ | Config | Prompt_paths _
      | Users Probe )
    , Error _ ) -> m, []
  | Btw id, Error e -> update_btw m id ~f:(fun b -> Btw.fail b e), []
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
  | (Prompt_done _ | Exported | Imported), Error e -> prompt_failed m e
  | Fallback_saved, Error e ->
    error m (sprintf "Couldn't set the fallback models: %s" e)
  | Default_dir_saved, Error e ->
    error m (sprintf "Couldn't set the default directory: %s" e)
  | Sessions_deleted, Error e ->
    error m (sprintf "Couldn't delete: %s" e)
  | Archived_set { title; archived }, Error e ->
    let m, cmds =
      error
        m
        (sprintf
           "Couldn't %s %S: %s"
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
  | Reload_state, Error e ->
    error
      { m with pending = { m.pending with session = None } }
      (sprintf "Couldn't open the session: %s" e)
  (* Lost with the connection: the reconnect's [Resync] brings the messages
     (and older pages are asked for again on scrolling up). *)
  | Page { path; before = None }, Error "backend closed" ->
    { m with chat_cache = Chat_cache.set_loading m.chat_cache path false }, []
  | Page { path; before = Some _ }, Error "backend closed"
    when is_current m path ->
    { m with paging = { m.paging with loading = false } }, []
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
  | Page { path; before = Some _ }, Error e when is_current m path ->
    error
      { m with paging = { m.paging with loading = false } }
      (sprintf "Couldn't load earlier messages: %s. Scroll up to try again." e)
  | Tool_output { agent_id; call_id }, Error e ->
    error
      { m with
        outputs_loading =
          Set.remove m.outputs_loading (output_key ~agent_id ~call_id)
      }
      (sprintf "Couldn't load the whole output: %s. Load them tries again." e)
  | Model_set, Error e ->
    error
      { m with pending = { m.pending with model = None } }
      (String.capitalize e)
  | Thinking_set, Error e ->
    error
      { m with pending = { m.pending with thinking = None } }
      (String.capitalize e)
  (* The connection went with the call: the banner says so. *)
  | _, Error "backend closed" -> m, []
  | _, Error e -> error m (String.capitalize e)
  | Sent _, Ok _ ->
    { m with pending = In_flight.answered m.pending ~failed:None }, []
  | Model_set, Ok _ -> { m with pending = { m.pending with model = None } }, []
  | Thinking_set, Ok _ ->
    { m with pending = { m.pending with thinking = None } }, []
  | (Ignore | Show_error | Login_started), Ok _ -> m, []
  | Notice text, Ok _ -> toast m text
  (* The state we asked for, also after switching session: what is shown
     next is up to date. *)
  | State, Ok json ->
    decode
      m
      json
      State.of_json
      ~f:(set_state { m with pending = { m.pending with session = None } })
  (* The backend answers a change of session with the new state; an older
     one only with [{}]. *)
  | Reload_state, Ok json ->
    (match field json "state" with
     | Some state ->
       decode
         m
         state
         State.of_json
         ~f:(set_state { m with pending = { m.pending with session = None } })
     | None -> m, [ rpc "get_state" [] ~tag:State ])
  | Page { path; before = None }, Ok json ->
    decode m json Message_page.of_json ~f:(newest_page m path)
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
        }
        ~confirms:pending.confirms
        ~questions:pending.questions)
  | Mode_set notice, Ok json ->
    decode m json State.of_json ~f:(fun state ->
      let m, cmds = set_state m state in
      let m, toast_cmds = toast m ~replaces:"Mode: " notice in
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
  | Subagent _, Ok json ->
    (match subagent_of_json json with
     | Ok (subagent, `Parent parent, `Call call_id) ->
       ( { m with
           chat = Chat.set_nested_subagent m.chat ~parent ~call_id subagent
         }
       , [] )
     | Error _ -> m, [])
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
      ( { m with
          sessions
        ; marks =
            Session_marks.prune
              m.marks
              ~paths:(List.map sessions ~f:(fun s -> s.path))
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
      | Login_picker -> open_picker m (login_picker auth)
      | Logout_picker ->
        (match logout_picker auth with
         | Some dialog -> open_picker m dialog
         | None -> toast m "No provider is logged in: /login logs in to one."))
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
          (List.map result.deleted ~f:(fun (d : Delete_result.Deleted.t) -> d.path))
      in
      let m =
        { m with
          sessions =
            List.filter m.sessions ~f:(fun s -> not (Set.mem gone s.path))
        ; marks =
            (match result.refused with
             | [] -> Session_marks.empty
             | _ -> { m.marks with marked = Set.diff m.marks.marked gone })
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
       then sprintf "Archived %S (a new message unarchives it)" title
       else sprintf "Unarchived %S" title)
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
  | Prompt_done notice, Ok _ ->
    let m, close = prompt_succeeded m in
    let m, cmds = toast m notice in
    m, close @ cmds
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
;;

(* The replies that a [loading] dialog waits for. *)
let loads : Reply_tag.t -> bool = function
  | Auth_status (Login_picker | Logout_picker | Show)
  | Session_stats | Entries _
  | Users Picker
  | Skills { purpose = Picker _; _ }
  | Mcp Picker -> true
  | _ -> false
;;

let reply (m : Model.t) (tag : Reply_tag.t) result =
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

let main_event (m : Model.t) (event : Event.t) =
  match event with
  | State state -> set_state m state
  | Notice text -> toast m text
  | Queue_update { steer; follow_up } ->
    { m with queue = Queued.update m.queue ~steer ~follow_up }, []
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
  | Ask_user_done call_id ->
    if List.exists m.questions ~f:(fun q -> String.equal q.call_id call_id)
    then (
      let m, cmds = remove_question m call_id in
      let m, toast_cmds =
        toast m "Question answered elsewhere, or the run stopped"
      in
      m, cmds @ toast_cmds)
    else m, []
  | Tool_end { call; _ } ->
    (* An older backend's way to say a confirmation is no longer waited for. *)
    remove_confirm m ~call_id:call.id ~agent_id:None
  | Sessions_changed -> m, [ rpc "list_sessions" [] ~tag:Sessions ]
  (* Another client's /confirm, /scoped-models or defaults. *)
  | Config_changed config -> { m with config = Some config }, []
  | Auth e -> auth_event m e
  | Btw_delta { btw_id; delta } ->
    update_btw m btw_id ~f:(fun b -> Btw.append b delta), []
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
    }
  in
  let m, cmds = main_event m e in
  m, cmds @ if Agents.starts_or_ends e then [ list_subagents ] else []
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
    ( { (set_draft m "") with history; completion = None }
    , [ Command.Save_history (History.to_list history) ] )
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
  | Some c ->
    let draft, cursor = Completion.accept c ~text:m.draft in
    let runs =
      run
      &&
      match Completion.source c, Completion.selected_item c with
      | Command, Some item ->
        (* Optional free text (e.g. /compact's instructions) can be left out;
           an argument with completions is offered next. *)
        (match Slash.find item.id with
         | Some { args; argument = None; _ } ->
           String.is_empty args || String.is_prefix args ~prefix:"["
         | Some { argument = Some _; _ } | None -> false)
      | Argument (Directory | Backend_directory | Path | Skill), _ -> false
      | Argument _, _ -> true
      | (Command | Path), _ -> false
    in
    if runs
    then send (set_draft m (String.strip draft)) ~follow_up:false
    else edit m ~cursor draft
;;

let picker_accept (m : Model.t) ~kind (item : Picker.Item.t) =
  let m = { m with dialog = None } in
  let m, cmds =
    match (kind : Dialog.Picker_kind.t) with
    | Models -> set_model m item.id
    | Thinking -> set_thinking m item.id
    | Login ->
      (match String.lsplit2 item.id ~on:' ' with
       | Some (provider, method_) ->
         start_login m ~provider ~method_:(Some method_)
       | None -> start_login m ~provider:item.id ~method_:None)
    | Logout -> logout m item.id
    | Verbosity ->
      (match
         List.find Prigh_ui.Verbosity.all ~f:(fun v ->
           String.equal (Prigh_ui.Verbosity.name v) item.id)
       with
       | Some v -> set_verbosity m v
       | None -> m, [])
    | Confirm_tools -> set_confirm m (String.equal item.id "on")
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
    | Mcp l -> mcp_chosen m l item.id
  in
  m, (if Option.is_none m.dialog then [ focus_editor ] else []) @ cmds
;;

let with_picker (m : Model.t) ~f =
  match m.dialog with
  | Some (Picker { kind; picker }) ->
    { m with dialog = Some (Picker { kind; picker = f picker }) }, []
  | Some (Scoped_models { picker; checked }) ->
    { m with dialog = Some (Scoped_models { picker = f picker; checked }) }, []
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
  | Some (Picker { kind; picker }) ->
    (match Picker.selected_item picker with
     | Some item -> picker_accept m ~kind item
     | None -> m, [])
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
    let method_ : Request.Method.t =
      match target with
      | Sessions paths -> Delete_sessions paths
      | Archived -> Delete_archived
    in
    m, cmds @ [ call method_ ~tag:Sessions_deleted ]
  | Some (Login flow) ->
    (match flow.failed, flow.prompt with
     | None, Some _ -> respond_login m flow
     | Some _, _ -> close_dialog m
     | None, None -> m, [])
  | Some (Help | Hotkeys | Auth _ | Session _ | Text _ | Mcp_tools _ | Diff _) ->
    close_dialog m
  | Some (Loading _) -> m, []
  | Some (Scoped_models { checked; _ }) ->
    let m, cmds = save_scoped m checked in
    m, focus_editor :: cmds
  | Some (Prompt prompt) -> submit_prompt m prompt
  | Some (Rewind_confirm { id; _ }) ->
    let m, cmds = close_dialog m in
    let m, toast_cmds = toast m "Rewound: later messages stay in /tree" in
    ( m
    , cmds @ toast_cmds @ [ rpc "rewind" [ "to", str id ] ~tag:Reload_messages ]
    )
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

let update (m : Model.t) (action : Action.t) =
  match action with
  | Start -> m, start_commands m
  | Hello hello -> { m with hello = Some hello }, []
  | Saved_login -> { m with saved_login = true }, []
  (* What another session does is not this one's. *)
  | Event e
    when Option.is_some m.session_gone
         &&
         match e with
         | Sessions_changed -> false
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
  | Set_notifications { enabled; permission } ->
    ( { m with
        notifications =
          { m.notifications with
            enabled
          ; permission = Page_notify.Permission.of_string permission
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
       (* Everything on show stays, also what the agent waits for: once back,
          [get_pending] says what still does. *)
       ( { m with
           connection = Reconnecting { attempt = 0; generation }
         ; generation
         }
       , [ Command.Reconnect
             { generation
             ; delay_ms = 0
             ; session = Option.map m.state ~f:(fun s -> s.session_id)
             }
         ] ))
  | Reply (tag, result) -> reply m tag result
  (* The ages in the sidebar; the list itself follows [sessions_changed]. *)
  | Tick now -> { m with now = Some now }, []
  | Set_utc_offset utc_offset -> { m with utc_offset }, []
  | Set_narrow narrow ->
    if Bool.equal narrow m.narrow
    then m, []
    else { m with narrow; sidebar_open = not narrow }, []
  | Load_history entries -> { m with history = History.of_list entries }, []
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
    then ask_delete m (List.map (Session_marks.marked m.marks m.sessions) ~f:(fun s -> s.path))
    else ask_delete m [ path ]
  | Select_sessions on ->
    { m with marks = (if on then Session_marks.start else Session_marks.empty) }, []
  | Mark_session { path; range } ->
    ( { m with
        marks = Session_marks.click m.marks ~path ~range ~order:(session_order m)
      }
    , [] )
  | Delete_marked ->
    ask_delete m (List.map (Session_marks.marked m.marks m.sessions) ~f:(fun s -> s.path))
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
  | Open_model_picker -> open_picker m (model_picker m)
  | Open_thinking_picker -> open_thinking_picker m
  | Open_help -> open_dialog m Help
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
     | Some (Picker { kind; picker }) ->
       (match
          List.find (Picker.visible picker) ~f:(fun i -> String.equal i.id id)
        with
        | Some item -> picker_accept m ~kind item
        | None -> m, [])
     | _ -> m, [])
  | Dialog_input text ->
    (match m.dialog with
     | Some (Rename _) -> { m with dialog = Some (Rename text) }, []
     | Some (Login flow) ->
       { m with dialog = Some (Login { flow with input = text }) }, []
     | Some (Picker _ | Scoped_models _) ->
       with_picker m ~f:(fun p -> Picker.set_query p text)
     | Some (Prompt prompt) -> prompt_input m (Prompt.set_input prompt text)
     | _ -> m, [])
  | Dialog_move delta ->
    (match m.dialog with
     | Some (Login flow) ->
       { m with dialog = Some (Login (Login_flow.move flow delta)) }, []
     | Some (Picker _ | Scoped_models _) ->
       with_picker m ~f:(fun p -> Picker.move p delta)
     | Some (Prompt prompt) ->
       { m with dialog = Some (Prompt (Prompt.move prompt delta)) }, []
     | _ -> m, [])
  | Dialog_accept -> dialog_accept m
  | Close_dialog ->
    (match m.dialog with
     | Some (Login { failed = None; provider; _ }) ->
       let m, cmds = close_dialog { m with cancelled_login = Some provider } in
       m, cmds @ [ rpc "auth_cancel" [] ]
     | Some _ -> close_dialog m
     | None -> m, [])
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
  | Dequeue -> m, [ rpc "dequeue" [] ~tag:Dequeued ]
  | Edit_queued q ->
    ( { m with queue = Queued.remove m.queue q }
    , [ rpc "remove_queued" (Queued.Message.remove_params q) ~tag:Dequeued ] )
  | Remove_queued q ->
    ( { m with queue = Queued.remove m.queue q }
    , [ rpc
          "remove_queued"
          (Queued.Message.remove_params q)
          ~tag:(Unqueued q.text)
      ] )
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
  | Scroll_chat pages -> m, [ Command.Scroll_chat pages ]
  | Chat_scrolled { at_bottom } -> { m with scrolled_up = not at_bottom }, []
  | Jump_to_bottom -> to_bottom m
  | Jump_to_user_message dir -> m, [ Command.Jump_to_user_message dir ]
  | Run command ->
    (match Slash.parse command with
     | Some parsed -> run_command { m with dialog = None } parsed
     | None -> m, [])
  | Toggle_terminal ->
    if m.terminal.open_ then close_terminal m else open_terminal m
  | Reopen_terminal -> with_terminal m ~f:(fun t -> { t with open_ = true }), []
  | Close_terminal -> close_terminal m
  | New_shell -> new_shell m
  | Terminal_status { key; status } ->
    with_terminal m ~f:(fun t -> { t with status = Some (key, status) }), []
  | Files action -> with_files m (Files.update m.files action)
  | Resize action ->
    let layout, saves = Resize.update m.layout (Model.shown m) action in
    { m with layout }, List.map saves ~f:(fun s -> Command.Save_size s)
;;
