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
    | Messages of string (** the session's id *)
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
    | Deleted of string
    | Refresh_sessions
    | Login_started
    | Notice of string
    | Reconnect of int
    | Config
    | Config_saved of string
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

module Toast = struct
  type t =
    { id : int
    ; text : string
    ; error : bool
    }
  [@@deriving sexp_of, equal]

  let lifetime_ms = 4000
end

module Confirm = struct
  type t =
    { call_id : string
    ; name : string
    ; summary : string
    }
  [@@deriving sexp_of, equal]
end

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
    | Set_session_query of string
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
    | Toggle_sidebar
    | Respond_confirm of
        { call_id : string
        ; allow : bool
        }
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
    | Set_terminal_height of int
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
    ; queue : int * int
    ; confirms : Confirm.t list
    ; dialog : Dialog.t option
    ; cancelled_login : string option
    ; toasts : Toast.t list
    ; next_toast : int
    ; sidebar_open : bool
    ; scrolled_up : bool
    ; session_query : string
    ; agents : Agents.t
    ; verbosity : Prigh_ui.Verbosity.t
    ; config : Config.t option
    ; btw : Btw.t option
    ; btw_seq : int
    ; accounts : Accounts.Account.t list
    ; account : Accounts.Account.t option
    ; users : string list option
    ; terminal : Terminal.t
    ; skills : Skills.t
    }
  [@@deriving sexp_of]

  let running t =
    match t.state with
    | Some s -> s.running
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
  ; queue = 0, 0
  ; confirms = []
  ; dialog = None
  ; cancelled_login = None
  ; toasts = []
  ; next_toast = 0
  ; sidebar_open = true
  ; scrolled_up = false
  ; session_query = ""
  ; agents = Agents.empty
  ; verbosity = Normal
  ; config = None
  ; btw = None
  ; btw_seq = 0
  ; accounts = []
  ; account = None
  ; users = None
  ; terminal = Terminal.closed
  ; skills = Skills.empty
  }
;;

let rpc ?(tag = Reply_tag.Show_error) method_ params =
  Command.Rpc { method_; params; tag }
;;

let call ?tag (method_ : Request.Method.t) =
  rpc ?tag (Request.Method.name method_) (Request.Method.params method_)
;;

let str s = `String s
let max_toasts = 4

(* A toast replaces the ones saying the same, or, with [replaces], those
   starting with it: cycling a setting shows only where it ended up. *)
let toast (m : Model.t) ?(error = false) ?replaces text =
  let id = m.next_toast in
  let stale (t : Toast.t) =
    String.equal t.text text
    || Option.exists replaces ~f:(fun prefix -> String.is_prefix t.text ~prefix)
  in
  let toasts =
    List.filter m.toasts ~f:(Fn.non stale) @ [ { Toast.id; text; error } ]
  in
  ( { m with
      toasts = List.drop toasts (List.length toasts - max_toasts)
    ; next_toast = id + 1
    }
  , if error
    then []
    else [ Command.Expire_toast { id; after_ms = Toast.lifetime_ms } ] )
;;

let error m text = toast m ~error:true text
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

let session_id (m : Model.t) =
  Option.value_map m.state ~default:"" ~f:(fun s -> s.session_id)
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
let set_state (m : Model.t) (state : State.t) =
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
  let m = { m with state = Some state } in
  if changed
  then
    ( { m with
        chat = Chat.empty
      ; confirms = []
      ; queue = 0, 0
      ; dialog = Option.filter m.dialog ~f:(Fn.non Dialog.per_session)
      ; agents = { Agents.empty with open_ = m.agents.open_ && not m.narrow }
      ; btw = None
      ; completion = None
      ; scrolled_up = false
      ; skills = Skills.empty
      }
    , cancel_btw m
      @ [ Command.Set_url_session state.session_id
        ; rpc "get_messages" [] ~tag:(Messages state.session_id)
        ; rpc "get_pending" [] ~tag:Pending
        ; rpc "list_sessions" [] ~tag:Sessions
        ; list_subagents
        ; list_jobs
        ] )
  else m, background_changed
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
             "%s · %s ctx%s"
             model.provider
             (compact_tokens model.context_window)
             (if usable model then "" else " · not logged in"))
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
  sprintf "custom · %s · %s" c.base_url c.api_label
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
            ~detail:(custom_detail c ^ " · edit")
            ~search:(String.concat ~sep:" " [ s.name; "custom"; c.base_url ])
            s.name
        | None ->
          let configured =
            match s.configured with
            | Some c when String.equal c.method_ meth.method_ ->
              sprintf " · logged in via %s" c.source
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
           | Some custom -> custom_detail custom ^ " · " ^ key_source s
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

let set_thinking (m : Model.t) level =
  match m.state with
  | Some state when not state.model.supports_thinking ->
    error
      m
      (sprintf
         "%s has no thinking levels: switch to a model that thinks with /model"
         state.model.name)
  | _ -> m, [ rpc "set_thinking" [ "thinking", str level ] ]
;;

let set_model (m : Model.t) key = m, [ rpc "set_model" [ "model", str key ] ]

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
          | Some (method_, prefix) ->
            [ rpc method_ [ "prefix", str prefix ] ~tag:(Paths prefix) ]) ))
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
  else
    ( m
    , [ rpc "switch_session" [ "path", str path ] ~tag:Reload_state
      ; focus_editor
      ] )
;;

(* The keyboard through the sidebar's (filtered) sessions: down from the
   search to the first, up from the first back to the search. *)
let session_nav (m : Model.t) ~from ~by =
  let ids =
    List.map
      (Session_list.filter m.sessions ~query:m.session_query)
      ~f:(fun s -> s.id)
  in
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
  |> List.map ~f:(fun (a : Agents.Agent.t) ->
    rpc "get_subagent" [ "id", str a.id ] ~tag:(Subagent a.id))
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

let open_agents (m : Model.t) arg =
  match arg with
  | None ->
    ( with_agents m ~f:(fun a -> { a with open_ = true; selected = None })
    , [ list_subagents; list_jobs ] )
  | Some arg -> with_item m arg ~f:select
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

let save_config (m : Model.t) config ~notice =
  ( { m with config = Some config }
  , [ rpc
        "set_config"
        [ "config", Config.to_json config ]
        ~tag:(Config_saved notice)
    ] )
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
      m, cmds @ [ rpc "set_model" [ "model", str next.key ] ])
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
      m, cmds @ [ rpc "set_thinking" [ "thinking", str next ] ]))
;;

let verbosity_detail : Prigh_ui.Verbosity.t -> string = function
  | Quiet -> "tool calls without their output; no thinking"
  | Normal -> "tool output and thinking folded"
  | Verbose -> "everything unfolded"
;;

let set_verbosity (m : Model.t) verbosity =
  toast
    { m with verbosity }
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

let set_confirm (m : Model.t) enabled =
  save_config
    m
    { (config m) with confirm_tools = enabled }
    ~notice:
      (if enabled
       then "Tool confirmation on: bash, write and edit ask first"
       else "Tool confirmation off: tools run without asking")
;;

let confirm_picker (m : Model.t) =
  let current = Option.map m.config ~f:(fun c -> c.confirm_tools) in
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
    ~notice:
      (match scoped_models with
       | [] -> "Scope cleared: Ctrl+P cycles through the logged-in models"
       | l ->
         sprintf
           "%d scoped model%s: Ctrl+P and Alt+P cycle through them"
           (List.length l)
           (if List.length l = 1 then "" else "s"))
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
  | _ -> error m e
;;

let prompt_succeeded (m : Model.t) =
  match m.dialog with
  | Some (Prompt { busy = true; _ }) -> close_dialog m
  | _ -> m, []
;;

let help_command (m : Model.t) name =
  let name = String.chop_prefix_if_exists name ~prefix:"/" in
  match Slash.find name with
  | Some spec -> toast m (sprintf "%s — %s" (Slash.Spec.usage spec) spec.help)
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
    ; sessions = []
    ; session_query = ""
    ; confirms = []
    ; queue = 0, 0
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
  | _, "act" -> m, [ rpc "list_users" [] ~tag:(Users Picker) ]
  | _, "back" ->
    (match own_user m with
     | Some own -> act_as m own
     | None -> m, [])
  | _, "add" -> m, [ Command.Add_account ]
  | _, "signout" -> m, [ Command.Sign_out ]
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
      m, [ call List_skills ~tag:(Skills { place; purpose = Picker query }) ])
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
  | "new", _ -> m, [ rpc "new_session" [] ~tag:Reload_state ]
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
  | "thinking", "" -> open_picker m (thinking_picker m)
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
  | "confirm", ("on" | "true") -> set_confirm m true
  | "confirm", ("off" | "false") -> set_confirm m false
  | "confirm", other ->
    error m (sprintf "Unknown setting %S: use on or off." other)
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
  | "name", "" -> open_rename m
  | "name", name ->
    m, [ rpc "set_session_name" [ "name", str name ] ~tag:Refresh_sessions ]
  | "session", _ -> m, [ rpc "session_stats" [] ~tag:Session_stats ]
  | "sessions", _ | "switch", "" -> open_sessions m
  | "switch", path ->
    m, [ rpc "switch_session" [ "path", str path ] ~tag:Reload_state ]
  | "clone", _ -> m, [ rpc "clone" [] ~tag:Reload_state ]
  | "fork", _ -> m, [ rpc "get_entries" [] ~tag:(Entries Fork) ]
  | "rewind", _ -> m, [ rpc "get_entries" [] ~tag:(Entries Rewind) ]
  | "tree", _ ->
    m, [ rpc "get_entries" [ "all", Json.bool true ] ~tag:(Entries Tree) ]
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
  | "host", "" -> hosts_picker m
  | "host", arg -> switch_host m arg
  | "export", "" -> open_prompt m (Prompt.create Export)
  | "export", path -> export m path
  | "import", "" -> open_prompt m (Prompt.create Import)
  | "import", path -> m, [ rpc "import" [ "path", str path ] ~tag:Imported ]
  | ("skills" | "skill:"), query -> open_skills m ~query
  | "mcp", "" -> m, [ call (List_mcp { reconnect = false }) ~tag:(Mcp Picker) ]
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
  | "login", "" -> m, [ rpc "auth_status" [] ~tag:(Auth_status Login_picker) ]
  | "login", args ->
    (match
       String.split args ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
     with
     | provider :: method_ :: _ ->
       start_login m ~provider ~method_:(Some method_)
     | _ -> start_login m ~provider:args ~method_:None)
  | "logout", "" -> m, [ rpc "auth_status" [] ~tag:(Auth_status Logout_picker) ]
  | "logout", provider -> logout m provider
  | "auth", _ -> m, [ rpc "auth_status" [] ~tag:(Auth_status Show) ]
  | "setusr", "" -> m, [ rpc "list_users" [] ~tag:(Users Picker) ]
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
      { m with chat = Chat.empty }
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

(* A [get_pending] reply: the queue's sizes and the tool calls waiting for
   the user (asked before a reload, or in another tab). *)
let pending_of_json json =
  let open Or_error.Let_syntax in
  let%bind steer =
    Json.list_field json "steer_texts" ~f:Json.to_string_or_error
  in
  let%bind follow_up =
    Json.list_field json "follow_up_texts" ~f:Json.to_string_or_error
  in
  let%map confirms =
    Json.list_field json "confirms" ~f:(fun c ->
      let%bind call_id = Json.string_field c "call_id" in
      let%bind name = Json.string_field c "name" in
      let%map summary = Json.string_field c "summary" in
      { Confirm.call_id; name; summary })
  in
  (List.length steer, List.length follow_up), confirms
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

let reply (m : Model.t) (tag : Reply_tag.t) result =
  match tag, result with
  | Reconnect generation, _ when generation <> m.generation -> m, []
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
  | Reconnect _, Ok json ->
    let m = { m with connection = Connected } in
    let m =
      match Hello_reply.of_json json with
      | Ok hello -> { m with hello = Some hello }
      | Error _ -> m
    in
    (* The session may have moved on while we were away: start over. *)
    let m, cmds =
      toast { m with state = None; chat = Chat.empty } "Reconnected"
    in
    m, cmds @ startup
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
  | Deleted title, Error e ->
    error
      m
      (sprintf
         "Couldn't delete %S: %s%s"
         title
         e
         (if String.is_substring e ~substring:"live"
          then
            ". Switch to another session and close other tabs using it first."
          else ""))
  | Login_started, Error e ->
    (match m.dialog with
     | Some (Login flow) ->
       { m with dialog = Some (Login { flow with failed = Some e }) }, []
     | _ -> error m e)
  | Sent { text; images }, Error e ->
    (* Back in the editor, unless something else is being written there. *)
    if String.is_empty m.draft && List.is_empty m.images
    then
      error
        (set_draft { m with images } text)
        (sprintf "Couldn't send: %s. Your message is back in the editor." e)
    else error m (sprintf "Couldn't send: %s. ↑ brings your message back." e)
  | _, Error e -> error m e
  | (Ignore | Show_error | Login_started | Sent _), Ok _ -> m, []
  | Notice text, Ok _ -> toast m text
  | State, Ok json -> decode m json State.of_json ~f:(set_state m)
  | Reload_state, Ok _ -> m, [ rpc "get_state" [] ~tag:State ]
  (* Another session's, asked for before switching again. *)
  | Messages session, Ok _ when not (String.equal session (session_id m)) ->
    m, []
  | Messages _, Ok json ->
    decode m json (decode_list Message.of_json) ~f:(fun messages ->
      let chat = Chat.of_messages ~running:(Model.running m) messages in
      ( { m with chat; scrolled_up = false }
      , Command.Scroll_to_bottom
        :: List.map (Chat.subagents_to_load chat) ~f:(fun call_id ->
          rpc "get_subagent" [ "id", str call_id ] ~tag:(Subagent call_id)) ))
  | Pending, Ok json ->
    (match pending_of_json json with
     | Ok (queue, confirms) ->
       let confirms =
         m.confirms
         @ List.filter confirms ~f:(fun (c : Confirm.t) ->
           not
             (List.exists m.confirms ~f:(fun (c' : Confirm.t) ->
                String.equal c.call_id c'.call_id)))
       in
       ( { m with queue; confirms }
       , if List.is_empty confirms then [] else [ Command.Focus "confirm" ] )
     | Error e -> error m (Error.to_string_hum e))
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
      { m with sessions }, [])
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
  | Dequeued, Ok json ->
    (match field json "text" with
     | Some (`String text) ->
       let draft =
         if String.is_empty m.draft then text else text ^ "\n\n" ^ m.draft
       in
       set_draft m draft, [ focus_editor ]
     | _ -> toast m "Nothing is queued")
  | Deleted title, Ok _ ->
    let m, cmds = toast m (sprintf "Deleted %S" title) in
    m, cmds @ [ rpc "list_sessions" [] ~tag:Sessions ]
  | Config, Ok json ->
    decode m json Config.of_json ~f:(fun config ->
      { m with config = Some config }, [])
  | Config_saved notice, Ok json ->
    decode m json Config.of_json ~f:(fun config ->
      toast { m with config = Some config } notice)
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
    ( m
    , [ rpc "get_state" [] ~tag:State
      ; rpc "get_messages" [] ~tag:(Messages (session_id m))
      ] )
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
  | Queue_update { steer; follow_up } -> { m with queue = steer, follow_up }, []
  | Tool_confirm { call_id; name; summary } ->
    ( { m with confirms = m.confirms @ [ { call_id; name; summary } ] }
    , [ Command.Focus "confirm" ] )
  | Tool_end { call; _ } ->
    ( { m with
        confirms =
          List.filter m.confirms ~f:(fun c ->
            not (String.equal c.call_id call.id))
      }
    , (* A [!command] may have just saved the session. *)
      if String.equal call.name "shell"
      then [ rpc "list_sessions" [] ~tag:Sessions ]
      else [] )
  | Agent_end _ -> m, [ rpc "list_sessions" [] ~tag:Sessions ]
  (* Another client's /confirm, /scoped-models or defaults. *)
  | Config_changed config -> { m with config = Some config }, []
  | Auth e -> auth_event m e
  | Btw_delta { btw_id; delta } ->
    update_btw m btw_id ~f:(fun b -> Btw.append b delta), []
  | _ -> m, []
;;

let event (m : Model.t) (e : Event.t) =
  let m =
    { m with
      chat = Chat.apply m.chat e
    ; agents = Agents.record m.agents ~now:(now_of m) e
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
  let sent (m : Model.t) =
    let history = History.add m.history text in
    ( { (set_draft m "") with history; completion = None }
    , [ Command.Save_history (History.to_list history) ] )
  in
  match Slash.parse text with
  | _ when String.is_prefix text ~prefix:"!" && List.is_empty m.images ->
    let m, save = sent m in
    let m, cmds = shell m text in
    m, save @ (Command.Scroll_to_bottom :: cmds)
  | Some parsed when List.is_empty m.images && not (invokes_skill parsed) ->
    let m, save = sent m in
    let m, cmds = run_command m parsed in
    m, save @ cmds
  | _ when not (Connection.equal m.connection Connected) ->
    error
      m
      "Not connected to the backend: your message stays here until it is back \
       (/retry-backend-connection tries now)."
  | _ ->
    if String.is_empty text && List.is_empty m.images
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
      let agents =
        if String.equal method_ "prompt"
        then Agents.prompt_sent m.agents
        else m.agents
      in
      ( { m with images = []; agents }
      , save
        @ [ Command.Scroll_to_bottom
          ; rpc
              method_
              (("text", str text) :: images)
              ~tag:(Sent { text; images = m.images })
          ] ))
;;

let accept_completion (m : Model.t) ~run =
  match Model.popup m with
  | None -> m, []
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
      | Argument (Directory | Path | Skill), _ -> false
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
    | Fork entries ->
      let m =
        match Session_tree.user_text entries item.id with
        | Some text -> set_draft m text
        | None -> m
      in
      let m, cmds =
        toast m "Forked into a new session: edit the message and send it"
      in
      m, cmds @ [ rpc "fork" [ "at", str item.id ] ~tag:Reload_state ]
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
  | Some (Delete { path; title }) ->
    let m, cmds = close_dialog m in
    m, cmds @ [ rpc "delete_session" [ "path", str path ] ~tag:(Deleted title) ]
  | Some (Login flow) ->
    (match flow.failed, flow.prompt with
     | None, Some _ -> respond_login m flow
     | Some _, _ -> close_dialog m
     | None, None -> m, [])
  | Some (Help | Hotkeys | Auth _ | Session _ | Text _ | Mcp_tools _) ->
    close_dialog m
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
  | Start ->
    let probe =
      (* Only superusers may list the users: the account menu offers them. *)
      match own_user m with
      | Some _ -> [ rpc "list_users" [] ~tag:(Users Probe) ]
      | None -> []
    in
    m, (if m.narrow then [] else [ focus_editor ]) @ startup @ probe
  | Hello hello -> { m with hello = Some hello }, []
  | Saved_login -> { m with saved_login = true }, []
  | Event e -> event m e
  | Protocol_error e -> error m ("Protocol error: " ^ e)
  | Backend_closed ->
    (match m.connection with
     | Reconnecting _ -> m, []
     | Connected ->
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
         ] ))
  | Reply (tag, result) -> reply m tag result
  | Tick now ->
    let refresh =
      match m.connection, m.now with
      | Connected, Some _ -> [ rpc "list_sessions" [] ~tag:Sessions ]
      | _ -> []
    in
    { m with now = Some now }, refresh
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
    ( { m with sidebar_open = m.sidebar_open && not m.narrow }
    , [ rpc "new_session" [] ~tag:Reload_state ] )
  | Switch_session path -> switch_session m path
  | Ask_delete path ->
    (match List.find m.sessions ~f:(fun s -> String.equal s.path path) with
     | None -> m, []
     | Some s -> open_dialog m (Delete { path; title = Session_list.title s }))
  | Set_session_query session_query -> { m with session_query }, []
  | Open_sessions -> open_sessions m
  | Session_nav { from; by } -> session_nav m ~from ~by
  | Open_first_session ->
    (match Session_list.filter m.sessions ~query:m.session_query with
     | [] -> m, []
     | first :: _ -> switch_session m first.path)
  | Leave_sidebar ->
    { m with sidebar_open = m.sidebar_open && not m.narrow }, [ focus_editor ]
  | Set_model key -> set_model m key
  | Set_thinking level -> set_thinking m level
  | Open_model_picker -> open_picker m (model_picker m)
  | Open_thinking_picker -> open_picker m (thinking_picker m)
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
  | Toggle_sidebar -> { m with sidebar_open = not m.sidebar_open }, []
  | Respond_confirm { call_id; allow } ->
    ( { m with
        confirms =
          List.filter m.confirms ~f:(fun c ->
            not (String.equal c.call_id call_id))
      }
    , [ rpc
          "tool_confirm_respond"
          [ "call_id", str call_id; "allow", Json.bool allow ]
      ; focus_editor
      ] )
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
  | Set_terminal_height px ->
    ( with_terminal m ~f:(fun t ->
        { t with height = Some (Int.max Terminal.min_height px) })
    , [] )
;;
