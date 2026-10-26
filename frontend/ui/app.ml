open! Core
open! Import

module Reply_tag = struct
  type t =
    | Ignore
    | Show_error
    | Initial_state
    | Initial_messages
    | Reload_messages
    | Auth_refresh
    | Auth_show
    | Auth_login_picker
    | Auth_logout_picker
    | Forge_refresh
    | Prs_picker of string option (** the state asked for *)
    | Pr_shown
    | Pr_form_defaults of string option (** the title typed *)
    | Pr_created
    | Pr_checked_out of int
    | Models_for_picker of string
    | Models_for_switch of string
    | Models_after_login of string
    | Sessions_picker
    | Sessions_cache
    | Session_stats
    | Entries_for_fork
    | Entries_for_rewind
    | Entries_for_tree
    | Export_done
    | Sessions_refresh (** the open sessions picker and the cache *)
    | Sessions_deleted
    | Sessions_to_delete of string list (** [/delete]'s arguments *)
    | Archived_to_delete
    | Archive_done of
        { name : string
        ; archived : bool
        }
    | Session_to_archive of
        { arg : string
        ; archived : bool
        }
    | Paths_for_autocomplete of string
    | Dirs_for_autocomplete of string
    | Set_model_done of string (** the key requested *)
    | Config
    | Config_saved
    | Default_saved
    | Fallback_shown
    | Fallback_saved of string
    | Default_dir_shown
    | Default_dir_saved of string
    | Models_catalog
    | Models_for_scoped
    | Compact_done
    | Git_diff
    | Review_comments of
        { dir : string option
        ; base : string option
        } (** [/review [WORKTREE] [--base REV]]: then the diff *)
    | Review_status of
        { review : P.Review.t
        ; dir : string option (** [None]: the session's own worktree *)
        }
    | Review_named of
        { command : string
        ; name : string
        ; base : string option
        ; model : string option
        } (** [/review NAME]: [list_worktrees], to find it *)
    | Review_started
    | Abort_done
    | Notice_on_success of string
    | History
    | Prefs_history
    | Dequeued
    | Agent_message of
        { agent_id : string
        ; kind : Queued_message.Kind.t
        ; text : string
        ; retry : string
        }
    | Editor_text
    | Reload_messages_notice of string
    | Reconnect of int (** generation; stale replies are ignored *)
    | Btw of string (** btw id *)
    | Users_list
    | Nix_store_shown (** [/nix-gc] *)
    | Nix_gc_done (** [/nix-gc run] *)
    | User_switched
    | Jobs_picker
    | Job_output
    | Job_started
    | Skills_picker
    | Snippets_saved of string (** the notice *)
    | Reminder_added
    | Skills_for_autocomplete of string (** the [Location_cache] key *)
    | Skill_prompt of string (** the [/skill:] text sent *)
    | Mcp_picker
    | Mcp_reconnected
    | Mcp_approved of string (** server *)
    | Pending
    | Mode_set of string (** the notice *)
    | Refresh_state
    | Subagent_loaded
    (** [get_subagent], for a subagent whose events we missed *)
    | Dir_changed of P.Dir_change.Target.t
    (** [Missing]: confirm creating it; otherwise acknowledge *)
    | Worktrees_picker
    | Worktrees_for_autocomplete of string
    | Worktree_target of
        { command : string
        ; action : Worktree_command.Action.t
        }
    | Worktree_created
    | Worktree_entered of string
    | Worktree_session_started of
        { path : string
        ; notice : string
        }
    | Worktree_removed of
        { target : string
        ; force : bool
        }
    | Retry_in_editor of
        { command : string
        ; hint : string
        ; then_ : t
        }
  [@@deriving sexp_of, equal]
end

module Command = struct
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * P.Json.t) list
        ; tag : Reply_tag.t
        }
    | Open_browser of string
    | Load_history
    | Append_history of string
    | Copy_to_clipboard of string
    | Suspend
    | Edit_externally of string
    | Reconnect of
        { generation : int
        ; delay_ms : int
        ; session : string option
        ; as_user : string option
        }
    | Relink of
        { generation : int
        ; change : Server_link.Change.t
        }
    | Sign_out
    | Notify of
        { mode : Notify.Mode.t
        ; notification : Notify.t
        }
    | Quit
  [@@deriving sexp_of, equal]
end

module Connection = struct
  type t =
    | Connected
    | Reconnecting of
        { attempt : int
        ; generation : int
        ; delay_ms : int
        ; session : string option
        (** the session to resume; dropped once the backend says it no longer
            exists (a never-persisted session has no file) *)
        }
    | Switching of
        { generation : int
        ; previous : t (** back to it when the new server cannot be reached *)
        ; notices : string list
        }
    | Disconnected of { reason : string }
  [@@deriving sexp_of, equal]

  let session_missing error = String.is_prefix error ~prefix:"no session "
  let max_delay_ms = 60_000
  let max_attempt = 9

  (* 250ms, 500ms, 1s, ... capped at 60s. Check the cap before calculating the
     exponential backoff so that a large attempt cannot overflow the integer
     arithmetic. *)
  let delay_ms ~attempt =
    if attempt >= max_attempt
    then max_delay_ms
    else (
      let exponent = if attempt <= 1 then 0 else attempt - 1 in
      250 * Int.pow 2 exponent)
  ;;

  let next_attempt attempt =
    if attempt >= max_attempt then max_attempt else attempt + 1
  ;;
end

module Action = struct
  type t =
    | Start
    | Key of Key.t
    | Intent of Intent.t
    | Event of P.Event.t
    | Protocol_error of string
    | Stderr of string
    | Backend_closed
    | Reply of Reply_tag.t * (P.Json.t, string) Result.t
    | Tick
    | Set_home of string
    | Set_utc_offset of Time_ns.Span.t
    | Focus of bool
    | Hello of P.Hello_reply.t
    | Server_link of Server_link.t
    | Relinked of
        { generation : int
        ; outcome : Server_link.Outcome.t
        }
    | Rejected of string
    | Resize of
        { width : int
        ; height : int
        }
  [@@deriving sexp_of]
end

module Model = struct
  type t =
    { state : P.State.t option
    ; models : P.Model.t list
    ; auth : P.Auth_status.t list
    ; forge : P.Forge_account.t list
    ; transcript : Transcript.t
    ; agents : Agent_view.t list
    ; focus : [ `Main | `Agent of string ]
    ; editor : Editor.t
    ; mode : Mode.t
    ; autocomplete : Autocomplete.t option
    ; sessions : P.Session_summary.t list option
    ; skills : P.Skill.t list Location_cache.t
    ; worktrees : P.Worktree.Listing.t Location_cache.t
    ; known_paths : String.Set.t
    ; queued_messages : Queued_message.t list
    ; agent_queues : Agent_queues.t
    ; login_lines : string list
    ; viewport : Viewport.t
    ; pending_quit : bool
    ; spinner : int
    ; activity : Activity.t
    ; verbosity : Verbosity.t
    ; config : P.Config.t option
    ; home : string option
    ; client_id : string option (** our host id, from [hello] *)
    ; namespace : string option
    ; user : string option
    ; superuser : bool
    ; stderr_tail : string list
    ; pending_confirms : P.Pending.Confirm.t list
    ; pending_questions : P.Pending.Ask.t list
    ; connection : Connection.t
    ; reconnect_generation : int
    ; btw : Btw_box.t option
    ; btw_seq : int
    ; width : int
    ; height : int
    ; quitting : bool
    ; utc_offset : Time_ns.Span.t
    ; terminal_focus : Focus.t
    ; server : Server_link.t option
    ; notice : Notice.t option
    ; nix_gc : P.Nix_gc_progress.t option
    ; sidebar : bool
    }
  [@@deriving sexp_of]

  let running t =
    Option.value_map t.state ~default:false ~f:(fun s -> s.running)
  ;;

  let activity t = Activity.set_running t.activity (running t)

  let agents_running t =
    List.exists t.agents ~f:(fun (a : Agent_view.t) ->
      Agent_view.Status.equal a.status Running)
    || Option.exists t.state ~f:(fun s ->
      List.exists s.subagents ~f:(fun a -> a.running))
  ;;

  let jobs_running t =
    Option.exists t.state ~f:(fun s ->
      List.exists s.jobs ~f:(fun (j : P.State.Job.t) -> j.running))
  ;;

  let backend_gone t =
    match t.connection with
    | Connected -> false
    | Reconnecting _ | Switching _ | Disconnected _ -> true
  ;;

  let acting_as t =
    P.Hello_reply.acting_as
      { client_id = ""
      ; namespace = t.namespace
      ; user = t.user
      ; superuser = t.superuser
      }
  ;;
end

open Model

let rpc ?(params = []) ?(tag = Reply_tag.Show_error) method_ =
  Command.Rpc { method_; params; tag }
;;

let call ?(tag = Reply_tag.Show_error) (method_ : P.Request.Method.t) =
  Command.Rpc
    { method_ = P.Request.Method.name method_
    ; params = P.Request.Method.params method_
    ; tag
    }
;;

let str = P.Json.str

let focused_agent m =
  match m.focus with
  | `Main -> None
  | `Agent id -> Agent_view.find m.agents id
;;

let shown_queue m =
  match m.focus with
  | `Main -> m.queued_messages
  | `Agent agent_id -> Agent_queues.messages m.agent_queues ~agent_id
;;

let editor_row_count m =
  let inner = Int.max 1 (m.width - 2) in
  List.sum (module Int) (Editor.lines m.editor) ~f:(fun line ->
    let rec count text acc =
      let _, rest = Text_width.take text ~width:inner in
      if String.is_empty rest then acc + 1 else count rest (acc + 1)
    in
    count line 0)
;;

let layout m =
  Layout.compute
    ~width:m.width
    ~height:m.height
    ~sidebar:m.sidebar
    ~editor_rows:(editor_row_count m)
;;

let transcript_width m = (layout m).main_width

let transcript_line_count m =
  let transcript =
    match m.focus with
    | `Main -> m.transcript
    | `Agent id ->
      (match Agent_view.find m.agents id with
       | Some agent -> agent.transcript
       | None -> m.transcript)
  in
  Transcript.line_count
    transcript
    ~width:(transcript_width m)
    ~verbosity:m.verbosity
;;

(* A focused subagent's header takes the transcript's first row. *)
let transcript_rows m =
  let rows = (layout m).main_rows in
  match m.focus with
  | `Agent _ -> Int.max 0 (rows - 1)
  | `Main -> rows
;;

(* The single place transcript edits go through so the anchored viewport can
   count lines appended beneath it. *)
let with_transcript m ~f =
  let before = transcript_line_count m in
  let m = { m with transcript = f m.transcript } in
  match m.viewport with
  | Viewport.Follow -> m
  | Viewport.Anchored { top; new_lines } ->
    let added = Int.max 0 (transcript_line_count m - before) in
    { m with
      viewport = Viewport.Anchored { top; new_lines = new_lines + added }
    }
;;

(* Same as [with_transcript] but for the agent list, so streaming inside a
   focused subagent still bumps the anchored viewport. *)
let update_agents m ~f =
  let before = transcript_line_count m in
  let m = { m with agents = f m.agents } in
  match m.viewport with
  | Viewport.Follow -> m
  | Viewport.Anchored { top; new_lines } ->
    let added = Int.max 0 (transcript_line_count m - before) in
    { m with
      viewport = Viewport.Anchored { top; new_lines = new_lines + added }
    }
;;

(* Acknowledgements go to the notice slot and expire; [log] is for what
   belongs to the session's history. *)
let notice ?severity m text =
  { m with notice = Some (Notice.create ?severity text) }
;;

let log ?severity m text =
  with_transcript m ~f:(fun t -> Transcript.notice ?severity t text)
;;

let error m text = notice ~severity:Error m text
let warn m text = notice ~severity:Warn m text

let with_state m ~f =
  match m.state with
  | Some state -> notice m (f state), []
  | None -> warn m "not connected to the backend yet", []
;;

let confirm_tool m (c : P.Pending.Confirm.t) =
  { m with
    mode =
      Confirm { question = Confirm_prompt.title c; action = Tool_confirm c }
  ; autocomplete = None
  }
;;

let same_confirm (a : P.Pending.Confirm.t) (b : P.Pending.Confirm.t) =
  String.equal a.call_id b.call_id
  && Option.equal String.equal a.agent_id b.agent_id
;;

(* What is shown or queued already is not added again ([get_pending] after
   the events). *)
let add_pending_confirms m confirms =
  let known (c : P.Pending.Confirm.t) =
    List.exists m.pending_confirms ~f:(same_confirm c)
    ||
    match m.mode with
    | Confirm { action = Tool_confirm shown; _ } -> same_confirm shown c
    | Text_prompt { action = Deny_reason { confirm; _ }; _ } ->
      same_confirm confirm c
    | _ -> false
  in
  { m with
    pending_confirms =
      m.pending_confirms @ List.filter confirms ~f:(Fn.non known)
  }
;;

let add_pending_questions m questions =
  let known (q : P.Pending.Ask.t) =
    List.exists m.pending_questions ~f:(fun p ->
      String.equal p.call_id q.call_id)
    ||
    match m.mode with
    | Question { call_id; _ } -> String.equal call_id q.call_id
    | _ -> false
  in
  { m with
    pending_questions =
      m.pending_questions @ List.filter questions ~f:(Fn.non known)
  }
;;

(* Confirmations first, then the model's questions, one dialog at a time. *)
let maybe_open_pending m =
  if Model.backend_gone m
  then m
  else (
    match m.mode, m.pending_confirms, m.pending_questions with
    | (Editing | Output _), c :: rest, _ ->
      { (confirm_tool m c) with pending_confirms = rest }
    | (Editing | Output _), [], (q : P.Pending.Ask.t) :: rest ->
      { m with
        mode =
          Question
            { call_id = q.call_id
            ; form = Question_form.create ?deadline:q.deadline q.questions
            }
      ; autocomplete = None
      ; pending_questions = rest
      }
    | _ -> m)
;;

let answer_confirm
      ?reason
      m
      (c : P.Pending.Confirm.t)
      (answer : Confirm_prompt.Answer.t)
  =
  ( log m (Confirm_prompt.acknowledgement c answer)
  , [ rpc
        "tool_confirm_respond"
        ~params:(Confirm_prompt.params c answer ~reason)
        ~tag:Show_error
    ] )
;;

let verbosity_notice = function
  | Verbosity.Quiet ->
    "view: quiet — intermediate output and thinking are hidden"
  | Normal -> "view: normal — tool output is summarised"
  | Verbose -> "view: verbose — everything is shown"
;;

let log_block m content =
  with_transcript m ~f:(fun t -> Transcript.add t (Block content))
;;

(* Over the transcript, unless a dialog is open: then into it, so that a
   late reply is not lost. *)
let output m ~title content =
  if Mode.is_dialog m.mode
  then log_block m content
  else
    { m with
      mode = Output (Output_box.create ~title content)
    ; autocomplete = None
    }
;;

let follow m = { m with viewport = Viewport.Follow }

let tilde m path =
  match m.home with
  | Some home when String.equal path home -> "~"
  | Some home when String.is_prefix path ~prefix:(home ^ "/") ->
    "~" ^ String.drop_prefix path (String.length home)
  | _ -> path
;;

let banner m (state : P.State.t) : Content.Line.t =
  let where =
    match state.session_name with
    | Some name -> sprintf "%s %S" (tilde m state.cwd) name
    | None -> tilde m state.cwd
  in
  [ { text =
        sprintf "prigh in %s · /help · Esc aborts · Ctrl+C twice quits" where
    ; style = Style.fg Gray
    }
  ]
;;

let set_verbosity m verbosity =
  follow (notice { m with verbosity } (verbosity_notice verbosity))
;;

let init =
  { state = None
  ; models = []
  ; auth = []
  ; forge = []
  ; transcript = Transcript.empty
  ; agents = []
  ; focus = `Main
  ; editor = Editor.empty
  ; mode = Editing
  ; autocomplete = None
  ; sessions = None
  ; skills = Location_cache.empty
  ; worktrees = Location_cache.empty
  ; known_paths = String.Set.empty
  ; queued_messages = []
  ; agent_queues = []
  ; login_lines = []
  ; viewport = Viewport.Follow
  ; pending_quit = false
  ; spinner = 0
  ; activity = Idle
  ; verbosity = Verbosity.Normal
  ; config = None
  ; home = None
  ; client_id = None
  ; namespace = None
  ; user = None
  ; superuser = false
  ; stderr_tail = []
  ; pending_confirms = []
  ; pending_questions = []
  ; connection = Connected
  ; reconnect_generation = 0
  ; btw = None
  ; btw_seq = 0
  ; width = 80
  ; height = 24
  ; quitting = false
  ; utc_offset = Time_ns.Span.zero
  ; terminal_focus = Unknown
  ; server = None
  ; notice = None
  ; nix_gc = None
  ; sidebar = false
  }
;;

(* What a new connection shows, also after [/connect] and [/signin]. *)
let refill_commands =
  [ rpc "get_state" ~tag:Initial_state
  ; rpc "get_messages" ~tag:Initial_messages
  ; rpc "auth_status" ~tag:Auth_refresh
  ; call Forge_status ~tag:Forge_refresh
  ; rpc "get_config" ~tag:Config
  ; rpc "list_models" ~tag:Models_catalog
  ; rpc "get_pending" ~tag:Pending
  ]
;;

(* The user's prompt history is the backend's ([get_prefs], [push_pref]),
   shared with prigh-web; [~/.prigh/history] is this machine's, read first
   and for when the backend keeps none or is out of reach. *)
let history_key = "history"
let history_max = 500

let start_commands =
  refill_commands
  @ [ Command.Load_history
    ; rpc
        "get_prefs"
        ~params:(P.Prefs.get_params ~keys:[ history_key ])
        ~tag:Prefs_history
    ]
;;

(* Newest first, as the backend keeps it. *)
let history_of_json = function
  | Some (`Array items) ->
    Some
      (List.filter_map items ~f:(function
         | `String s -> Some s
         | _ -> None))
  | _ -> None
;;

let set_backend_history m entries =
  if Editor.browsing_history m.editor
  then m
  else { m with editor = Editor.set_history m.editor (List.rev entries) }
;;

let push_history text =
  rpc
    "push_pref"
    ~params:
      (P.Prefs.push_params ~key:history_key (`String text) ~max:history_max)
    ~tag:Ignore
;;

let decode_list json ~f =
  match json with
  | `Array items -> Or_error.all (List.map items ~f)
  | other -> Or_error.errorf "expected array, got %s" (P.Json.to_string other)
;;

let decode_restored json =
  match P.Json.field json "restored" with
  | None -> Ok []
  | Some (`Array items) ->
    Or_error.all (List.map items ~f:P.Json.to_string_or_error)
  | Some other ->
    Or_error.errorf "restored: expected array, got %s" (P.Json.to_string other)
;;

let logged_in m provider =
  List.exists m.auth ~f:(fun a ->
    String.equal a.provider provider && Option.is_some a.configured)
;;

(** Models Ctrl+P / Ctrl+T / [/scoped-models] operate on, in catalog order. *)
let effective_scope m =
  match m.config with
  | Some { P.Config.scoped_models = _ :: _ as keys; _ } ->
    List.filter m.models ~f:(fun (model : P.Model.t) ->
      List.mem keys model.key ~equal:String.equal)
  | _ ->
    let logged =
      List.filter m.models ~f:(fun model -> logged_in m model.provider)
    in
    if List.is_empty logged then m.models else logged
;;

(* Empty when every model is in scope: a mark on every row says nothing. *)
let scoped_keys m =
  let scope = effective_scope m in
  if List.length scope >= List.length m.models
  then String.Set.empty
  else
    String.Set.of_list
      (List.map scope ~f:(fun (model : P.Model.t) -> model.key))
;;

(* ---- pickers ---------------------------------------------------------- *)

let open_picker m kind picker =
  if Mode.is_dialog m.mode
  then warn m "close the current dialog first (Esc)"
  else { m with mode = Picker { kind; picker }; pending_quit = false }
;;

let format_price (c : P.Model.Cost.t) =
  if Float.(c.input = 0. && c.output = 0.)
  then "price unknown"
  else sprintf "$%g/$%g per M" c.input c.output
;;

let format_tokens n =
  if n < 1000
  then Int.to_string n
  else if n < 10000
  then sprintf "%.1fk" (Float.of_int n /. 1e3)
  else if n < 1_000_000
  then sprintf "%dk" (Int.of_float (Float.round (Float.of_int n /. 1e3)))
  else if n < 10_000_000
  then sprintf "%.1fM" (Float.of_int n /. 1e6)
  else sprintf "%dM" (Int.of_float (Float.round (Float.of_int n /. 1e6)))
;;

let model_picker_value m ~logged_in_only ~query =
  let current = Option.map m.state ~f:(fun s -> s.model.key) in
  let scoped = scoped_keys m in
  let models =
    if logged_in_only
    then List.filter m.models ~f:(fun model -> logged_in m model.provider)
    else m.models
  in
  let items =
    List.map models ~f:(fun (model : P.Model.t) ->
      let logged = logged_in m model.provider in
      let scoped_mark = if Set.mem scoped model.key then " ◆" else "" in
      Picker.Item.create
        ~id:model.key
        ~detail:
          (String.concat
             ~sep:"  "
             ([ model.key ^ scoped_mark
              ; "ctx " ^ format_tokens model.context_window
              ; format_price model.cost
              ]
              @ if logged then [] else [ "not logged in" ]))
        ~search:(model.name ^ " " ^ model.key)
        ~marked:
          (Option.value_map current ~default:false ~f:(String.equal model.key))
        ~dimmed:(not logged)
        model.name)
  in
  let title = if logged_in_only then "Model (logged in)" else "Model" in
  Picker.create ~query ~title items
;;

let model_picker ?(logged_in_only = false) m ~query =
  open_picker
    m
    (Models { logged_in_only })
    (model_picker_value m ~logged_in_only ~query)
;;

let scoped_models_picker m =
  let checked =
    match m.config with
    | Some { P.Config.scoped_models = _ :: _ as keys; _ } ->
      String.Set.of_list keys
    | _ -> scoped_keys m
  in
  let items =
    List.map m.models ~f:(fun (model : P.Model.t) ->
      Picker.Item.create ~id:model.key ~detail:model.name model.name)
  in
  open_picker
    m
    Scoped_models
    (Picker.create ~multi:true ~checked ~title:"Scoped models" items)
;;

let thinking_picker m =
  let current = Option.map m.state ~f:(fun s -> s.thinking) in
  let items =
    List.map Commands.thinking_levels ~f:(fun level ->
      Picker.Item.create
        ~id:level
        ~detail:(if String.equal level "on" then "provider default" else "")
        ~marked:
          (Option.value_map current ~default:false ~f:(String.equal level))
        level)
  in
  open_picker m Thinking (Picker.create ~title:"Thinking level" items)
;;

let verbosity_picker m =
  let items =
    List.map Verbosity.all ~f:(fun verbosity ->
      Picker.Item.create
        ~id:(Verbosity.name verbosity)
        ~marked:(Verbosity.equal verbosity m.verbosity)
        (String.capitalize (Verbosity.name verbosity)))
  in
  open_picker m Verbosity (Picker.create ~title:"Transcript verbosity" items)
;;

let login_items m =
  List.concat_map m.auth ~f:(fun (s : P.Auth_status.t) ->
    List.map s.methods ~f:(fun meth ->
      let configured =
        match s.configured with
        | Some c when String.equal c.method_ meth.method_ ->
          sprintf "logged in via %s" c.source
        | _ -> ""
      in
      match s.custom with
      | Some c ->
        Picker.Item.create
          ~id:(s.provider ^ " " ^ meth.method_)
          ~detail:(sprintf "custom: %s (%s)" c.base_url c.api)
          ~search:(s.provider ^ " custom " ^ c.base_url)
          (s.name ^ " (edit)")
      | None ->
        Picker.Item.create
          ~id:(s.provider ^ " " ^ meth.method_)
          ~detail:(String.strip (meth.label ^ "  " ^ configured))
          ~marked:(not (String.is_empty configured))
          (s.name ^ " (" ^ meth.method_ ^ ")")))
  @ [ Picker.Item.create
        ~id:"custom api_key"
        ~detail:"aiproxy, LiteLLM, OpenRouter, vLLM, Ollama, ..."
        "custom (add an OpenAI-compatible endpoint)"
    ]
  @ Forge_accounts.login_items m.forge
;;

let login_picker m =
  open_picker m Login (Picker.create ~title:"Log in to" (login_items m))
;;

(* A custom provider's logout asks itself what to remove (a backend login
   prompt), so it needs no confirmation here. *)
let logout m provider =
  if
    List.exists m.auth ~f:(fun s ->
      String.equal s.provider provider && Option.is_some s.custom)
  then m, [ rpc "logout" ~params:[ "provider", str provider ] ]
  else
    ( { m with
        mode =
          Confirm
            { question =
                sprintf
                  "Log out of %s and delete its credential? (y/n)"
                  provider
            ; action = Logout provider
            }
      }
    , [] )
;;

let logout_items m =
  List.filter_map m.auth ~f:(fun (s : P.Auth_status.t) ->
    Option.map s.configured ~f:(fun c ->
      Picker.Item.create
        ~id:s.provider
        ~detail:
          (match s.custom with
           | Some custom ->
             sprintf "custom: %s, key: %s" custom.base_url c.source
           | None -> sprintf "%s via %s" c.method_ c.source)
        s.name))
  @ Forge_accounts.logout_items m.forge
;;

let logout_picker m =
  let items = logout_items m in
  if List.is_empty items
  then notice m "no provider or git host is signed in; /login signs in"
  else open_picker m Logout (Picker.create ~title:"Log out of" items)
;;

let current_session m = Option.map m.state ~f:(fun s -> s.session_path)

let sessions_picker ?named_only m (sessions : P.Session_summary.t list) =
  let selection = Session_selection.create ?named_only sessions in
  let picker =
    Session_selection.picker selection ~current:(current_session m)
  in
  if List.is_empty (Picker.visible picker)
  then notice m "no saved sessions"
  else open_picker m (Sessions selection) picker
;;

(* Back in the editor (unless something else is there now), to correct rather
   than retype. *)
let restore_command m command =
  if Editor.is_empty m.editor
  then { m with editor = Editor.set_text m.editor command }
  else m
;;

(* ---- git hosting ------------------------------------------------------ *)

let forge_login m (login : Forge_accounts.Login.t) =
  match login with
  | Host { host; kind; method_ } ->
    ( notice m (sprintf "signing in to %s…" host)
    , [ call (Forge_login { host; kind; method_ }) ] )
  | Ask_host { kind; method_ } ->
    ( { m with
        mode =
          Text_prompt
            { question = Forge_accounts.Login.host_question ~kind
            ; action = Forge_host { kind; method_ }
            }
      ; editor = Editor.clear m.editor
      ; autocomplete = None
      }
    , [] )
;;

let forge_logout m host =
  match Forge_accounts.find m.forge host with
  | Some account when not (P.Forge_account.signed_in account) ->
    ( notice
        m
        (sprintf
           "not signed in to %s; /login %s signs in"
           host
           (P.Forge_account.login_arg account))
    , [] )
  | account ->
    let user =
      match Option.bind account ~f:(fun a -> a.user) with
      | Some user -> sprintf " (%s)" user
      | None -> ""
    in
    ( { m with
        mode =
          Confirm
            { question =
                sprintf
                  "Sign out of %s%s and delete its token? (y/n)"
                  host
                  user
            ; action = Forge_logout host
            }
      }
    , [] )
;;

let pr_command m rest =
  match Pr_command.parse rest with
  | Error e -> error (restore_command m (String.strip ("/pr " ^ rest))) e, []
  | Ok (List { state }) ->
    ( notice m "reading pull requests…"
    , [ call (Pr_list { state }) ~tag:(Prs_picker state) ] )
  | Ok (View number) ->
    ( notice m (sprintf "reading pull request #%d…" number)
    , [ call (Pr_view number) ~tag:Pr_shown ] )
  | Ok (New { title }) ->
    ( notice m "reading the branch…"
    , [ call Pr_defaults ~tag:(Pr_form_defaults title) ] )
  | Ok (Checkout number) ->
    ( notice m (sprintf "checking out pull request #%d…" number)
    , [ call (Pr_checkout number) ~tag:(Pr_checked_out number) ] )
;;

let prs_picker m ~state (list : P.Pr_list.t) =
  match list.prs with
  | [] ->
    notice
      m
      (sprintf
         "no %spull requests in %s; /pr new creates one%s"
         (match state with
          | None -> "open "
          | Some "all" -> ""
          | Some state -> state ^ " ")
         (P.Forge_repo.slug list.repo)
         (match state with
          | Some "all" -> ""
          | _ -> ", /pr list all lists every one"))
  | prs ->
    open_picker
      m
      Pull_requests
      (Picker.create
         ~title:(Pr_view.picker_title list ~state)
         (List.map prs ~f:Pr_view.item))
;;

let pr_checked_out m number (checkout : P.Pr_checkout.t) =
  let what =
    sprintf
      "%s pull request #%d (%s) at %s"
      (if checkout.created then "checked out" else "updated")
      number
      checkout.branch
      checkout.path
  in
  if Mode.is_dialog m.mode
  then notice m (sprintf "%s: /cd %s goes there" what checkout.path)
  else
    { (notice m what) with
      mode =
        Confirm
          { question =
              sprintf
                "Change the session's directory to %s? (y/n)"
                checkout.path
          ; action = Cd_to_checkout checkout.path
          }
    ; autocomplete = None
    }
;;

let archive_verb ~archived = if archived then "archived" else "unarchived"

(* No argument: the current session; otherwise it is looked up in the list
   ([Session_to_archive]). *)
let archive_command m ~archived arg =
  match arg, m.state with
  | "", None -> error m "not connected", []
  | "", Some state ->
    ( m
    , [ call
          (Set_archived { path = state.session_path; archived })
          ~tag:
            (Notice_on_success
               (if archived
                then "archived this session (your next message unarchives it)"
                else "unarchived this session"))
      ] )
  | arg, _ ->
    m, [ rpc "list_sessions" ~tag:(Session_to_archive { arg; archived }) ]
;;

let archive_resolved m ~arg ~archived sessions =
  match Session_selection.resolve sessions [ arg ] with
  | Error e ->
    let command =
      sprintf "/%s %s" (if archived then "archive" else "unarchive") arg
    in
    error (restore_command m command) e, []
  | Ok found ->
    let set (s : P.Session_summary.t) =
      let notice =
        sprintf "%s %s" (archive_verb ~archived) (Session_selection.describe s)
      in
      call
        (Set_archived { path = s.path; archived })
        ~tag:(Notice_on_success notice)
    in
    m, List.map found ~f:set
;;

let format_stats (s : P.Session_stats.t) : Content.t =
  let tools =
    if List.is_empty s.tool_calls
    then "none"
    else
      String.concat
        ~sep:", "
        (List.map s.tool_calls ~f:(fun (name, count) ->
           sprintf "%s %d" name count))
  in
  let rows =
    [ "messages", Int.to_string s.message_count
    ; "turns", Int.to_string s.turns
    ; "tools", tools
    ; ( "usage"
      , sprintf
          "in %s out %s cache %s"
          (format_tokens s.usage.input)
          (format_tokens s.usage.output)
          (format_tokens s.usage.cache_read) )
    ; "cost", sprintf "$%.4f" s.cost_usd
    ; "context", sprintf "%.1f%%" s.context_percent
    ; "model changes", Int.to_string s.model_changes
    ; "compactions", Int.to_string s.compactions
    ; "duration", sprintf "%.1fs" s.duration_seconds
    ]
  in
  let width =
    List.fold rows ~init:0 ~f:(fun acc (label, _) ->
      Int.max acc (String.length label))
  in
  List.map rows ~f:(fun (label, value) ->
    [ { Content.Span.text = Text_width.pad_right label ~width
      ; style = Style.bold Style.plain
      }
    ; { text = "  " ^ value; style = Style.plain }
    ])
;;

let decode_entries json =
  let open Or_error.Let_syntax in
  let%bind head = P.Json.string_opt_field json "head" in
  let%map entries = P.Json.list_field json "entries" ~f:P.Entry.of_json in
  head, entries
;;

let first_line text =
  match String.split_lines text with
  | line :: _ -> line
  | [] -> ""
;;

let user_first_line = Injected_note.label

let message_first_line (m : P.Message.t) =
  match m with
  | P.Message.User user -> user_first_line user
  | P.Message.Assistant { content; _ } ->
    (match
       List.find_map content ~f:(function
         | P.Content.Text text -> Some text
         | _ -> None)
     with
     | Some text -> first_line text
     | None ->
       (match
          List.find_map content ~f:(function
            | P.Content.Server_tool s -> Some s
            | _ -> None)
        with
        | Some s -> Tool_summary.server_tool_line s
        | None -> "(assistant)"))
  | P.Message.Tool_result { tool_name; text; _ } ->
    let line = first_line text in
    if String.is_empty line then tool_name else line
;;

let user_entries entries =
  List.filter_map entries ~f:(fun (entry : P.Entry.t) ->
    match entry.kind with
    | P.Entry.Kind.Message (P.Message.User user) -> Some (entry, user)
    | _ -> None)
;;

let entry_picker m kind ~title entries =
  let users = user_entries entries in
  let count = List.length users in
  let items =
    List.mapi users ~f:(fun i ((entry : P.Entry.t), user) ->
      Picker.Item.create
        ~id:entry.id
        ~detail:(sprintf "#%d" (i + 1))
        ~marked:(i = count - 1)
        (user_first_line user))
  in
  if List.is_empty items
  then notice m "no user messages"
  else open_picker m (kind entries) (Picker.create ~title items)
;;

let fork_picker m entries =
  entry_picker m (fun entries -> Fork entries) ~title:"Fork at" entries
;;

let rewind_picker m entries =
  entry_picker m (fun entries -> Rewind entries) ~title:"Rewind to" entries
;;

let tree_items entries head =
  let by_id =
    String.Map.of_alist_reduce
      (List.map entries ~f:(fun (e : P.Entry.t) -> e.id, e))
      ~f:(fun first _ -> first)
  in
  let is_message (e : P.Entry.t) =
    match e.kind with
    | P.Entry.Kind.Message _ -> true
    | _ -> false
  in
  (* Settings entries (model, name, cwd, archived...) are not listed: a
     message hangs under the nearest message above it. *)
  let rec message_parent parent =
    match Option.bind parent ~f:(Map.find by_id) with
    | None -> parent
    | Some (e : P.Entry.t) ->
      if is_message e then Some e.id else message_parent e.parent
  in
  let entries =
    List.filter_map entries ~f:(fun (e : P.Entry.t) ->
      Option.some_if (is_message e) { e with parent = message_parent e.parent })
  in
  let ids =
    String.Set.of_list (List.map entries ~f:(fun (e : P.Entry.t) -> e.id))
  in
  let children =
    String.Map.of_alist_multi
      (List.filter_map entries ~f:(fun (e : P.Entry.t) ->
         Option.map e.parent ~f:(fun parent -> parent, e)))
  in
  let roots =
    List.filter entries ~f:(fun (e : P.Entry.t) ->
      match e.parent with
      | None -> true
      | Some parent -> not (Set.mem ids parent))
  in
  let active =
    let rec go id acc =
      match
        List.find entries ~f:(fun (e : P.Entry.t) -> String.equal e.id id)
      with
      | None -> acc
      | Some entry ->
        let acc = Set.add acc id in
        (match entry.parent with
         | None -> acc
         | Some parent -> go parent acc)
    in
    match message_parent head with
    | None -> String.Set.empty
    | Some head -> go head String.Set.empty
  in
  let items = ref [] in
  let rec walk depth (entry : P.Entry.t) =
    let glyph =
      match entry.kind with
      | P.Entry.Kind.Message (P.Message.User _) -> ">"
      | P.Entry.Kind.Message (P.Message.Assistant _) -> "·"
      | P.Entry.Kind.Message (P.Message.Tool_result _) -> "⚙"
      | P.Entry.Kind.Model _
      | P.Entry.Kind.Compaction _
      | P.Entry.Kind.Name _
      | P.Entry.Kind.Description _
      | P.Entry.Kind.Cwd _
      | P.Entry.Kind.Archived _
      | P.Entry.Kind.System_prompt
      | P.Entry.Kind.Other _ -> "·"
    in
    let label =
      sprintf
        "%s%s %s"
        (String.make (2 * depth) ' ')
        glyph
        (match entry.kind with
         | P.Entry.Kind.Message message -> message_first_line message
         | _ -> "")
    in
    items
    := Picker.Item.create ~id:entry.id ~marked:(Set.mem active entry.id) label
       :: !items;
    List.iter
      (Option.value (Map.find children entry.id) ~default:[])
      ~f:(walk (depth + 1))
  in
  List.iter roots ~f:(walk 0);
  List.rev !items
;;

let tree_picker m entries head =
  let items = tree_items entries head in
  if List.is_empty items
  then notice m "no messages"
  else open_picker m (Tree entries) (Picker.create ~title:"Session tree" items)
;;

let agent_status_text (a : Agent_view.t) =
  match a.status with
  | Running -> "running"
  | Done { turns; cost_usd } -> sprintf "done %d turns $%.2f" turns cost_usd
  | Failed _ -> "failed"
;;

let agents_picker m =
  if List.is_empty m.agents
  then notice m "no subagents", []
  else (
    let items =
      List.map m.agents ~f:(fun (a : Agent_view.t) ->
        Picker.Item.create
          ~id:a.id
          ~detail:(sprintf "%s  %s  %s" a.id (agent_status_text a) a.model)
          ~marked:
            (match m.focus with
             | `Agent id -> String.equal id a.id
             | `Main -> false)
          a.task)
    in
    open_picker m Agents (Picker.create ~title:"Subagents" items), [])
;;

let format_elapsed seconds =
  let s = Float.iround_down_exn seconds in
  if s < 60
  then sprintf "%ds" s
  else if s < 3600
  then sprintf "%dm%02ds" (s / 60) (s % 60)
  else sprintf "%dh%02dm" (s / 3600) (s % 3600 / 60)
;;

let jobs_picker m (jobs : P.Job_info.t list) =
  if List.is_empty jobs
  then notice m "no jobs", []
  else (
    let items =
      List.map jobs ~f:(fun (j : P.Job_info.t) ->
        let state =
          match j.exit with
          | None -> "running"
          | Some status -> status
        in
        Picker.Item.create
          ~id:j.id
          ~detail:
            (sprintf
               "%s  %s  %s%s"
               j.id
               state
               (format_elapsed j.elapsed)
               (Option.value_map j.last_line ~default:"" ~f:(fun l ->
                  "  " ^ String.prefix l 80)))
          ~search:(j.id ^ " " ^ j.command)
          j.command)
    in
    open_picker m Jobs (Picker.create ~title:"Jobs" items), [])
;;

let queue_picker m =
  let queued = shown_queue m in
  if List.is_empty queued
  then notice m "nothing queued", []
  else (
    let items =
      List.map queued ~f:(fun (q : Queued_message.t) ->
        Picker.Item.create
          ~id:(Queued_message.id q)
          ~detail:(Queued_message.Kind.describe q.kind)
          (Queued_message.one_line q))
    in
    open_picker m Queue (Picker.create ~title:"Queued" items), [])
;;

(* [edit]: back into the editor, as [Dequeue] does. *)
let unqueue m id ~edit =
  let m = { m with mode = Editing } in
  match Queued_message.find (shown_queue m) ~id with
  | None -> notice m "that message is no longer queued", []
  | Some q ->
    let method_, params =
      match m.focus with
      | `Main -> "remove_queued", Queued_message.remove_params q
      | `Agent agent_id ->
        ( "remove_agent_queued"
        , ("agent_id", str agent_id) :: Queued_message.remove_params q )
    in
    ( m
    , [ rpc
          method_
          ~params
          ~tag:
            (if edit
             then Dequeued
             else
               Notice_on_success
                 (sprintf
                    "removed from the queue: %s"
                    (Queued_message.one_line q)))
      ] )
;;

let kill_job m id =
  ( m
  , [ rpc
        "kill_job"
        ~params:[ "job_id", str id ]
        ~tag:(Notice_on_success (sprintf "killing job %s" id))
    ] )
;;

let job_output m id =
  ( { m with mode = Editing }
  , [ rpc
        "job_output"
        ~params:[ "job_id", str id; "lines", P.Json.int 200 ]
        ~tag:Job_output
    ] )
;;

(* ---- skills and MCP servers ------------------------------------------ *)

let skills_hint =
  "add one as .prigh/skills/<name>/SKILL.md (or .claude/skills/<name>/) in \
   the project, or under ~/.prigh/skills/"
;;

(* [/abs/proj/.claude/skills/x/SKILL.md] as [~/proj/.claude/skills/x]. *)
let skill_location m (skill : P.Skill.t) =
  let dir =
    Option.value
      (String.chop_suffix skill.path ~suffix:"/SKILL.md")
      ~default:skill.path
  in
  tilde m dir
;;

let skills_picker m (skills : P.Skill.t list) =
  if List.is_empty skills
  then notice m ("no skills here; " ^ skills_hint)
  else (
    let items =
      List.map skills ~f:(fun (s : P.Skill.t) ->
        let location = skill_location m s in
        Picker.Item.create
          ~id:s.name
          ~detail:
            (String.concat
               ~sep:"  "
               ([ s.description; location ]
                @ if s.model_invocable then [] else [ "(only you invoke it)" ]))
          ~search:(s.name ^ " " ^ s.description ^ " " ^ location)
          s.name)
    in
    open_picker m Skills (Picker.create ~title:"Skills" items))
;;

let open_skills m = m, [ call List_skills ~tag:Skills_picker ]

let mcp_config_hint =
  "configure servers under \"mcpServers\" in ~/.prigh/mcp.json or a \
   project's .mcp.json"
;;

let plural n word = sprintf "%d %s%s" n word (if n = 1 then "" else "s")

let mcp_status (s : P.Mcp_server.t) =
  match s.status with
  | Ready -> "ready, " ^ plural (List.length s.tools) "tool"
  | Failed -> "failed: " ^ Option.value s.error ~default:"unknown error"
  | Needs_approval -> "needs approval"
;;

let mcp_problems m (list : P.Mcp_list.t) =
  List.fold list.problems ~init:m ~f:(fun m problem ->
    warn m ("MCP config: " ^ problem))
;;

let mcp_picker ?(problems = true) m (list : P.Mcp_list.t) =
  let m = if problems then mcp_problems m list else m in
  if List.is_empty list.servers
  then notice m ("no MCP servers; " ^ mcp_config_hint)
  else (
    let items =
      List.mapi list.servers ~f:(fun i (s : P.Mcp_server.t) ->
        Picker.Item.create
          ~id:(Int.to_string i)
          ~detail:(mcp_status s ^ "  " ^ s.source)
          ~search:(s.name ^ " " ^ s.source)
          ~dimmed:(P.Mcp_server.Status.equal s.status Failed)
          s.name)
    in
    open_picker m (Mcp list) (Picker.create ~title:"MCP servers" items))
;;

let mcp_tools (s : P.Mcp_server.t) : Content.t =
  let header : Content.Line.t =
    [ { Content.Span.text = s.name; style = Style.bold (Style.fg Cyan) }
    ; { text = "  " ^ plural (List.length s.tools) "tool"; style = Style.plain }
    ; { text = "  " ^ s.source; style = Style.dim Style.plain }
    ]
  in
  let width =
    List.fold s.tools ~init:0 ~f:(fun acc (t : P.Mcp_server.Tool.t) ->
      Int.max acc (Text_width.string t.name))
  in
  header
  :: List.map s.tools ~f:(fun (t : P.Mcp_server.Tool.t) ->
    [ { Content.Span.text = "  " ^ Text_width.pad_right t.name ~width
      ; style = Style.bold Style.plain
      }
    ; { text = "  " ^ first_line t.description; style = Style.plain }
    ])
;;

let mcp_selected m (list : P.Mcp_list.t) id =
  match Option.bind (Int.of_string_opt id) ~f:(List.nth list.servers) with
  | None -> m, []
  | Some s ->
    (match s.status with
     | Ready -> output m ~title:("MCP: " ^ s.name) (mcp_tools s), []
     | Failed ->
       ( error
           m
           (sprintf
              "%s %s; check its entry in %s, then /mcp reconnect"
              s.name
              (mcp_status s)
              s.source)
       , [] )
     | Needs_approval ->
       ( notice m (sprintf "approving %s from %s…" s.name s.source)
       , [ call
             (Mcp_approve { source = s.source; server = s.name })
             ~tag:(Mcp_approved s.name)
         ] ))
;;

(* One line per server, for [/mcp reconnect]. *)
let mcp_summary (list : P.Mcp_list.t) : Content.t =
  let width =
    List.fold list.servers ~init:0 ~f:(fun acc (s : P.Mcp_server.t) ->
      Int.max acc (Text_width.string s.name))
  in
  List.map list.servers ~f:(fun (s : P.Mcp_server.t) ->
    let style =
      match s.status with
      | Ready -> Style.fg Green
      | Failed -> Style.fg Red
      | Needs_approval -> Style.fg Yellow
    in
    [ { Content.Span.text = Text_width.pad_right s.name ~width
      ; style = Style.bold Style.plain
      }
    ; { text = "  " ^ mcp_status s; style }
    ; { text = "  " ^ s.source; style = Style.dim Style.plain }
    ])
;;

(* A host's display name: "(here)" marks this frontend, "(in ...)" a client
   attached to another session. *)
let host_label m (h : P.Host.t) =
  if Option.value_map m.client_id ~default:false ~f:(String.equal h.id)
  then h.name ^ " (here)"
  else (
    let ours = Option.map m.state ~f:(fun (s : P.State.t) -> s.session_id) in
    match h.session_id with
    | Some id when not (Option.equal String.equal ours (Some id)) ->
      sprintf "%s (in %s)" h.name (Option.value h.session_name ~default:id)
    | _ -> h.name)
;;

let hosts_picker m =
  match m.state with
  | None -> notice m "not connected", []
  | Some s ->
    let items =
      List.map s.hosts ~f:(fun h ->
        Picker.Item.create
          ~id:h.id
          ~detail:h.cwd
          ~search:(h.name ^ " " ^ h.id ^ " " ^ h.cwd)
          ~marked:(String.equal h.id s.active_host)
          (host_label m h))
    in
    open_picker m Hosts (Picker.create ~title:"Tool host" items), []
;;

(* The session cwd belongs to the host, so switching asks for the directory to
   use there, prefilled with the current one. *)
let host_cwd_prompt m (host : P.Host.t) =
  let current =
    Option.value_map m.state ~default:host.cwd ~f:(fun s -> s.cwd)
  in
  { m with
    mode =
      Text_prompt
        { question = sprintf "Working directory on %s" (host_label m host)
        ; action = Host_cwd host.id
        }
  ; editor = Editor.set_text (Editor.clear m.editor) current
  ; autocomplete = None
  }
;;

let change_dir ?(if_missing = P.Dir_change.If_missing.Ask) ~target path =
  call (Change_dir { target; path; if_missing }) ~tag:(Dir_changed target)
;;

let host_target m id : P.Dir_change.Target.t =
  let name =
    Option.bind m.state ~f:(fun (s : P.State.t) ->
      List.find s.hosts ~f:(fun h -> String.equal h.id id))
    |> Option.value_map ~default:id ~f:(fun (h : P.Host.t) -> h.name)
  in
  Host { id; name }
;;

let set_host_command m ~host ~cwd = change_dir ~target:(host_target m host) cwd

let create_dir_question (target : P.Dir_change.Target.t) path =
  match target with
  | Cwd -> sprintf "Create %s? (y/n)" path
  | Host { name; _ } -> sprintf "Create %s on %s? (y/n)" path name
;;

let switch_host m arg =
  match m.state with
  | None -> notice m "not connected", []
  | Some s ->
    let arg = String.strip arg in
    let matches =
      List.filter s.hosts ~f:(fun h ->
        String.equal h.id arg
        || String.equal h.name arg
        || (String.equal arg "here"
            && Option.value_map
                 m.client_id
                 ~default:false
                 ~f:(String.equal h.id)))
    in
    (match matches with
     | [ h ] -> host_cwd_prompt m h, []
     | [] ->
       ( error
           m
           (sprintf
              "unknown host %S; one of: %s"
              arg
              (String.concat ~sep:", " (List.map s.hosts ~f:(host_label m))))
       , [] )
     | _ -> hosts_picker m)
;;

let set_focus m focus = follow { m with focus }

(* Agents stay listed while they run and until the backend has handed their
   report to the main agent; after that, until the next prompt. *)
let prune_agents m =
  let undelivered id =
    Option.exists m.state ~f:(fun (s : P.State.t) ->
      List.exists s.subagents ~f:(fun a -> String.equal a.id id))
  in
  let agents =
    List.filter m.agents ~f:(fun (a : Agent_view.t) ->
      Agent_view.Status.equal a.status Running || undelivered a.id)
  in
  let focus =
    match m.focus with
    | `Agent id when not (List.exists agents ~f:(fun a -> String.equal a.id id))
      -> `Main
    | focus -> focus
  in
  { m with agents; focus }
;;

(* The session's subagents whose events this client missed (it switched
   session or reconnected) join the list at once and their transcripts
   follow. *)
let set_state m (state : P.State.t) =
  let missed =
    List.filter state.subagents ~f:(fun a ->
      Option.is_none (Agent_view.find m.agents a.id))
  in
  let adopted =
    List.map missed ~f:(fun a ->
      let agent =
        Agent_view.create ~call_id:"" ~agent_id:a.id ~task:a.task ~model:""
      in
      if a.running
      then agent
      else { agent with status = Done { turns = 0; cost_usd = 0. } })
  in
  ( { m with state = Some state; agents = m.agents @ adopted }
  , List.map missed ~f:(fun a ->
      rpc "get_subagent" ~params:[ "id", str a.id ] ~tag:Subagent_loaded) )
;;

let subagent_loaded m (d : P.Subagent_detail.t) =
  update_agents m ~f:(fun agents ->
    Agent_view.replace agents d.id (fun a ->
      { a with
        call_id = d.call_id
      ; model = d.model
      ; turns = d.turns
      ; status =
          (match d.result with
           | None -> a.status
           | Some result ->
             Agent_view.status_of_result ~turns:d.turns ~cost_usd:0. result)
      ; transcript =
          List.fold
            d.messages
            ~init:(Transcript.clear a.transcript)
            ~f:Transcript.add_message
      }))
;;

(* [arg] is the agent's number in the strip or its id. *)
let cancel_agent m arg =
  match Agents_command.find m.agents arg with
  | None when List.is_empty m.agents -> error m "no subagents to cancel", []
  | None ->
    ( error
        m
        (sprintf
           "no subagent %s; /agents lists them (1–%d), Ctrl+D there cancels"
           arg
           (List.length m.agents))
    , [] )
  | Some a when not (Agent_view.Status.equal a.status Running) ->
    notice m (sprintf "subagent %s is not running" a.id), []
  | Some a ->
    ( m
    , [ rpc
          "cancel_subagent"
          ~params:[ "agent_id", str a.id ]
          ~tag:(Notice_on_success (sprintf "cancelling subagent %s" a.id))
      ] )
;;

(* What the user types to a focused subagent, or with [/agents steer]: a
   finished one takes nothing, and the text stays to send elsewhere. *)
let message_agent
      m
      (a : Agent_view.t)
      (kind : Queued_message.Kind.t)
      text
      ~retry
      ~finished_hint
      ~ack
  =
  match a.status with
  | Done _ | Failed _ ->
    ( error
        (restore_command m retry)
        (sprintf "%s has finished: %s" a.id finished_hint)
    , [] )
  | Running ->
    let m =
      { m with
        agent_queues = Agent_queues.add m.agent_queues ~agent_id:a.id kind text
      }
    in
    let m =
      if ack
      then
        notice
          m
          (sprintf
             "%s for %s queued: %s"
             (Queued_message.Kind.name kind)
             a.id
             (match kind with
              | Steer -> "it reads it at its next step"
              | Follow_up -> "it reads it before it reports"))
      else m
    in
    ( m
    , [ rpc
          "steer_agent"
          ~params:
            [ "agent_id", str a.id
            ; "text", str text
            ; "mode", str (Queued_message.Kind.wire kind)
            ]
          ~tag:(Agent_message { agent_id = a.id; kind; text; retry })
      ; Command.Append_history text
      ; push_history text
      ] )
;;

let agent_message_failed m ~agent_id ~kind ~text ~retry e =
  let m =
    { m with
      agent_queues = Agent_queues.remove m.agent_queues ~agent_id kind text
    }
  in
  error (restore_command m retry) e
;;

let agents_command m rest =
  match Agents_command.run rest with
  | Pick -> agents_picker m
  | Cancel arg -> cancel_agent m arg
  | Error e -> error (restore_command m ("/agents " ^ rest)) e, []
  | Message { agent; kind; text } ->
    (match Agents_command.find m.agents agent with
     | None ->
       ( error
           (restore_command m ("/agents " ^ rest))
           (Agents_command.unknown m.agents agent)
       , [] )
     | Some a ->
       message_agent
         m
         a
         kind
         text
         ~retry:("/agents " ^ rest)
         ~finished_hint:"tell the main agent instead"
         ~ack:true)
;;

let cycle_focus m =
  let n = List.length m.agents in
  match m.focus with
  | `Main -> if n = 0 then m else set_focus m (`Agent (List.hd_exn m.agents).id)
  | `Agent id ->
    (match List.findi m.agents ~f:(fun _ a -> String.equal a.id id) with
     | Some (i, _) when i + 1 < n ->
       set_focus m (`Agent (List.nth_exn m.agents (i + 1)).id)
     | _ -> set_focus m `Main)
;;

let focus_agent m n =
  match List.nth m.agents (n - 1) with
  | Some a -> set_focus m (`Agent a.id)
  | None -> m
;;

(* ---- auth ------------------------------------------------------------- *)

let format_auth m : Content.t =
  let statuses = m.auth in
  let width =
    List.fold statuses ~init:0 ~f:(fun acc s ->
      Int.max acc (String.length s.provider))
  in
  List.map statuses ~f:(fun (s : P.Auth_status.t) ->
    let methods =
      String.concat
        ~sep:", "
        (List.map s.methods ~f:(fun m -> m.method_ ^ " (" ^ m.label ^ ")"))
    in
    let state : Content.Line.t =
      match s.configured, s.custom with
      | configured, Some custom ->
        [ { text = "custom"; style = Style.fg Cyan }
        ; { text = sprintf " %s (%s)" custom.base_url custom.api
          ; style = Style.plain
          }
        ; { text =
              (match configured with
               | Some { source = "no key"; _ } | None -> "  no key"
               | Some c -> "  key: " ^ c.source)
          ; style = Style.dim Style.plain
          }
        ]
      | Some c, None ->
        [ { text = "logged in"; style = Style.fg Green }
        ; { text = " via " ^ c.source; style = Style.plain }
        ]
      | None, None -> [ { text = "not configured"; style = Style.fg Gray } ]
    in
    [ { Content.Span.text = Text_width.pad_right s.provider ~width ^ "  "
      ; style = Style.bold Style.plain
      }
    ]
    @ state
    @
    if Option.is_some s.custom
    then []
    else [ { text = "  [" ^ methods ^ "]"; style = Style.dim Style.plain } ])
  @ [ [ { Content.Span.text = "/login custom adds an OpenAI-compatible endpoint"
        ; style = Style.dim Style.plain
        }
      ]
    ]
  @ if List.is_empty m.forge then [] else [] :: Forge_accounts.describe m.forge
;;

let logins_title = "Logins"

(* What lists the git hosts is redrawn when they change. *)
let forge_refreshed m forge =
  let m = { m with forge } in
  match m.mode with
  | Picker { kind = Login; picker } ->
    { m with
      mode =
        Picker
          { kind = Login; picker = Picker.set_items picker (login_items m) }
    }
  | Picker { kind = Logout; picker } ->
    { m with
      mode =
        Picker
          { kind = Logout; picker = Picker.set_items picker (logout_items m) }
    }
  | Output box when String.equal box.title logins_title ->
    { m with mode = Output { box with lines = format_auth m } }
  | _ -> m
;;

(* ---- reconnection ----------------------------------------------------- *)

let schedule_reconnect m ~attempt ~delay_ms ~session =
  let generation = m.reconnect_generation + 1 in
  ( { m with
      connection = Reconnecting { attempt; generation; delay_ms; session }
    ; reconnect_generation = generation
    }
  , [ Command.Reconnect
        { generation; delay_ms; session; as_user = Model.acting_as m }
    ] )
;;

(* Closes the dialogs answering the session's agent and drops what is waiting
   to be shown; [get_pending] brings back what is still open. *)
let drop_pending m =
  let m = { m with pending_confirms = []; pending_questions = [] } in
  match m.mode with
  | Confirm { action = Tool_confirm _; _ }
  | Question _
  | Picker { kind = Agents | Queue | Reminders | Worktrees _; _ }
  | Confirm { action = Remove_worktree _; _ } -> { m with mode = Editing }
  | Text_prompt { action = Deny_reason { draft; _ }; _ } ->
    { m with mode = Editing; editor = Editor.set_text m.editor draft }
  | _ -> m
;;

(* Everything that belongs to the session shown, reset when it is replaced
   (a switch, another user, a reconnect), and the requests that refill it. *)
let reset_session m =
  let m = drop_pending m in
  ( follow
      { m with
        mode =
          (match m.mode with
           | Output _
           | Pr_form _
           | Picker { kind = Pull_requests; _ }
           | Confirm { action = Cd_to_checkout _; _ } -> Editing
           | mode -> mode)
      ; notice = None
      ; skills = Location_cache.empty
      ; worktrees = Location_cache.empty
      ; agents = []
      ; focus = `Main
      ; queued_messages = []
      ; agent_queues = []
      ; transcript = Transcript.clear m.transcript
      ; activity = Idle
      }
  , [ rpc "get_state" ~tag:Initial_state
    ; rpc "get_messages" ~tag:Initial_messages
    ; rpc "get_pending" ~tag:Pending
    ] )
;;

let backend_closed m =
  let m =
    { (drop_pending m) with
      state = Option.map m.state ~f:(fun s -> { s with running = false })
    }
  in
  let m =
    log
      ~severity:Error
      m
      "backend connection lost; reconnecting (/retry-backend-connection to \
       retry now, Ctrl+C quits)"
  in
  let m =
    if List.is_empty m.stderr_tail
    then m
    else
      log_block
        m
        (Content.lines
           ~style:(Style.dim Style.plain)
           (String.concat ~sep:"\n" m.stderr_tail))
  in
  schedule_reconnect
    { m with stderr_tail = [] }
    ~attempt:1
    ~delay_ms:0
    ~session:(Option.map m.state ~f:(fun s -> s.session_path))
;;

(* ---- the server: /connect, /signin, /signout ---------------------------- *)

let remote_address m =
  Option.bind m.server ~f:(fun (s : Server_link.t) -> s.address)
;;

let server_user m =
  Option.first_some
    (Option.bind m.server ~f:(fun (s : Server_link.t) -> s.user))
    m.user
;;

(* Everything that came from the old connection (the session, the user, the
   models, any dialog about them), as when switching users; the editor's
   draft and the prompt history are the user's and stay. *)
let reset_connection m =
  let m, (_ : Command.t list) = reset_session m in
  { m with
    state = None
  ; models = []
  ; auth = []
  ; forge = []
  ; sessions = None
  ; config = None
  ; client_id = None
  ; namespace = None
  ; user = None
  ; superuser = false
  ; stderr_tail = []
  ; known_paths = String.Set.empty
  ; login_lines = []
  ; btw = None
  ; nix_gc = None
  ; mode = Editing
  ; autocomplete = None
  ; pending_quit = false
  }
;;

let sign_in_dialog ?error ?user m =
  match remote_address m with
  | None -> m
  | Some address ->
    { m with
      mode =
        Sign_in
          (Sign_in_form.create
             ~address
             ~user:(Option.first_some user (server_user m))
             ~error)
    ; autocomplete = None
    }
;;

(* The server refused our credentials: nothing of it is shown any more, and
   the sign-in dialog asks for others. *)
let signed_out ?user m ~reason =
  let user = Option.first_some user (server_user m) in
  let form =
    match m.mode with
    | Sign_in form -> Some form
    | _ -> None
  in
  let m = { (reset_connection m) with connection = Disconnected { reason } } in
  match form with
  | Some form ->
    { m with mode = Sign_in (Sign_in_form.failed form ~error:reason) }
  | None -> sign_in_dialog m ?user ~error:reason
;;

let not_here m command =
  ( error
      m
      (sprintf
         "%s is for the terminal UI (prigh-tui); here, /signout signs in again \
          or to another backend"
         command)
  , [] )
;;

let relink ?doing m change =
  match m.connection with
  | Switching _ -> warn m "still connecting; wait for it, or Ctrl+C quits", []
  | previous ->
    let generation = m.reconnect_generation + 1 in
    let m =
      { m with
        connection = Switching { generation; previous; notices = [] }
      ; reconnect_generation = generation
      }
    in
    ( Option.value_map doing ~default:m ~f:(notice m)
    , [ Command.Relink { generation; change } ] )
;;

let connect_hint =
  "/connect HOST:PORT (of prigh serve -listen), /connect wss://HOST/ws (a \
   prigh-web URL, e.g. behind an HTTPS proxy), or /connect local"
;;

let connect m arg =
  match m.server with
  | None -> not_here m "/connect"
  | Some server ->
    (match String.strip arg with
     | "" ->
       ( { m with
           mode =
             Text_prompt
               { question =
                   sprintf
                     "Connect to HOST:PORT, a prigh-web URL (wss://…) or local \
                      (now: %s)"
                     (Server_link.describe server)
               ; action = Connect
               }
         ; editor =
             Editor.set_text
               m.editor
               (Option.value
                  (Option.first_some server.address server.saved)
                  ~default:"")
         ; autocomplete = None
         }
       , [] )
     | "local" -> relink m (Connect None) ~doing:"starting a local backend…"
     | address ->
       (match Server_address.parse address with
        | Error e -> error m (sprintf "%s; or /connect local" e), []
        | Ok parsed ->
          relink
            m
            (Connect (Some (Server_address.normalize address)))
            ~doing:(sprintf "connecting to %s…" (Server_address.display parsed))))
;;

let nothing_to_sign_in m what =
  ( notice
      m
      (sprintf
         "prigh-tui started this backend: there is nothing to %s (/connect \
          HOST:PORT for a remote one)"
         what)
  , [] )
;;

let sign_in m args =
  match m.server, args with
  | None, _ -> not_here m "/signin"
  | Some { address = None; _ }, _ -> nothing_to_sign_in m "sign in to"
  | Some _, ([] | [ _ ]) -> sign_in_dialog m ?user:(List.hd args), []
  | Some _, _ -> error m "usage: /signin [user]", []
;;

let sign_out m =
  match m.server, m.connection with
  | None, _ -> m, [ Command.Sign_out ]
  | Some { address = None; _ }, _ -> nothing_to_sign_in m "sign out of"
  | Some _, Switching _ ->
    warn m "still connecting; wait for it, or Ctrl+C quits", []
  | Some { address = Some address; _ }, _ ->
    let m = signed_out { m with mode = Editing } ~reason:"signed out" in
    ( notice
        m
        (sprintf
           "signed out of %s; the saved token is forgotten"
           (Server_address.display_string address))
    , [ Command.Sign_out ] )
;;

let connected_notice m (link : Server_link.t) =
  match link.address with
  | None -> "connected to a local backend"
  | Some address ->
    let address = Server_address.display_string address in
    (match m.user, m.namespace with
     | Some user, _ | None, Some user ->
       sprintf "connected to %s as %s" address user
     | None, None -> sprintf "connected to %s" address)
;;

let relinked m ~generation (outcome : Server_link.Outcome.t) =
  match m.connection with
  | Switching { generation = current; previous; notices }
    when generation = current ->
    let m = { m with server = Some outcome.link } in
    (match outcome.result with
     | Unreachable e ->
       let m = { m with connection = previous } in
       let m =
         match m.mode with
         | Sign_in form ->
           { m with mode = Sign_in (Sign_in_form.failed form ~error:e) }
         | _ ->
           error
             m
             (sprintf
                "%s; still on %s (is prigh serve -listen running there?)"
                e
                (Server_link.describe outcome.link))
       in
       (match previous with
        | Reconnecting { attempt; session; _ } ->
          schedule_reconnect m ~attempt ~delay_ms:0 ~session
        | Connected | Switching _ | Disconnected _ -> m, [])
     | Rejected e when Server_link.is_rejection e -> signed_out m ~reason:e, []
     | Rejected e ->
       let m =
         { (reset_connection m) with connection = Disconnected { reason = e } }
       in
       ( error
           m
           (sprintf
              "%s: %s; %s"
              (Server_link.describe outcome.link)
              e
              connect_hint)
       , [] )
     | Connected json ->
       let m = reset_connection m in
       let m =
         match P.Hello_reply.of_json json with
         | Ok { client_id; namespace; user; superuser } ->
           { m with client_id = Some client_id; namespace; user; superuser }
         | Error _ -> m
       in
       let m = { m with connection = Connected } in
       let m =
         List.fold_right notices ~init:m ~f:(fun text m ->
           with_transcript m ~f:(fun t ->
             Transcript.apply t (P.Event.Notice text)))
       in
       follow (notice m (connected_notice m outcome.link)), refill_commands)
  | _ -> m, []
;;

let retry_backend_connection m =
  match m.connection with
  | Connected -> notice m "backend is connected", []
  | Reconnecting { attempt; session; _ } ->
    schedule_reconnect (notice m "reconnecting…") ~attempt ~delay_ms:0 ~session
  | Switching _ -> notice m "connecting already…", []
  | Disconnected { reason } ->
    ( warn
        m
        (sprintf
           "not connected (%s): /signin signs in, /connect switches server"
           reason)
    , [] )
;;

let reconnect_reply m ~generation result =
  match m.connection with
  | Reconnecting { generation = current; attempt; session; _ }
    when generation = current ->
    (match result with
     | Error e
       when Server_link.is_rejection e && Option.is_some (remote_address m) ->
       signed_out m ~reason:e, []
     | Error e when Option.is_some session && Connection.session_missing e ->
       schedule_reconnect
         (log
            ~severity:Warn
            m
            (sprintf
               "%s; the session was never saved or has been deleted, starting \
                a new one"
               e))
         ~attempt
         ~delay_ms:0
         ~session:None
     | Error e ->
       let attempt = Connection.next_attempt attempt in
       let delay_ms = Connection.delay_ms ~attempt in
       schedule_reconnect
         (warn
            m
            (sprintf
               "reconnect failed: %s; retrying in %gs (attempt %d)"
               e
               (Float.of_int delay_ms /. 1000.)
               attempt))
         ~attempt
         ~delay_ms
         ~session
     | Ok json ->
       let client_id, namespace, user, superuser =
         match P.Hello_reply.of_json json with
         | Ok { client_id; namespace; user; superuser } ->
           Some client_id, namespace, user, superuser
         | Error _ -> m.client_id, m.namespace, m.user, m.superuser
       in
       let m, session_commands =
         reset_session
           { m with
             connection = Connected
           ; client_id
           ; namespace
           ; user
           ; superuser
           }
       in
       let m =
         match session, m.state with
         | None, Some { session_path; _ } ->
           log
             ~severity:Warn
             (notice m "reconnected to the backend")
             (sprintf
                "session %s no longer exists (it was never saved, or was \
                 deleted); starting a new one"
                session_path)
         | _ -> notice m "reconnected to the backend"
       in
       ( follow m
       , session_commands
         @ [ rpc "auth_status" ~tag:Auth_refresh
           ; call Forge_status ~tag:Forge_refresh
           ; rpc "get_config" ~tag:Config
           ; rpc "list_models" ~tag:Models_catalog
           ] ))
  | Connected | Reconnecting _ | Switching _ | Disconnected _ -> m, []
;;

(* ---- slash commands --------------------------------------------------- *)

let set_model_command key =
  rpc "set_model" ~params:[ "model", str key ] ~tag:(Set_model_done key)
;;

let export_command path =
  let format =
    if String.is_suffix path ~suffix:".jsonl" then "jsonl" else "markdown"
  in
  rpc
    "export"
    ~params:[ "format", str format; "path", str path ]
    ~tag:Export_done
;;

let switch_model m arg =
  match Model_match.resolve m.models arg with
  | Found model -> m, [ set_model_command model.key ]
  | Ambiguous _ ->
    ( model_picker (notice m (sprintf "several models match %S" arg)) ~query:arg
    , [] )
  | Not_found suggestions ->
    let hint =
      match suggestions with
      | [] -> ""
      | l ->
        sprintf
          " (did you mean %s?)"
          (String.concat ~sep:", " (List.map l ~f:(fun s -> s.name)))
    in
    let m = error m (sprintf "unknown model %S%s; pick one below" arg hint) in
    model_picker m ~query:"", []
;;

let cycle_model m ~step =
  let scope = effective_scope m in
  match scope with
  | [] -> notice m "no models in scope", []
  | [ _ ] -> notice m "only one model in scope", []
  | models ->
    let n = List.length models in
    let index =
      match Option.map m.state ~f:(fun s -> s.model.key) with
      | None -> 0
      | Some key ->
        Option.value_map
          (List.findi models ~f:(fun _ (model : P.Model.t) ->
             String.equal model.key key))
          ~default:0
          ~f:fst
    in
    let next = List.nth_exn models ((index + step + n) mod n) in
    let m =
      match m.state with
      | Some s -> { m with state = Some { s with model = next } }
      | None -> m
    in
    m, [ set_model_command next.key ]
;;

let thinking_cycle = Commands.thinking_levels

let cycle_thinking m =
  match m.state with
  | None -> notice m "not connected", []
  | Some s when not s.model.supports_thinking ->
    notice m "thinking: n/a for this model", []
  | Some s ->
    let index =
      Option.value_map
        (List.findi thinking_cycle ~f:(fun _ level ->
           String.equal level s.thinking))
        ~default:0
        ~f:fst
    in
    let next =
      List.nth_exn thinking_cycle ((index + 1) mod List.length thinking_cycle)
    in
    let m = { m with state = Some { s with thinking = next } } in
    ( notice m (sprintf "thinking: %s" next)
    , [ rpc "set_thinking" ~params:[ "thinking", str next ] ] )
;;

(* The backend keeps the fields left out, so each command sends only its own:
   a stale or missing [m.config] cannot undo another change. *)
let set_config ~tag fields =
  rpc "set_config" ~params:[ "config", `Object fields ] ~tag
;;

let notify_summary_of (mode : Notify.Mode.t) (focus : Focus.t) =
  match mode with
  | Off -> "notifications off (/notify bell or /notify desktop turns them on)"
  | Bell | Desktop ->
    sprintf
      "notifications: %s when a turn ends, a tool waits for you or background \
       work finishes, while this terminal is not focused%s"
      (match mode with
       | Desktop -> "a desktop notification"
       | Bell | Off -> "the bell")
      (match focus with
       | Unknown ->
         " (it has not reported focus yet: in tmux, set -g focus-events on)"
       | Focused | Unfocused -> "")
;;

let notify_summary m =
  notify_summary_of (Notify.Mode.of_config m.config) m.terminal_focus
;;

(* While the user looks elsewhere: the terminal says so when it reports
   focus; one that never has is assumed to have it. *)
let notify m (e : P.Event.t) =
  match Notify.Mode.of_config m.config, m.terminal_focus with
  | Off, _ | _, (Unknown | Focused) -> []
  | ((Bell | Desktop) as mode), Unfocused ->
    (match Notify.of_event e ~where:(Notify.where m.state) with
     | Some notification -> [ Command.Notify { mode; notification } ]
     | None -> [])
;;

(* The default mode for new sessions, which the backend applies to this
   one too. *)
let set_confirm m enabled =
  let approval_mode : P.Approval_mode.t = if enabled then Ask else Auto in
  let label =
    sprintf
      "tool confirmation %s (approval mode %s, also for new sessions)"
      (if enabled then "on" else "off")
      (P.Approval_mode.to_string approval_mode)
  in
  ( { m with
      config =
        Option.map m.config ~f:(fun config ->
          { config with P.Config.approval_mode })
    }
  , [ set_config
        ~tag:(Notice_on_success label)
        [ "approval_mode", str (P.Approval_mode.to_string approval_mode) ]
    ] )
;;

(* Shown at once; the backend's state follows. *)
let set_mode m (mode : Session_mode.t) =
  match m.state with
  | None -> notice m "not connected", []
  | Some s ->
    let m =
      { m with
        state =
          Some { s with approval_mode = mode.approval; plan_mode = mode.plan }
      }
    in
    ( m
    , [ rpc
          "set_mode"
          ~params:(Session_mode.params mode)
          ~tag:
            (Mode_set
               (sprintf
                  "mode: %s — %s"
                  (Session_mode.label mode)
                  (Session_mode.describe mode)))
      ] )
;;

let cycle_mode m =
  match m.state with
  | None -> notice m "not connected", []
  | Some s -> set_mode m (Session_mode.cycle (Session_mode.of_state s))
;;

let mode_picker m =
  match m.state with
  | None -> notice m "not connected", []
  | Some s ->
    let current = Session_mode.of_state s in
    let items =
      List.map (Session_mode.choices ~current) ~f:(fun mode ->
        Picker.Item.create
          ~id:(Session_mode.label mode)
          ~detail:(Session_mode.describe mode)
          ~marked:(Session_mode.equal mode current)
          (Session_mode.label mode))
    in
    open_picker m Modes (Picker.create ~title:"Mode (Alt+M cycles)" items), []
;;

let mode_by_name m name =
  match m.state with
  | None -> notice m "not connected", []
  | Some s ->
    (match Session_mode.of_name ~current:(Session_mode.of_state s) name with
     | Some mode -> set_mode m mode
     | None ->
       ( error
           m
           (sprintf
              "unknown mode %S; use %s"
              name
              (String.concat ~sep:", " Session_mode.names))
       , [] ))
;;

let show_plan m =
  match Option.bind m.state ~f:(fun s -> s.plan) with
  | None ->
    notice m "no plan: the model has not made one (it uses update_plan)", []
  | Some plan ->
    ( output
        m
        ~title:"Plan"
        (Content.lines
           (String.concat
              ~sep:"\n"
              (sprintf
                 "plan %d/%d"
                 (P.Plan.completed plan)
                 (List.length plan.steps)
               :: Plan_view.lines plan)))
    , [] )
;;

let show_permissions m =
  match m.state with
  | None -> notice m "not connected", []
  | Some s ->
    let mode = Session_mode.of_state s in
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
      [ sprintf
          "mode: %s — %s"
          (Session_mode.label mode)
          (Session_mode.describe mode)
      ]
      @ rules "allowed this session" s.session_rules
      @ config
      @ [ "rules: permissions in ~/.prigh/config.json, e.g. {\"allow\": \
           [\"bash(git status:*)\"], \"deny\": [\"bash(rm:*)\"]}; /mode or \
           Alt+M switches the mode"
        ]
    in
    ( output
        m
        ~title:"Permissions"
        (Content.lines (String.concat ~sep:"\n" lines))
    , [] )
;;

let fallback_summary m (config : P.Config.t) =
  match config.fallback_models with
  | [] ->
    "no fallback models: /fallback MODEL [MODEL...] sets the chain that takes \
     over when a model's usage runs out"
  | chain ->
    let now =
      Option.value_map m.state ~default:"" ~f:(fun s ->
        sprintf " (now on %s)" s.model.key)
    in
    sprintf "fallback: %s%s" (String.concat ~sep:" → " chain) now
;;

let default_dir_summary (config : P.Config.t) =
  match config.default_cwd with
  | Some dir ->
    sprintf
      "default directory: %s (new sessions start there; /default-dir off \
       clears it)"
      dir
  | None ->
    "no default directory: /default-dir PATH makes new sessions start there"
;;

let show_config m ~tag ~summary =
  match m.config with
  | Some config -> notice m (summary config), []
  | None -> m, [ rpc "get_config" ~tag ]
;;

let set_fallback m (cmd : Commands.Parsed.t) =
  let models =
    match cmd.args with
    | [ "off" ] -> []
    | models -> models
  in
  ( m
  , [ set_config
        ~tag:(Fallback_saved ("/fallback " ^ cmd.rest))
        [ "fallback_models", `Array (List.map models ~f:str) ]
    ] )
;;

let set_default_dir m (cmd : Commands.Parsed.t) =
  let dir =
    match cmd.args with
    | [ "off" ] -> `Null
    | _ -> str cmd.rest
  in
  ( m
  , [ set_config
        ~tag:(Default_dir_saved ("/default-dir " ^ cmd.rest))
        [ "default_cwd", dir ]
    ] )
;;

(* ---- snippets ---------------------------------------------------------- *)

let snippets m = Option.map m.config ~f:(fun (c : P.Config.t) -> c.snippets)

let last_prompt m =
  Transcript.items m.transcript
  |> List.rev
  |> List.find_map ~f:(function
    | Transcript.Item.User user ->
      Option.some_if
        (not (String.is_empty (String.strip (Injected_note.typed user))))
        (Injected_note.typed user)
    | _ -> None)
;;

let snippet_picker m (snippets : P.Snippet.t list) =
  let items =
    List.map snippets ~f:(fun (s : P.Snippet.t) ->
      Picker.Item.create
        ~id:s.name
        ~detail:(P.Snippet.first_line s)
        ~search:(s.name ^ " " ^ s.text)
        s.name)
  in
  open_picker m Snippets (Picker.create ~title:"Snippets" items)
;;

let insert_snippet m (snippet : P.Snippet.t) =
  { m with mode = Editing; editor = Editor.insert m.editor snippet.text }
;;

let snippet_action m (action : Snippet_command.Action.t) =
  match action with
  | Pick snippets -> snippet_picker m snippets, []
  | Insert snippet -> insert_snippet m snippet, []
  | Save { snippets; notice } ->
    ( m
    , [ set_config
          ~tag:(Snippets_saved notice)
          (Snippet_command.config_fields snippets)
      ] )
  | Info text -> notice m text, []
  | Error text -> error m text, []
;;

(* ---- reminders --------------------------------------------------------- *)

let reminders m = Option.map m.state ~f:(fun (s : P.State.t) -> s.reminders)

let reminder_picker m (reminders : P.Reminder.t list) =
  let items =
    List.map reminders ~f:(fun (r : P.Reminder.t) ->
      Picker.Item.create
        ~id:r.name
        ~detail:
          (sprintf
             "%s · %s"
             (P.Reminder.Trigger.describe r.trigger)
             (P.Reminder.first_line r))
        ~search:(r.name ^ " " ^ r.text)
        r.name)
  in
  open_picker m Reminders (Picker.create ~title:"Reminders" items)
;;

let nix_gc_still_running m =
  ( warn
      m
      "the Nix garbage collection you started is still running (see the status \
       line)"
  , [] )
;;

let start_nix_gc m generations =
  if Option.is_some m.nix_gc
  then nix_gc_still_running m
  else
    ( notice
        { m with
          nix_gc =
            Some { paths_deleted = 0; generations_deleted = 0; message = None }
        }
        (Nix_gc_command.starting generations)
    , [ call
          (Nix_store_gc { delete_generations = generations })
          ~tag:Nix_gc_done
      ] )
;;

let nix_gc_command m rest =
  match Nix_gc_command.parse rest with
  | Status ->
    ( notice m "reading the Nix store…"
    , [ call Nix_store_status ~tag:Nix_store_shown ] )
  | Error e ->
    error (restore_command m (String.strip ("/nix-gc " ^ rest))) e, []
  | Collect _ when Option.is_some m.nix_gc -> nix_gc_still_running m
  | Collect None -> start_nix_gc m None
  | Collect (Some generations as g) ->
    ( { m with
        mode =
          Confirm
            { question = Option.value_exn (Nix_gc_command.warning g)
            ; action = Nix_gc generations
            }
      ; autocomplete = None
      }
    , [] )
;;

let remind_action m ~command (action : Remind_command.Action.t) =
  match action with
  | Pick reminders -> reminder_picker m reminders, []
  | Info text -> notice m text, []
  | Error text -> error (restore_command m command) text, []
  | Add _ | Remove _ | Clear _ ->
    (match Remind_command.rpc action with
     | None -> m, []
     | Some (method_, params) ->
       let then_ : Reply_tag.t =
         match action with
         | Remove name -> Notice_on_success (Remind_command.removed name)
         | Clear n -> Notice_on_success (Remind_command.cleared n)
         | _ -> Reminder_added
       in
       ( { m with mode = Editing }
       , [ rpc
             method_
             ~params
             ~tag:(Retry_in_editor { command; hint = ""; then_ })
         ] ))
;;

(* ---- worktrees -------------------------------------------------------- *)

let worktree_key m = Location_cache.key m.state

let cache_worktrees m listing =
  { m with
    worktrees = Location_cache.put m.worktrees ~key:(worktree_key m) listing
  }
;;

let worktree_items (listing : P.Worktree.Listing.t) =
  List.map listing.worktrees ~f:(fun (w : P.Worktree.t) ->
    let label = P.Worktree.label w in
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
;;

let worktrees_title m (listing : P.Worktree.Listing.t) =
  sprintf "Worktrees of %s" (tilde m listing.root)
;;

let worktrees_picker m listing =
  open_picker
    (cache_worktrees m listing)
    (Worktrees listing)
    (Picker.create ~title:(worktrees_title m listing) (worktree_items listing))
;;

let enter_worktree ~notice path =
  call
    (Change_dir { target = Cwd; path; if_missing = Fail })
    ~tag:(Worktree_entered notice)
;;

let open_worktree m (w : P.Worktree.t) ~new_session =
  let m = { m with mode = Editing } in
  if w.prunable
  then
    ( error
        m
        (sprintf
           "the worktree %s is gone (%s was deleted); /worktree rm %s forgets \
            it"
           (P.Worktree.label w)
           w.path
           (Worktree_command.target w))
    , [] )
  else (
    let where = sprintf "the worktree %s at %s" (P.Worktree.label w) w.path in
    if new_session
    then
      ( notice m ("starting a session in " ^ where ^ "…")
      , [ rpc
            "new_session"
            ~tag:
              (Worktree_session_started
                 { path = w.path; notice = "new session in " ^ where })
        ] )
    else m, [ enter_worktree ~notice:("working in " ^ where) w.path ])
;;

let remove_command ~force target =
  sprintf "/worktree rm %s%s" (if force then "-f " else "") target
;;

let confirm_remove_worktree m (w : P.Worktree.t) ~force ~back =
  match Worktree_command.cannot_remove w with
  | Some reason -> error m reason, []
  | None ->
    ( { m with
        mode =
          Confirm
            { question = Worktree_command.confirm_remove w ~force ^ " (y/n)"
            ; action = Remove_worktree { worktree = w; force; back }
            }
      ; autocomplete = None
      }
    , [] )
;;

let worktree_command m rest =
  let command = String.strip ("/worktree " ^ rest) in
  match Worktree_command.parse rest with
  | Error text -> error (restore_command m command) text, []
  | List -> m, [ call (List_worktrees { cwd = None }) ~tag:Worktrees_picker ]
  | New { branch; base } ->
    ( notice m (sprintf "creating a worktree for %s…" branch)
    , [ call
          (Create_worktree { branch; base; cwd = None })
          ~tag:
            (Retry_in_editor { command; hint = ""; then_ = Worktree_created })
      ] )
  | (Open _ | Remove _) as action ->
    ( m
    , [ call
          (List_worktrees { cwd = None })
          ~tag:(Worktree_target { command; action })
      ] )
;;

let worktree_target m ~command (action : Worktree_command.Action.t) listing =
  let m = cache_worktrees m listing in
  let resolve target ~f =
    match Worktree_command.find listing target with
    | Error e -> error (restore_command m command) e, []
    | Ok w -> f w
  in
  match action with
  | List | New _ | Error _ -> m, []
  | Open { target; new_session } ->
    resolve target ~f:(open_worktree m ~new_session)
  | Remove { target; force } ->
    resolve target ~f:(confirm_remove_worktree m ~force ~back:None)
;;

(* One with uncommitted changes is kept unless forced: [rm -f] is. *)
let worktree_remove_failed m ~target ~force e =
  if (not force) && String.is_substring e ~substring:"force"
  then (
    let command = remove_command ~force:true target in
    ( error
        (restore_command m command)
        (sprintf "%s; %s removes it with its changes" e command)
    , [] ))
  else error (restore_command m (remove_command ~force target)) e, []
;;

(* [/review]: the files' approvals of a worktree, or the model's review. *)
let rec review_action m ~command (action : Review_command.Action.t) =
  match action with
  | Show { dir; base } ->
    ( notice m "reading the review…"
    , [ rpc "get_review" ~tag:(Review_comments { dir; base }) ] )
  | Named { name; base; model } ->
    ( m
    , [ call
          (List_worktrees { cwd = None })
          ~tag:(Review_named { command; name; base; model })
      ] )
  | Model args ->
    ( notice m "reading the changes to review…"
    , [ rpc
          "review"
          ~params:[ "args", str args ]
          ~tag:(Retry_in_editor { command; hint = ""; then_ = Review_started })
      ] )
  | Error e -> error (restore_command m command) e, []

and review_command m rest =
  review_action
    m
    ~command:(String.strip ("/review " ^ rest))
    (Review_command.parse rest)
;;

let cancel_btw m =
  match m.btw with
  | Some box when Btw_box.is_streaming box ->
    [ rpc "btw_cancel" ~params:[ "btw_id", str box.id ] ~tag:Ignore ]
  | _ -> []
;;

(* A newer question replaces (and cancels) the previous one. *)
let start_btw m question =
  let id = sprintf "btw-%d" (m.btw_seq + 1) in
  ( { m with btw = Some (Btw_box.create ~id ~question); btw_seq = m.btw_seq + 1 }
  , cancel_btw m
    @ [ rpc
          "btw"
          ~params:[ "question", str question; "btw_id", str id ]
          ~tag:(Btw id)
      ] )
;;

let dismiss_btw m = { m with btw = None }, cancel_btw m

let update_btw m id ~f =
  match m.btw with
  | Some box when String.equal box.id id -> { m with btw = Some (f box) }
  | _ -> m
;;

(* The command goes back into the editor as typed, so nothing is lost; the
   error names the closest command to fix it to. *)
let unknown_command m name ~retry =
  let m = { m with editor = Editor.set_text m.editor (retry name) } in
  match Commands.closest ~superuser:m.superuser name with
  | Some c ->
    error
      m
      (sprintf
         "unknown command /%s (back in the editor); did you mean /%s?"
         name
         c.name)
  | None ->
    error
      m
      (sprintf
         "unknown command /%s (back in the editor); Tab or / lists commands"
         name)
;;

let help_heading text : Content.Line.t =
  [ { text; style = Style.bold (Style.fg Gray) } ]
;;

let run_command m (cmd : Commands.Parsed.t) =
  match cmd.name, cmd.args with
  | "", _ -> m, []
  | "help", [] ->
    ( output
        m
        ~title:"Help"
        (List.concat
           [ help_heading "Commands" :: Commands.help ~superuser:m.superuser
           ; [ []; help_heading "In the prompt" ]
           ; Commands.input_help
           ; [ []; help_heading "Keys" ]
           ; Keymap.help
           ; [ []; help_heading "In lists" ]
           ; Keymap.list_help
           ])
    , [] )
  | "help", name :: _ ->
    (match Commands.find ~superuser:m.superuser name with
     | Some spec ->
       let usage = Commands.usage spec in
       ( output
           m
           ~title:("/" ^ spec.name)
           [ [ { Content.Span.text = usage; style = Style.bold Style.plain }
             ; { text = "  " ^ spec.help; style = Style.plain }
             ]
           ]
       , [] )
     | None -> unknown_command m name ~retry:(fun name -> "/help " ^ name), [])
  | "hotkeys", _ ->
    ( output
        m
        ~title:"Keys"
        ((help_heading "Keys" :: Keymap.help)
         @ ([] :: help_heading "In lists" :: Keymap.list_help))
    , [] )
  | "model", [] ->
    if List.is_empty m.models
    then m, [ rpc "list_models" ~tag:(Models_for_picker "") ]
    else model_picker m ~query:"", []
  | "model", _ ->
    if List.is_empty m.models
    then m, [ rpc "list_models" ~tag:(Models_for_switch cmd.rest) ]
    else switch_model m cmd.rest
  | "scoped-models", _ ->
    if List.is_empty m.models
    then m, [ rpc "list_models" ~tag:Models_for_scoped ]
    else scoped_models_picker m, []
  | "thinking", [] -> thinking_picker m, []
  | "thinking", level :: _ ->
    m, [ rpc "set_thinking" ~params:[ "thinking", str level ] ]
  | "verbosity", [] -> verbosity_picker m, []
  | "verbosity", name :: _ ->
    (match
       List.find Verbosity.all ~f:(fun v ->
         String.equal (Verbosity.name v) name)
     with
     | Some verbosity -> set_verbosity m verbosity, []
     | None ->
       ( error
           m
           (sprintf "unknown verbosity %S; use quiet, normal or verbose" name)
       , [] ))
  | "confirm", [] ->
    (match m.state with
     | Some s ->
       let mode = Session_mode.of_state s in
       ( notice
           m
           (sprintf
              "mode: %s — %s (/confirm on|off sets ask or auto; /mode has all)"
              (Session_mode.label mode)
              (Session_mode.describe mode))
       , [] )
     | None -> notice m "not connected", [])
  | "mode", [] -> mode_picker m
  | "mode", name :: _ -> mode_by_name m name
  | "plan", [] ->
    (match m.state with
     | None -> notice m "not connected", []
     | Some s ->
       set_mode m { (Session_mode.of_state s) with plan = not s.plan_mode })
  | "plan", (("on" | "off") as on) :: _ ->
    (match m.state with
     | None -> notice m "not connected", []
     | Some s ->
       set_mode m { (Session_mode.of_state s) with plan = String.equal on "on" })
  | "plan", other :: _ ->
    ( error
        m
        (sprintf
           "unknown argument %S; use on or off (or nothing to toggle)"
           other)
    , [] )
  | "todos", _ -> show_plan m
  | "permissions", _ -> show_permissions m
  | "confirm", ("on" | "true") :: _ -> set_confirm m true
  | "confirm", ("off" | "false") :: _ -> set_confirm m false
  | "confirm", other :: _ ->
    error m (sprintf "unknown argument %S; use on or off" other), []
  | "notify", [] -> notice m (notify_summary m), []
  | "notify", [ word ] when Option.is_some (Notify.Mode.of_string word) ->
    let mode = Option.value_exn (Notify.Mode.of_string word) in
    ( { m with
        config =
          Option.map m.config ~f:(fun config ->
            { config with P.Config.notify = word })
      }
    , [ set_config
          ~tag:(Notice_on_success (notify_summary_of mode m.terminal_focus))
          [ "notify", P.Json.str word ]
      ] )
  | "notify", _ -> error m "usage: /notify [off|bell|desktop]", []
  | "change_default", _ -> m, [ rpc "change_default" ~tag:Default_saved ]
  | "fallback", [] ->
    show_config m ~tag:Fallback_shown ~summary:(fallback_summary m)
  | "fallback", _ -> set_fallback m cmd
  | "default-dir", [] ->
    show_config m ~tag:Default_dir_shown ~summary:default_dir_summary
  | "default-dir", _ -> set_default_dir m cmd
  | "auth", _ ->
    ( m
    , [ rpc "auth_status" ~tag:Auth_show; call Forge_status ~tag:Forge_refresh ]
    )
  | "budget", [] -> with_state m ~f:Loop_status.describe_budget
  | "budget", _ ->
    (match Loop_status.parse_budget cmd.rest with
     | Error e -> error m e, []
     | Ok usd ->
       ( m
       , [ rpc
             "set_budget"
             ~params:
               [ "usd", Option.value_map usd ~default:`Null ~f:P.Json.float ]
             ~tag:
               (Notice_on_success
                  (match usd with
                   | None -> "budget removed"
                   | Some usd -> sprintf "budget set to $%.2f" usd))
         ] ))
  | "goal", [] -> with_state m ~f:Loop_status.describe_goal
  | "goal", [ "off" ] ->
    m, [ rpc "clear_goal" ~tag:(Notice_on_success "goal cleared") ]
  | "goal", [ "resume" ] ->
    m, [ rpc "resume_goal" ~tag:(Notice_on_success "goal resumed") ]
  | "goal", _ ->
    ( m
    , [ rpc
          "set_goal"
          ~params:[ "text", str cmd.rest ]
          ~tag:
            (Retry_in_editor
               { command = "/goal " ^ cmd.rest
               ; hint = ""
               ; then_ =
                   Notice_on_success "goal set: the agent works until it is met"
               })
      ] )
  | "resume", [] ->
    with_state m ~f:(Loop_status.describe_resume ~utc_offset:m.utc_offset)
  | "resume", [ "now" ] ->
    m, [ rpc "resume_now" ~tag:(Notice_on_success "resuming now") ]
  | "resume", [ "cancel" ] ->
    ( m
    , [ rpc
          "cancel_resume"
          ~tag:(Notice_on_success "automatic resume cancelled")
      ] )
  | "resume", _ -> error m "usage: /resume [now|cancel]", []
  | "login", [] ->
    ( m
    , [ rpc "auth_status" ~tag:Auth_login_picker
      ; call Forge_status ~tag:Forge_refresh
      ] )
  | "login", provider :: rest ->
    (match Forge_accounts.Login.parse m.forge cmd.args with
     | Some (Ok login) -> forge_login m login
     | Some (Error e) -> error (restore_command m ("/login " ^ cmd.rest)) e, []
     | None ->
       let params =
         ("provider", str provider)
         :: Option.to_list
              (Option.map (List.hd rest) ~f:(fun meth -> "method", str meth))
       in
       m, [ rpc "login" ~params ])
  | "logout", [] ->
    ( m
    , [ rpc "auth_status" ~tag:Auth_logout_picker
      ; call Forge_status ~tag:Forge_refresh
      ] )
  | "logout", provider :: _ ->
    (match Forge_accounts.logout_host m.forge provider with
     | Some host -> forge_logout m host
     | None -> logout m provider)
  | "compact", _ ->
    ( notice m "compacting…"
    , [ rpc
          "compact"
          ~params:
            (if String.is_empty cmd.rest
             then []
             else [ "instructions", str cmd.rest ])
          ~tag:Compact_done
      ] )
  | "pr", _ -> pr_command m cmd.rest
  | "diff", _ ->
    ( notice m "reading the changes…"
    , [ rpc
          "git_diff"
          ~params:
            (if String.is_empty cmd.rest then [] else [ "path", str cmd.rest ])
          ~tag:Git_diff
      ] )
  | "review", _ -> review_command m cmd.rest
  | "init", _ -> m, [ rpc "init" ~tag:Review_started ]
  | "new", _ ->
    m, [ rpc "new_session" ~tag:(Reload_messages_notice "new session") ]
  | "name", [] ->
    ( { m with
        mode = Text_prompt { question = "Session name"; action = Name }
      ; editor = Editor.clear m.editor
      }
    , [] )
  | "name", _ ->
    ( m
    , [ rpc
          "set_session_name"
          ~params:[ "name", str cmd.rest ]
          ~tag:(Notice_on_success "session named")
      ] )
  | "session", _ -> m, [ rpc "session_stats" ~tag:Session_stats ]
  | "agents", _ -> agents_command m cmd.rest
  | "jobs", [ "kill"; id ] -> kill_job m id
  | "jobs", "kill" :: _ -> error m "usage: /jobs kill <id>", []
  | "jobs", [] -> m, [ rpc "list_jobs" ~tag:Jobs_picker ]
  | "queue", _ -> queue_picker m
  | "jobs", [ id ] -> job_output m id
  | "host", [] -> hosts_picker m
  | "host", _ -> switch_host m cmd.rest
  | "sessions", _ | "switch", [] | "delete", [] ->
    m, [ rpc "list_sessions" ~tag:Sessions_picker ]
  | "delete", args -> m, [ rpc "list_sessions" ~tag:(Sessions_to_delete args) ]
  | "delete-archived", _ -> m, [ rpc "list_sessions" ~tag:Archived_to_delete ]
  | (("archive" | "unarchive") as verb), _ ->
    archive_command m ~archived:(String.equal verb "archive") cmd.rest
  | "switch", _ ->
    ( m
    , [ rpc
          "switch_session"
          ~params:[ "path", str cmd.rest ]
          ~tag:
            (Retry_in_editor
               { command = "/switch " ^ cmd.rest
               ; hint = "/sessions lists them, or Tab after /switch completes"
               ; then_ = Reload_messages
               })
      ] )
  | "cd", [] ->
    ( { m with
        mode = Text_prompt { question = "Change directory to"; action = Cd }
      ; editor = Editor.clear m.editor
      }
    , [] )
  | "cd", _ ->
    ( m
    , [ call
          (Change_dir { target = Cwd; path = cmd.rest; if_missing = Ask })
          ~tag:
            (Retry_in_editor
               { command = "/cd " ^ cmd.rest
               ; hint = "Tab after /cd completes directories"
               ; then_ = Dir_changed Cwd
               })
      ] )
  | "mkdir", [] ->
    ( { m with
        mode =
          Text_prompt
            { question = "Create and change to directory"; action = Mkdir }
      ; editor = Editor.clear m.editor
      }
    , [] )
  | "mkdir", _ ->
    ( m
    , [ call
          (Change_dir { target = Cwd; path = cmd.rest; if_missing = Create })
          ~tag:
            (Retry_in_editor
               { command = "/mkdir " ^ cmd.rest
               ; hint = ""
               ; then_ = Dir_changed Cwd
               })
      ] )
  | "fork", _ -> m, [ rpc "get_entries" ~tag:Entries_for_fork ]
  | "rewind", _ -> m, [ rpc "get_entries" ~tag:Entries_for_rewind ]
  | "tree", _ ->
    ( m
    , [ rpc
          "get_entries"
          ~params:[ "all", P.Json.bool true ]
          ~tag:Entries_for_tree
      ] )
  | "clone", _ ->
    m, [ rpc "clone" ~tag:(Reload_messages_notice "cloned session") ]
  | "export", [] ->
    ( { m with
        mode = Text_prompt { question = "Export to"; action = Export_path }
      ; editor = Editor.clear m.editor
      }
    , [] )
  | "export", _ -> m, [ export_command cmd.rest ]
  | "import", [] ->
    ( { m with
        mode = Text_prompt { question = "Import from"; action = Import_path }
      ; editor = Editor.clear m.editor
      }
    , [] )
  | "import", _ ->
    m, [ rpc "import" ~params:[ "path", str cmd.rest ] ~tag:Reload_messages ]
  | "abort", _ -> m, [ rpc "abort" ]
  | "skills", _ -> open_skills m
  | "snippet", _ ->
    snippet_action
      m
      (Snippet_command.run
         ~snippets:(snippets m)
         ~last_prompt:(last_prompt m)
         cmd.rest)
  | "worktree", _ -> worktree_command m cmd.rest
  | "remind", _ ->
    remind_action
      m
      ~command:(String.strip ("/remind " ^ cmd.rest))
      (Remind_command.run ~reminders:(reminders m) cmd.rest)
  | "mcp", [] -> m, [ call (List_mcp { reconnect = false }) ~tag:Mcp_picker ]
  | "mcp", [ "reconnect" ] ->
    ( notice m "reconnecting MCP servers…"
    , [ call (List_mcp { reconnect = true }) ~tag:Mcp_reconnected ] )
  | "mcp", _ -> error m "usage: /mcp [reconnect]", []
  | "btw", [] -> error m "usage: /btw <question>", []
  | "btw", _ -> start_btw m cmd.rest
  | "retry-backend-connection", _ -> retry_backend_connection m
  | "state", _ ->
    let text =
      Option.value_map m.state ~default:"not connected" ~f:(fun s ->
        Sexp.to_string_hum (P.State.sexp_of_t s))
    in
    output m ~title:"State" (Content.lines ~style:(Style.fg Gray) text), []
  | "clear", _ ->
    follow { m with transcript = Transcript.clear m.transcript }, []
  | "connect", _ -> connect m cmd.rest
  | "signin", args -> sign_in m args
  | "signout", _ -> sign_out m
  | "nix-gc", _ when m.superuser -> nix_gc_command m cmd.rest
  | "setusr", [] -> m, [ rpc "list_users" ~tag:Users_list ]
  | "setusr", [ user ] ->
    m, [ rpc "set_user" ~params:[ "user", str user ] ~tag:User_switched ]
  | "setusr", _ -> error m "usage: /setusr [user]", []
  | "quit", _ | "exit", _ -> { m with quitting = true }, [ Quit ]
  | name, _ ->
    ( unknown_command m name ~retry:(fun name ->
        String.strip (sprintf "/%s %s" name cmd.rest))
    , [] )
;;

(* ---- editing mode ----------------------------------------------------- *)

let attachments m text =
  String.split text ~on:'\n'
  |> List.concat_map ~f:(String.split ~on:' ')
  |> List.filter_map ~f:(fun token ->
    match String.chop_prefix token ~prefix:"@" with
    | Some path when (not (String.is_empty path)) && Set.mem m.known_paths path
      -> Some path
    | _ -> None)
  |> List.dedup_and_sort ~compare:String.compare
;;

let user_params m text =
  let attachments = attachments m text in
  ("text", str text)
  ::
  (if List.is_empty attachments
   then []
   else
     [ "attachments", `Array (List.map attachments ~f:(fun p -> P.Json.str p)) ])
;;

let shell_command m text =
  if String.is_prefix text ~prefix:"!&"
  then (
    let command = String.drop_prefix text 2 |> String.strip in
    if String.is_empty command
    then error m "usage: !&command (runs it as a background job)", []
    else
      ( m
      , [ rpc
            "shell"
            ~params:[ "command", str command; "background", P.Json.bool true ]
            ~tag:Job_started
        ] ))
  else if Model.running m
  then
    ( warn
        { m with editor = Editor.set_text m.editor text }
        (sprintf
           "%s waits for the turn to end (kept in the editor); !&%s runs it in \
            the background now"
           text
           (String.lstrip (String.lstrip ~drop:(Char.equal '!') text)))
    , [] )
  else (
    let add_to_context = not (String.is_prefix text ~prefix:"!!") in
    let command =
      String.drop_prefix text (if add_to_context then 1 else 2) |> String.strip
    in
    if String.is_empty command
    then error m "usage: !command (!! for without context)", []
    else
      ( m
      , [ rpc
            "shell"
            ~params:
              [ "command", str command
              ; "add_to_context", P.Json.bool add_to_context
              ]
        ] ))
;;

(* [/skill:NAME ARGS] is a prompt that the backend expands; a failure (an
   unknown name) puts the text back. *)
let skill_invocation text =
  match Commands.parse text with
  | Some { name; _ } -> String.is_prefix name ~prefix:"skill:"
  | None -> false
;;

let prompt_tag text : Reply_tag.t =
  if skill_invocation text then Skill_prompt text else Show_error
;;

(* Shown at once; the backend's [queue_update] replaces it. *)
let shown_queued m (kind : Queued_message.Kind.t) text =
  let index =
    List.count m.queued_messages ~f:(fun q ->
      Queued_message.Kind.equal q.kind kind)
  in
  m.queued_messages @ [ { Queued_message.kind; index; text } ]
;;

let send_prompt m text =
  let m, cmds =
    if String.is_prefix text ~prefix:"!"
    then shell_command m text
    else if Model.running m
    then
      ( { m with queued_messages = shown_queued m Steer text }
      , [ rpc "steer" ~params:(user_params m text) ~tag:(prompt_tag text) ] )
    else
      ( prune_agents m
      , [ rpc "prompt" ~params:(user_params m text) ~tag:(prompt_tag text) ] )
  in
  m, cmds @ [ Command.Append_history text; push_history text ]
;;

let submit ?(kind = Queued_message.Kind.Steer) m =
  let text, editor = Editor.submit m.editor in
  let m = follow { m with editor; autocomplete = None } in
  if String.is_empty (String.strip text)
  then m, []
  else (
    match Commands.parse text, focused_agent m with
    | Some { name = "skill:"; _ }, _ -> open_skills m
    | Some { name; _ }, _ when String.is_prefix name ~prefix:"skill:" ->
      send_prompt m (String.strip text)
    | Some cmd, _ -> run_command m cmd
    | None, Some a when not (String.is_prefix text ~prefix:"!") ->
      message_agent
        m
        a
        kind
        text
        ~retry:text
        ~finished_hint:"tell the main agent instead (Esc goes back to it)"
        ~ack:false
    | None, _ -> send_prompt m text)
;;

let queue_follow_up m =
  if Option.is_some (focused_agent m)
  then submit ~kind:Follow_up m
  else if not (Model.running m)
  then submit m
  else (
    let text, editor = Editor.submit m.editor in
    if String.is_empty (String.strip text)
    then m, []
    else (
      let m = follow { m with editor; autocomplete = None } in
      let m = { m with queued_messages = shown_queued m Follow_up text } in
      ( m
      , [ rpc "follow_up" ~params:(user_params m text) ~tag:(prompt_tag text)
        ; Command.Append_history text
        ; push_history text
        ] )))
;;

let last_assistant_text m =
  let transcript =
    match m.focus with
    | `Main -> m.transcript
    | `Agent id ->
      (match Agent_view.find m.agents id with
       | Some a -> a.transcript
       | None -> m.transcript)
  in
  Transcript.items transcript
  |> List.rev
  |> List.find_map ~f:(function
    | Transcript.Item.Assistant { text; _ } -> Some text
    | _ -> None)
;;

let copy_last m =
  match last_assistant_text m with
  | Some text when not (String.is_empty (String.strip text)) ->
    ( notice m (sprintf "copied %d chars" (String.length text))
    , [ Command.Copy_to_clipboard text ] )
  | _ -> notice m "nothing to copy", []
;;

let current_line m =
  let pos = Editor.position m.editor in
  let line =
    Option.value (List.nth (Editor.lines m.editor) pos.line) ~default:""
  in
  let before =
    String.concat (List.take (List.map (Text_width.uchars line) ~f:fst) pos.col)
  in
  line, String.length before
;;

(* Path and directory completions come from the backend (from the tool host, so
   they reflect the machine tools run on); the same prefix is not asked for
   twice. *)
let fetch_autocomplete m ac =
  match Autocomplete.source ac with
  | (Autocomplete.Source.Path | Directory _) as source ->
    let same =
      match m.autocomplete with
      | Some prev ->
        Autocomplete.Source.equal (Autocomplete.source prev) source
        && String.equal (Autocomplete.prefix prev) (Autocomplete.prefix ac)
      | None -> false
    in
    if same
    then m, []
    else (
      let prefix = Autocomplete.prefix ac in
      let request =
        match source with
        | Directory { host } ->
          rpc
            "list_dirs"
            ~params:
              (("prefix", `String prefix)
               :: Option.value_map host ~default:[] ~f:(fun h ->
                 [ "host", `String h ]))
            ~tag:(Dirs_for_autocomplete prefix)
        | _ ->
          rpc
            "list_paths"
            ~params:[ "prefix", `String prefix ]
            ~tag:(Paths_for_autocomplete prefix)
      in
      { m with autocomplete = Some ac }, [ request ])
  | Autocomplete.Source.Skill ->
    let key = Location_cache.key m.state in
    (match Location_cache.request m.skills ~key with
     | None -> { m with autocomplete = Some ac }, []
     | Some skills ->
       ( { m with autocomplete = Some ac; skills }
       , [ call List_skills ~tag:(Skills_for_autocomplete key) ] ))
  | Autocomplete.Source.Argument { argument = Some (Worktree | Review); _ } ->
    let key = worktree_key m in
    (match Location_cache.request m.worktrees ~key with
     | None -> { m with autocomplete = Some ac }, []
     | Some worktrees ->
       ( { m with autocomplete = Some ac; worktrees }
       , [ call
             (List_worktrees { cwd = None })
             ~tag:(Worktrees_for_autocomplete key)
         ] ))
  | Autocomplete.Source.Argument spec
    when match spec.argument with
         | Some (Commands.Argument.Sessions | Session_list) ->
           Option.is_none m.sessions
         | _ -> false ->
    ( { m with autocomplete = Some ac }
    , [ rpc "list_sessions" ~tag:Sessions_cache ] )
  | _ -> { m with autocomplete = Some ac }, []
;;

(* [default_cwd] is a directory on the backend's host, unless the backend runs
   no tools ([-no-backend-host]): then it is used on whichever host runs them. *)
let default_dir_host m =
  Option.bind m.state ~f:(fun (s : P.State.t) ->
    Option.some_if
      (List.exists s.hosts ~f:(fun h -> String.equal h.id P.Host.backend_id))
      P.Host.backend_id)
;;

let refresh_command_autocomplete m =
  let line, col = current_line m in
  let line_index = (Editor.position m.editor).line in
  match
    Autocomplete.compute
      ~superuser:m.superuser
      ~current_session:(current_session m)
      ~line
      ~col
      ~line_index
      ~models:m.models
      ~auth:m.auth
      ~forge:m.forge
      ~sessions:m.sessions
      ~skills:(Location_cache.find m.skills ~key:(Location_cache.key m.state))
      ~snippets:(Option.value (snippets m) ~default:[])
      ~reminders:(Option.value (reminders m) ~default:[])
      ~agents:m.agents
      ~worktrees:
        (Option.map
           (Location_cache.find m.worktrees ~key:(worktree_key m))
           ~f:(fun (l : P.Worktree.Listing.t) -> l.worktrees))
      ~logged_in:(logged_in m)
      ~default_dir_host:(default_dir_host m)
      ~server:m.server
  with
  | None -> { m with autocomplete = None }, []
  | Some ac -> fetch_autocomplete m ac
;;

(* A dialog's text (a directory prompt prefilled with [/work], say) is not a
   command line: its completion, if any, is its own. *)
let refresh_autocomplete m =
  if Mode.is_dialog m.mode then m, [] else refresh_command_autocomplete m
;;

(* The directory prompts complete on the host the answer is for. *)
let refresh_directory_autocomplete m ~host =
  let text = Editor.text m.editor in
  match host with
  | None -> { m with autocomplete = None }, []
  | Some host -> fetch_autocomplete m (Autocomplete.directory ~host ~text)
;;

let path_complete m =
  let pos = Editor.position m.editor in
  let line =
    Option.value (List.nth (Editor.lines m.editor) pos.line) ~default:""
  in
  let start = Editor.word_start m.editor in
  let word =
    let pieces = List.map (Text_width.uchars line) ~f:fst in
    String.concat (List.take (List.drop pieces start.col) (pos.col - start.col))
  in
  let editor =
    if String.is_prefix word ~prefix:"@"
    then m.editor
    else Editor.insert (Editor.goto m.editor start) "@"
  in
  refresh_autocomplete { m with editor }
;;

let accept_autocomplete m ~submit_now =
  match m.autocomplete with
  | None -> None
  | Some ac ->
    (match Autocomplete.selected_item ac with
     | None -> None
     | Some item ->
       let editor =
         match Autocomplete.source ac with
         | Snippet ->
           (match
              P.Snippet.find (Option.value (snippets m) ~default:[]) item.id
            with
            | Some snippet -> Autocomplete.insert_snippet ac m.editor snippet
            | None -> m.editor)
         | _ ->
           Editor.set_text
             m.editor
             (Autocomplete.accept ac ~editor_text:(Editor.text m.editor))
       in
       let m = { m with editor; autocomplete = None } in
       let m, cmds =
         match Autocomplete.source ac, submit_now with
         | Autocomplete.Source.Snippet, _ -> m, []
         | Autocomplete.Source.Command, true ->
           (match Commands.find ~superuser:m.superuser item.id with
            (* [/review] and [/agents] alone are the common case: they run
               at once. *)
            | Some { argument = Some (Review | Agents); _ } -> submit m
            | Some spec when Option.is_some spec.argument ->
              refresh_autocomplete m
            | _ -> submit m)
         | Autocomplete.Source.Command, false
           when String.is_suffix item.id ~suffix:":" -> refresh_autocomplete m
         | Autocomplete.Source.Command, false -> m, []
         | (Autocomplete.Source.Argument _ | Path | Directory _ | Skill), true
           -> submit m
         | (Autocomplete.Source.Argument _ | Path | Directory _ | Skill), false
           -> m, []
       in
       Some (m, cmds))
;;

(* Denies pending tool confirmations and aborts the running turn. *)
let abort m =
  let denies =
    List.map m.pending_confirms ~f:(fun c ->
      rpc
        "tool_confirm_respond"
        ~params:(Confirm_prompt.params c Deny ~reason:None)
        ~tag:Ignore)
  in
  (* The backend withdraws the questions as the run stops. *)
  ( { m with pending_confirms = []; pending_questions = [] }
  , denies @ [ rpc "abort" ~tag:Abort_done ] )
;;

(* Ctrl+C: clear the editor; else abort the running turn; else quit on the
   second press. *)
let interrupt m =
  if Model.backend_gone m || m.pending_quit
  then { m with quitting = true }, [ Command.Quit ]
  else if not (Editor.is_empty m.editor)
  then { m with editor = Editor.clear m.editor; pending_quit = false }, []
  else if Model.running m
  then (
    let m, cmds = abort { m with pending_quit = true } in
    warn m "aborting; Ctrl+C again quits", cmds)
  else { m with pending_quit = true }, []
;;

let page_size m = Int.max 1 (m.height / 2)

(* Ten rows, fewer when the transcript area is short: the title and the
   filter take two. *)
let picker_rows m = Int.max 1 (Int.min 10 ((layout m).main_rows - 2))
let wheel_lines = 3

let scroll_up m ~lines =
  match m.viewport with
  | Viewport.Follow ->
    { m with
      viewport =
        Viewport.Anchored
          { top = Int.max 0 (transcript_line_count m - transcript_rows m - lines)
          ; new_lines = 0
          }
    }
  | Viewport.Anchored { top; new_lines } ->
    { m with
      viewport = Viewport.Anchored { top = Int.max 0 (top - lines); new_lines }
    }
;;

let scroll_down m ~lines =
  match m.viewport with
  | Viewport.Follow -> m
  | Viewport.Anchored { top; new_lines } ->
    let top = top + lines in
    if top + transcript_rows m >= transcript_line_count m
    then follow m
    else { m with viewport = Viewport.Anchored { top; new_lines } }
;;

let focused_transcript m =
  match m.focus with
  | `Main -> m.transcript
  | `Agent id ->
    (match Agent_view.find m.agents id with
     | Some agent -> agent.transcript
     | None -> m.transcript)
;;

let focused_lines m =
  Transcript.render_all
    (focused_transcript m)
    ~width:(transcript_width m)
    ~verbosity:m.verbosity
;;

let matches_of (lines : Content.t) query =
  if String.is_empty query
  then []
  else (
    let needle = String.lowercase query in
    List.filter_mapi lines ~f:(fun i line ->
      Option.some_if
        (String.is_substring
           ~substring:needle
           (String.lowercase (Content.Line.to_plain line)))
        i))
;;

let search_top_for m line = Int.max 0 (line - (transcript_rows m / 2))

let search_view m matches current =
  match List.nth matches current with
  | None -> m
  | Some line ->
    { m with
      viewport =
        Viewport.Anchored { top = search_top_for m line; new_lines = 0 }
    }
;;

let open_search m =
  { m with
    mode = Search { query = ""; matches = []; current = 0 }
  ; autocomplete = None
  }
;;

let search m (intent : Intent.t) =
  match m.mode with
  | Search { query; matches; current } ->
    let recompute q =
      let matches = matches_of (focused_lines m) q in
      let m = { m with mode = Search { query = q; matches; current = 0 } } in
      if List.is_empty matches then m else search_view m matches 0
    in
    let move step =
      if List.is_empty matches
      then m, []
      else (
        let count = List.length matches in
        let current = (((current + step) mod count) + count) mod count in
        let m = { m with mode = Search { query; matches; current } } in
        search_view m matches current, [])
    in
    (match intent with
     | Cancel | Interrupt -> { m with mode = Editing }, []
     | Force_quit -> { m with quitting = true }, [ Command.Quit ]
     | Down | Submit -> move 1
     | Up -> move (-1)
     | Insert s -> recompute (query ^ s), []
     | Backspace -> recompute (String.drop_suffix query 1), []
     | _ -> m, [])
  | _ -> m, []
;;

let current_top m =
  match m.viewport with
  | Viewport.Follow -> Int.max 0 (transcript_line_count m - transcript_rows m)
  | Viewport.Anchored { top; _ } -> top
;;

let jump_user_message m ~direction =
  let lines =
    Transcript.user_message_lines
      (focused_transcript m)
      ~width:(transcript_width m)
      ~verbosity:m.verbosity
  in
  let top = current_top m in
  match direction with
  | `Prev ->
    (match List.last (List.filter lines ~f:(fun line -> line < top)) with
     | Some line ->
       { m with viewport = Viewport.Anchored { top = line; new_lines = 0 } }, []
     | None -> warn m "no earlier message", [])
  | `Next ->
    (match List.find lines ~f:(fun line -> line > top) with
     | Some line ->
       { m with viewport = Viewport.Anchored { top = line; new_lines = 0 } }, []
     | None -> follow m, [])
;;

let editing_intent m (intent : Intent.t) =
  let ed f = { m with editor = f m.editor }, [] in
  match intent with
  | Insert s -> ed (fun e -> Editor.insert e s)
  | Paste s -> ed (fun e -> Editor.insert_paste e s)
  | Submit -> submit m
  | Submit_as_typed -> submit { m with autocomplete = None }
  | Newline -> ed Editor.newline
  | Backspace -> ed Editor.backspace
  | Delete -> ed Editor.delete
  | Left -> ed Editor.left
  | Right -> ed Editor.right
  | Word_left -> ed Editor.word_left
  | Word_right -> ed Editor.word_right
  | Delete_word_forward -> ed Editor.delete_word_forward
  | Yank -> ed Editor.yank
  | Yank_pop -> ed Editor.yank_pop
  | Undo -> ed Editor.undo
  | Home ->
    (match m.mode, Editor.is_empty m.editor with
     | Editing, true ->
       { m with viewport = Viewport.Anchored { top = 0; new_lines = 0 } }, []
     | _ -> ed Editor.home)
  | End ->
    (match m.mode, Editor.is_empty m.editor with
     | Editing, true -> follow m, []
     | _ -> ed Editor.end_)
  | Up ->
    ed (fun e ->
      match Editor.up e with
      | Some e -> e
      | None -> Option.value (Editor.history_prev e) ~default:e)
  | Down ->
    ed (fun e ->
      match Editor.down e with
      | Some e -> e
      | None -> Option.value (Editor.history_next e) ~default:e)
  | Page_up -> scroll_up m ~lines:(page_size m), []
  | Page_down -> scroll_down m ~lines:(page_size m), []
  | Scroll_up -> scroll_up m ~lines:wheel_lines, []
  | Scroll_down -> scroll_down m ~lines:wheel_lines, []
  | Complete -> m, []
  | Cancel ->
    (match m.focus with
     | `Agent _ -> set_focus m `Main, []
     | `Main ->
       if Model.running m
       then abort m
       else (
         match m.viewport with
         | Viewport.Anchored _ -> follow m, []
         | Viewport.Follow -> { m with notice = None }, []))
  | Interrupt -> interrupt m
  | Force_quit -> { m with quitting = true }, [ Quit ]
  | Kill_to_end -> ed Editor.kill_to_end
  | Kill_to_start -> ed Editor.kill_to_start
  | Kill_word -> ed Editor.kill_word
  | Cycle_verbosity -> set_verbosity m (Verbosity.next m.verbosity), []
  | Next_model -> cycle_model m ~step:1
  | Prev_model -> cycle_model m ~step:(-1)
  | Next_thinking -> cycle_thinking m
  | Cycle_mode -> cycle_mode m
  | Next_agent -> cycle_focus m, []
  | Focus_agent n -> focus_agent m n, []
  | Queue_follow_up -> queue_follow_up m
  | Dequeue ->
    (match m.focus with
     | `Main -> m, [ rpc "dequeue" ~tag:Dequeued ]
     | `Agent agent_id ->
       ( m
       , [ rpc
             "dequeue_agent"
             ~params:[ "agent_id", str agent_id ]
             ~tag:Dequeued
         ] ))
  | Copy_last -> copy_last m
  | Suspend -> m, [ Command.Suspend ]
  | Path_complete -> path_complete m
  | Edit_externally -> m, [ Command.Edit_externally (Editor.text m.editor) ]
  | Model_picker ->
    if List.is_empty m.models
    then m, [ rpc "list_models" ~tag:(Models_for_picker "") ]
    else model_picker m ~query:"", []
  | Snippet_picker -> snippet_action m (Snippet_command.pick (snippets m))
  | Picker_toggle_filter -> m, []
  | Search -> open_search m, []
  | Prev_user_message -> jump_user_message m ~direction:`Prev
  | Next_user_message -> jump_user_message m ~direction:`Next
  | Toggle_archived | Toggle_sidebar | Review_highlighted -> m, []
;;

(* Autocomplete is a sub-state of editing: while it is open it owns a few keys,
   everything else edits the buffer and then recomputes the completion. *)
let editing m (intent : Intent.t) =
  match intent with
  | Cancel when Option.is_some m.btw -> dismiss_btw m
  | Next_agent -> cycle_focus m, []
  | Focus_agent n -> focus_agent m n, []
  | Search -> open_search m, []
  | _ ->
    (match m.autocomplete with
     | Some _ ->
       (match intent with
        | Complete ->
          Option.value (accept_autocomplete m ~submit_now:false) ~default:(m, [])
        | Submit ->
          (match m.autocomplete with
           | Some ac when not (Autocomplete.accepts_on_enter ac) ->
             submit { m with autocomplete = None }
           | _ ->
             Option.value
               (accept_autocomplete m ~submit_now:true)
               ~default:(submit m))
        | Submit_as_typed -> submit { m with autocomplete = None }
        | Up ->
          ( { m with
              autocomplete = Option.map m.autocomplete ~f:Autocomplete.up
            }
          , [] )
        | Down ->
          ( { m with
              autocomplete = Option.map m.autocomplete ~f:Autocomplete.down
            }
          , [] )
        | Cancel -> { m with autocomplete = None }, []
        | Insert _
        | Paste _
        | Newline
        | Backspace
        | Delete
        | Left
        | Right
        | Word_left
        | Word_right
        | Delete_word_forward
        | Home
        | End
        | Page_up
        | Page_down
        | Scroll_up
        | Scroll_down
        | Interrupt
        | Force_quit
        | Kill_to_end
        | Kill_to_start
        | Kill_word
        | Yank
        | Yank_pop
        | Undo
        | Cycle_verbosity
        | Next_model
        | Prev_model
        | Next_thinking
        | Cycle_mode
        | Next_agent
        | Focus_agent _
        | Queue_follow_up
        | Dequeue
        | Copy_last
        | Suspend
        | Path_complete
        | Edit_externally
        | Model_picker
        | Snippet_picker
        | Picker_toggle_filter
        | Prev_user_message
        | Next_user_message
        | Toggle_archived
        | Review_highlighted
        | Toggle_sidebar
        | Search ->
          let m, cmds = editing_intent m intent in
          let m, more = refresh_autocomplete m in
          m, cmds @ more)
     | None ->
       (match intent with
        | Complete when Editor.is_empty m.editor ->
          refresh_autocomplete { m with editor = Editor.insert m.editor "/" }
        | Complete -> refresh_autocomplete m
        | _ ->
          let m, cmds = editing_intent m intent in
          let m, more = refresh_autocomplete m in
          m, cmds @ more))
;;

(* ---- picker mode ------------------------------------------------------ *)

let picker_selected m (kind : Mode.Picker_kind.t) (item : Picker.Item.t) =
  let m = { m with mode = Editing } in
  match kind with
  | Models _ -> m, [ set_model_command item.id ]
  | Scoped_models -> m, []
  | Thinking -> m, [ rpc "set_thinking" ~params:[ "thinking", str item.id ] ]
  | Modes -> mode_by_name m item.id
  | Verbosity ->
    let verbosity =
      Option.value
        (List.find Verbosity.all ~f:(fun v ->
           String.equal (Verbosity.name v) item.id))
        ~default:Verbosity.Normal
    in
    set_verbosity m verbosity, []
  | Login ->
    (match
       ( Forge_accounts.login_of_item_id m.forge item.id
       , String.split item.id ~on:' ' )
     with
     | Some login, _ -> forge_login m login
     | None, [ provider; meth ] ->
       ( m
       , [ rpc "login" ~params:[ "provider", str provider; "method", str meth ]
         ] )
     | None, _ -> error m "bad login selection", [])
  | Logout ->
    (match Forge_accounts.logout_of_item_id item.id with
     | Some host -> forge_logout m host
     | None -> logout m item.id)
  | Pull_requests -> pr_command m item.id
  | Sessions _ ->
    ( m
    , [ rpc
          "switch_session"
          ~params:[ "path", str item.id ]
          ~tag:Reload_messages
      ] )
  | Fork entries ->
    (match
       List.find entries ~f:(fun (entry : P.Entry.t) ->
         String.equal entry.id item.id)
     with
     | Some { kind = P.Entry.Kind.Message (P.Message.User user); _ } ->
       ( { m with editor = Editor.set_text m.editor (Injected_note.typed user) }
       , [ rpc "fork" ~params:[ "at", str item.id ] ~tag:Reload_messages ] )
     | _ -> m, [])
  | Rewind entries ->
    (match
       List.find entries ~f:(fun (entry : P.Entry.t) ->
         String.equal entry.id item.id)
     with
     | Some { kind = P.Entry.Kind.Message message; _ } ->
       ( { m with
           mode =
             Confirm
               { question =
                   sprintf
                     "Rewind to %S? Later messages are abandoned (y/n)"
                     (message_first_line message)
               ; action = Rewind item.id
               }
         }
       , [] )
     | _ -> m, [])
  | Tree _ ->
    m, [ rpc "rewind" ~params:[ "to", str item.id ] ~tag:Reload_messages ]
  | Agents -> set_focus m (`Agent item.id), []
  | Jobs -> job_output m item.id
  | Queue -> unqueue m item.id ~edit:true
  | Hosts ->
    (match
       Option.bind m.state ~f:(fun s ->
         List.find s.hosts ~f:(fun h -> String.equal h.id item.id))
     with
     | Some host -> host_cwd_prompt m host, []
     | None -> error m (sprintf "unknown host %S" item.id), [])
  | Skills ->
    { m with editor = Editor.set_text m.editor ("/skill:" ^ item.id ^ " ") }, []
  | Snippets ->
    (match P.Snippet.find (Option.value (snippets m) ~default:[]) item.id with
     | Some snippet -> insert_snippet m snippet, []
     | None ->
       ( error
           m
           (sprintf "snippet %S is gone; /snippet lists the others" item.id)
       , [] ))
  | Reminders ->
    (match P.Reminder.find (Option.value (reminders m) ~default:[]) item.id with
     | Some r ->
       ( notice
           m
           (sprintf
              "%s (%s): %s · /remind rm %s removes it"
              r.name
              (P.Reminder.Trigger.describe r.trigger)
              r.text
              r.name)
       , [] )
     | None ->
       ( error
           m
           (sprintf "reminder %s is gone; /remind lists the others" item.id)
       , [] ))
  | Worktrees listing ->
    (match
       List.find listing.worktrees ~f:(fun w -> String.equal w.path item.id)
     with
     | Some w -> open_worktree m w ~new_session:false
     | None -> m, [])
  | Mcp list -> mcp_selected m list item.id
  | Auth_select id ->
    m, [ rpc "auth_respond" ~params:[ "id", str id; "value", str item.id ] ]
;;

let save_scoped_models m picker =
  let checked = Picker.checked picker in
  let scoped_models =
    List.filter_map m.models ~f:(fun (model : P.Model.t) ->
      Option.some_if (Set.mem checked model.key) model.key)
  in
  ( { m with mode = Editing }
  , [ set_config
        ~tag:Config_saved
        [ "scoped_models", `Array (List.map scoped_models ~f:str) ]
    ] )
;;

(* ---- sessions picker --------------------------------------------------- *)

let confirm_delete m ~back (sessions : P.Session_summary.t list) =
  match Session_selection.deletable sessions ~current:(current_session m) with
  | Error e -> error m e, []
  | Ok [] -> m, []
  | Ok sessions ->
    ( { m with
        mode =
          Confirm
            { question = Session_selection.delete_question sessions
            ; action =
                Delete_sessions
                  { paths =
                      List.map sessions ~f:(fun (s : P.Session_summary.t) ->
                        s.path)
                  ; back
                  }
            }
      }
    , [] )
;;

let confirm_delete_archived m ~back selection =
  let current = current_session m in
  match Session_selection.delete_archived_question selection ~current with
  | None -> notice m "no archived sessions to delete", []
  | Some question ->
    let paths =
      List.map
        (Session_selection.archived_to_delete selection ~current)
        ~f:(fun (s : P.Session_summary.t) -> s.path)
    in
    ( { m with
        mode = Confirm { question; action = Delete_sessions { paths; back } }
      }
    , [] )
;;

(* The row that takes the highlighted one's place when it moves to the other
   section. *)
let neighbour picker =
  let visible = Picker.visible picker in
  let index = Picker.selected picker in
  Option.map
    (Option.first_some
       (List.nth visible (index + 1))
       (List.nth visible (index - 1)))
    ~f:(fun (item : Picker.Item.t) -> item.id)
;;

let toggle_archived m selection picker =
  match
    Option.bind (Picker.selected_item picker) ~f:(fun item ->
      Session_selection.find selection item.id)
  with
  | None -> m, []
  | Some session when session.running && not session.archived ->
    ( error
        m
        (sprintf
           "%s is running: wait for it, or switch to it and /abort, before \
            archiving"
           (Session_selection.describe session))
    , [] )
  | Some session ->
    let archived = not session.archived in
    let selection =
      Session_selection.set_archived selection ~path:session.path ~archived
    in
    let picker =
      Session_selection.picker
        ~previous:picker
        ?select:(neighbour picker)
        selection
        ~current:(current_session m)
    in
    ( { m with mode = Picker { kind = Sessions selection; picker } }
    , [ call
          (Set_archived { path = session.path; archived })
          ~tag:
            (Archive_done
               { name = Session_selection.describe session; archived })
      ] )
;;

(* Applies [f] to the sessions picker, open or waiting behind a confirmation. *)
let map_sessions_picker m ~f =
  let current = current_session m in
  let update selection picker =
    let selection = f selection in
    ( Mode.Picker_kind.Sessions selection
    , Session_selection.picker ~previous:picker selection ~current )
  in
  match m.mode with
  | Picker { kind = Sessions selection; picker } ->
    let kind, picker = update selection picker in
    { m with mode = Picker { kind; picker } }
  | Confirm
      { question
      ; action =
          Delete_sessions
            { paths; back = Some { kind = Sessions selection; picker } }
      } ->
    let kind, picker = update selection picker in
    let back = Some { Mode.Open_picker.kind; picker } in
    { m with
      mode = Confirm { question; action = Delete_sessions { paths; back } }
    }
  | _ -> m
;;

let sessions_picker_shown m =
  match m.mode with
  | Picker { kind = Sessions _; _ }
  | Confirm
      { action = Delete_sessions { back = Some { kind = Sessions _; _ }; _ }
      ; _
      } -> true
  | _ -> false
;;

let worktree_in_picker m (listing : P.Worktree.Listing.t) picker ~f =
  match
    Option.bind (Picker.selected_item picker) ~f:(fun item ->
      List.find listing.worktrees ~f:(fun w -> String.equal w.path item.id))
  with
  | Some w -> f w
  | None -> m, []
;;

let picker m (kind : Mode.Picker_kind.t) picker (intent : Intent.t) =
  let back = Some { Mode.Open_picker.kind; picker } in
  match intent with
  | (Complete | Toggle_archived | Delete_word_forward)
    when match kind with
         | Sessions _ -> true
         | _ -> false ->
    (match kind, intent with
     | Sessions _, Complete ->
       ( { m with mode = Picker { kind; picker = Picker.toggle_check picker } }
       , [] )
     | Sessions selection, Toggle_archived -> toggle_archived m selection picker
     | Sessions selection, _ -> confirm_delete_archived m ~back selection
     | _ -> m, [])
  | Review_highlighted ->
    (match kind with
     | Worktrees listing ->
       worktree_in_picker m listing picker ~f:(fun w ->
         review_action
           { m with mode = Editing }
           ~command:(Review_command.command ~dir:w.path ())
           (Show { dir = Some w.path; base = None }))
     | _ -> m, [])
  | Picker_toggle_filter ->
    (match kind with
     | Sessions selection ->
       let selection =
         { selection with named_only = not selection.named_only }
       in
       ( { m with
           mode =
             Picker
               { kind = Sessions selection
               ; picker =
                   Session_selection.picker
                     ~previous:picker
                     selection
                     ~current:(current_session m)
               }
         }
       , [] )
     | Worktrees listing ->
       worktree_in_picker m listing picker ~f:(fun w ->
         open_worktree m w ~new_session:true)
     | Models { logged_in_only } ->
       let logged_in_only = not logged_in_only in
       let picker =
         model_picker_value m ~logged_in_only ~query:(Picker.query picker)
       in
       { m with mode = Picker { kind = Models { logged_in_only }; picker } }, []
     | _ -> m, [])
  | Submit when Picker.multi picker -> save_scoped_models m picker
  | Force_quit ->
    (match kind with
     | Agents ->
       (match Picker.selected_item picker with
        | None -> m, []
        | Some item -> cancel_agent { m with mode = Editing } item.id)
     | Jobs ->
       (match Picker.selected_item picker with
        | None -> m, []
        | Some item -> kill_job { m with mode = Editing } item.id)
     | Queue ->
       (match Picker.selected_item picker with
        | None -> m, []
        | Some item -> unqueue m item.id ~edit:false)
     | Reminders ->
       (match Picker.selected_item picker with
        | None -> m, []
        | Some item ->
          remind_action
            m
            ~command:("/remind rm " ^ item.id)
            (Remind_command.Action.Remove item.id))
     | Sessions selection ->
       confirm_delete m ~back (Session_selection.targets selection picker)
     | Worktrees listing ->
       worktree_in_picker m listing picker ~f:(fun w ->
         confirm_remove_worktree m w ~force:false ~back)
     | _ -> { m with quitting = true }, [ Command.Quit ])
  | _ ->
    (match
       Picker.handle
         picker
         (if Intent.equal intent Interrupt then Cancel else intent)
         ~page:(picker_rows m)
     with
     | Continue picker -> { m with mode = Picker { kind; picker } }, []
     | Selected item -> picker_selected m kind item
     | Cancelled ->
       let m = { m with mode = Editing; login_lines = [] } in
       (match kind with
        | Auth_select _ -> m, [ rpc "auth_cancel" ]
        | _ -> m, []))
;;

(* ---- login prompt mode ------------------------------------------------ *)

let login_prompt m ~id ~(prompt : P.Auth_event.Prompt.t) (intent : Intent.t) =
  let allow_empty, keep_lines =
    match prompt with
    | Secret { allow_empty; _ } -> allow_empty, false
    | Text _ -> true, false
    | Manual_code _ -> false, true
    | Select _ -> false, false
  in
  match intent with
  | Submit | Submit_as_typed ->
    (* Answers are never prompt history. *)
    let value, editor = Editor.submit ~secret:true m.editor in
    let value = String.strip value in
    if String.is_empty value && not allow_empty
    then m, []
    else
      ( { m with
          editor
        ; mode = Editing
        ; login_lines = (if keep_lines then m.login_lines else [])
        }
      , [ rpc "auth_respond" ~params:[ "id", str id; "value", str value ] ] )
  | Cancel | Interrupt ->
    ( { m with editor = Editor.clear m.editor; mode = Editing; login_lines = [] }
    , [ rpc "auth_cancel" ] )
  | Force_quit -> { m with quitting = true }, [ Quit ]
  | Up
  | Down
  | Page_up
  | Page_down
  | Scroll_up
  | Scroll_down
  | Complete
  | Cycle_verbosity
  | Next_model
  | Prev_model
  | Next_thinking
  | Cycle_mode
  | Next_agent
  | Focus_agent _
  | Queue_follow_up
  | Dequeue
  | Copy_last
  | Suspend
  | Path_complete
  | Edit_externally
  | Model_picker
  | Snippet_picker
  | Picker_toggle_filter
  | Search
  | Prev_user_message
  | Next_user_message
  | Toggle_archived
  | Review_highlighted
  | Toggle_sidebar
  | Newline -> m, []
  | Insert _
  | Paste _
  | Backspace
  | Delete
  | Left
  | Right
  | Word_left
  | Word_right
  | Delete_word_forward
  | Home
  | End
  | Kill_to_end
  | Kill_to_start
  | Kill_word
  | Yank
  | Yank_pop
  | Undo ->
    let m', cmds = editing m intent in
    { m' with mode = m.mode }, cmds
;;

let text_prompt m ~(action : Mode.Text_prompt_action.t) (intent : Intent.t) =
  let submit text =
    let m =
      { m with
        mode = Editing
      ; editor = Editor.clear m.editor
      ; autocomplete = None
      }
    in
    match action with
    | Name ->
      if String.is_empty text
      then m, []
      else
        ( m
        , [ rpc
              "set_session_name"
              ~params:[ "name", str text ]
              ~tag:(Notice_on_success "session named")
          ] )
    | Cd ->
      if String.is_empty text then m, [] else m, [ change_dir ~target:Cwd text ]
    | Mkdir ->
      if String.is_empty text
      then m, []
      else m, [ change_dir ~if_missing:Create ~target:Cwd text ]
    | Export_path ->
      let command =
        if String.is_empty text
        then rpc "export" ~params:[ "format", str "markdown" ] ~tag:Export_done
        else export_command text
      in
      m, [ command ]
    | Import_path ->
      if String.is_empty text
      then m, []
      else m, [ rpc "import" ~params:[ "path", str text ] ~tag:Reload_messages ]
    | Host_cwd host ->
      if String.is_empty text
      then m, []
      else m, [ set_host_command m ~host ~cwd:text ]
    | Connect -> if String.is_empty text then m, [] else connect m text
    | Forge_host { kind; method_ } ->
      if String.is_empty text
      then m, []
      else forge_login m (Host { host = text; kind = Some kind; method_ })
    | Deny_reason { confirm; draft } ->
      answer_confirm
        { m with editor = Editor.set_text m.editor draft }
        confirm
        Deny_with_reason
        ~reason:text
  in
  let submit_text m =
    let text, _ = Editor.submit m.editor in
    submit (String.strip text)
  in
  (* Directory answers complete on the host they are for. *)
  let directory_host =
    match action with
    | Cd | Mkdir ->
      Some
        (Option.value_map m.state ~default:"backend" ~f:(fun s -> s.active_host))
    | Host_cwd host -> Some host
    | Name | Export_path | Import_path | Deny_reason _ | Connect | Forge_host _
      -> None
  in
  let edit m =
    let m', cmds = editing_intent m intent in
    let m' = { m' with mode = m.mode } in
    match directory_host with
    | None -> m', cmds
    | Some _ ->
      let m', more = refresh_directory_autocomplete m' ~host:directory_host in
      m', cmds @ more
  in
  let accept ~submit_now =
    match m.autocomplete with
    | None -> None
    | Some ac ->
      Option.map (Autocomplete.selected_item ac) ~f:(fun _ ->
        let text = Autocomplete.accept ac ~editor_text:(Editor.text m.editor) in
        let m = { m with editor = Editor.set_text m.editor text } in
        if submit_now
        then submit_text m
        else refresh_directory_autocomplete m ~host:directory_host)
  in
  match intent with
  | Submit ->
    (match m.autocomplete with
     | Some ac when Autocomplete.accepts_on_enter ac ->
       Option.value (accept ~submit_now:true) ~default:(submit_text m)
     | _ -> submit_text m)
  | Submit_as_typed -> submit_text { m with autocomplete = None }
  | Complete ->
    (match m.autocomplete with
     | Some _ -> Option.value (accept ~submit_now:false) ~default:(m, [])
     | None -> refresh_directory_autocomplete m ~host:directory_host)
  | Up when Option.is_some m.autocomplete ->
    { m with autocomplete = Option.map m.autocomplete ~f:Autocomplete.up }, []
  | Down when Option.is_some m.autocomplete ->
    { m with autocomplete = Option.map m.autocomplete ~f:Autocomplete.down }, []
  | Cancel when Option.is_some m.autocomplete ->
    { m with autocomplete = None }, []
  | Cancel | Interrupt ->
    (match action with
     | Deny_reason { confirm; draft } ->
       (* Back to the question, nothing answered yet. *)
       ( { (confirm_tool m confirm) with
           editor = Editor.set_text m.editor draft
         }
       , [] )
     | Name
     | Cd
     | Mkdir
     | Export_path
     | Import_path
     | Host_cwd _
     | Connect
     | Forge_host _ ->
       ( { m with
           editor = Editor.clear m.editor
         ; mode = Editing
         ; autocomplete = None
         }
       , [] ))
  | Force_quit -> { m with quitting = true }, [ Quit ]
  | Newline -> m, []
  | Insert _
  | Paste _
  | Backspace
  | Delete
  | Left
  | Right
  | Word_left
  | Word_right
  | Delete_word_forward
  | Home
  | End
  | Kill_to_end
  | Kill_to_start
  | Kill_word
  | Yank
  | Yank_pop
  | Undo -> edit m
  | Up
  | Down
  | Page_up
  | Page_down
  | Scroll_up
  | Scroll_down
  | Cycle_verbosity
  | Next_model
  | Prev_model
  | Next_thinking
  | Cycle_mode
  | Next_agent
  | Focus_agent _
  | Queue_follow_up
  | Dequeue
  | Copy_last
  | Suspend
  | Path_complete
  | Edit_externally
  | Model_picker
  | Snippet_picker
  | Picker_toggle_filter
  | Search
  | Prev_user_message
  | Next_user_message
  | Toggle_archived
  | Review_highlighted
  | Toggle_sidebar -> m, []
;;

(* ---- confirm mode ----------------------------------------------------- *)

let back_to m (back : Mode.Open_picker.t option) =
  match back with
  | Some { kind; picker } -> { m with mode = Picker { kind; picker } }
  | None -> m
;;

let confirm m ~(action : Mode.Confirm_action.t) (intent : Intent.t) =
  let yes m =
    match action with
    | Logout provider ->
      m, [ rpc "logout" ~params:[ "provider", str provider ] ]
    | Forge_logout host -> m, [ call (Forge_logout host) ]
    | Cd_to_checkout path -> m, [ change_dir ~target:Cwd path ]
    | Rewind id ->
      m, [ rpc "rewind" ~params:[ "to", str id ] ~tag:Reload_messages ]
    | Delete_sessions { paths; back } ->
      back_to m back, [ call (Delete_sessions paths) ~tag:Sessions_deleted ]
    | Tool_confirm c -> answer_confirm m c Allow
    | Nix_gc generations -> start_nix_gc m (Some generations)
    | Create_dir { target; path } ->
      m, [ change_dir ~if_missing:Create ~target path ]
    | Remove_worktree { worktree; force; back = _ } ->
      ( notice m (sprintf "removing the worktree at %s…" worktree.path)
      , [ call
            (Remove_worktree { path = worktree.path; force; cwd = None })
            ~tag:
              (Worktree_removed
                 { target = Worktree_command.target worktree; force })
        ] )
  in
  let no m =
    match action with
    | Tool_confirm c -> answer_confirm m c Deny
    | Delete_sessions { back = Some _ as back; _ } -> back_to m back, []
    | Remove_worktree { back = Some _ as back; _ } -> back_to m back, []
    | Create_dir { path; _ } -> notice m (sprintf "not created: %s" path), []
    | Cd_to_checkout path ->
      notice m (sprintf "staying here; /cd %s goes there" path), []
    | Logout _
    | Forge_logout _
    | Rewind _
    | Delete_sessions _
    | Remove_worktree _
    | Nix_gc _ -> notice m "cancelled", []
  in
  match intent, action with
  | (Insert ("y" | "Y") | Submit), _ -> yes { m with mode = Editing }
  | (Insert ("n" | "N") | Cancel | Interrupt), _ -> no { m with mode = Editing }
  | Insert ("a" | "A"), Tool_confirm ({ rules = _ :: _; _ } as c)
    when not (Confirm_prompt.is_plan c) ->
    answer_confirm { m with mode = Editing } c Always
  | Insert ("r" | "R"), Tool_confirm c ->
    ( { m with
        mode =
          Text_prompt
            { question = Confirm_prompt.reason_question c
            ; action = Deny_reason { confirm = c; draft = Editor.text m.editor }
            }
      ; editor = Editor.clear m.editor
      ; autocomplete = None
      }
    , [] )
  | Force_quit, _ -> { m with quitting = true }, [ Quit ]
  | _ -> m, []
;;

(* ---- sign-in dialog --------------------------------------------------- *)

let sign_in_mode m form (intent : Intent.t) =
  let set form = { m with mode = Sign_in form }, [] in
  match intent with
  | Submit | Submit_as_typed ->
    (match m.connection with
     | Switching _ -> m, []
     | Connected | Reconnecting _ | Disconnected _ ->
       (match Sign_in_form.submit form with
        | `Wait form -> set form
        | `Sign_in (form, change) ->
          relink { m with mode = Sign_in form } change))
  | Cancel | Interrupt -> { m with mode = Editing }, []
  | Force_quit -> { m with quitting = true }, [ Quit ]
  | Complete | Up | Down -> set (Sign_in_form.toggle form)
  | Insert text | Paste text -> set (Sign_in_form.insert form text)
  | Backspace -> set (Sign_in_form.backspace form)
  | Kill_to_start | Kill_word -> set (Sign_in_form.clear form)
  | _ -> m, []
;;

(* ---- the pull request form -------------------------------------------- *)

let pr_form_mode m form (intent : Intent.t) =
  let set form = { m with mode = Pr_form form }, [] in
  match intent with
  | Submit | Submit_as_typed ->
    (match Pr_form.submit form with
     | `Wait form -> set form
     | `Create (form, request) ->
       { m with mode = Pr_form form }, [ call request ~tag:Pr_created ])
  | Cancel | Interrupt ->
    ( notice
        { m with mode = Editing }
        (if Pr_form.submitting form
         then "closed; the pull request is still being created"
         else "no pull request created")
    , [] )
  | Force_quit -> { m with quitting = true }, [ Quit ]
  | Complete | Down -> set (Pr_form.next form)
  | Next_agent | Up -> set (Pr_form.prev form)
  | Newline -> set (Pr_form.newline form)
  | Insert text | Paste text -> set (Pr_form.insert form text)
  | Backspace -> set (Pr_form.backspace form)
  | Kill_to_start -> set (Pr_form.clear form)
  | Kill_word -> set (Pr_form.kill_word form)
  | _ -> m, []
;;

(* ---- question mode ---------------------------------------------------- *)

let question m ~call_id ~form (intent : Intent.t) =
  let respond m params =
    ( { m with mode = Editing }
    , [ rpc "ask_user_respond" ~params:(("call_id", str call_id) :: params) ] )
  in
  let set form = { m with mode = Question { call_id; form } }, [] in
  match intent with
  | Submit ->
    (match Question_form.accept form with
     | `Next form -> set form
     | `Done answers ->
       respond
         (notice m "answered")
         [ ( "answers"
           , `Array (List.map answers ~f:(fun a -> `Array (List.map a ~f:str)))
           )
         ])
  | Cancel | Interrupt ->
    respond (notice m "declined to answer") [ "decline", P.Json.bool true ]
  | Up -> set (Question_form.up form)
  | Down -> set (Question_form.down form)
  | Left -> set (Question_form.prev form)
  | Right | Complete -> set (Question_form.next form)
  | Insert " " -> set (Question_form.toggle form)
  | Insert text | Paste text -> set (Question_form.insert form text)
  | Backspace -> set (Question_form.backspace form)
  | Force_quit -> { m with quitting = true }, [ Quit ]
  | _ -> m, []
;;

(* ---- output box ------------------------------------------------------- *)

(* The box owns the keys that scroll it and Esc; anything else closes it and
   goes to the editor, so typing on after [/help] just works. *)
let output_mode m box (intent : Intent.t) =
  let rows = Int.max 1 ((layout m).main_rows - 2) in
  let width = Boxed.inner_width ~width:(transcript_width m) in
  let scroll by =
    { m with mode = Output (Output_box.scroll box ~by ~width ~rows) }, []
  in
  match intent with
  | Cancel | Interrupt | Submit -> { m with mode = Editing }, []
  | Up | Scroll_up -> scroll (-1)
  | Down | Scroll_down -> scroll 1
  | Page_up -> scroll (-rows)
  | Page_down -> scroll rows
  | Home -> scroll (-List.length (Output_box.wrapped box ~width))
  | End -> scroll (List.length (Output_box.wrapped box ~width))
  | Force_quit -> { m with quitting = true }, [ Command.Quit ]
  | _ -> editing { m with mode = Editing } intent
;;

let toggle_sidebar m =
  let m = { m with sidebar = not m.sidebar } in
  let narrow = m.width < Layout.sidebar_threshold in
  match m.sidebar, narrow with
  | true, true ->
    notice
      m
      (sprintf
         "sidebar on: it shows at %d columns and more (now %d)"
         Layout.sidebar_threshold
         m.width)
  | false, true -> notice m "sidebar off"
  | _, false -> m
;;

let intent m (intent : Intent.t) =
  let m =
    if Intent.equal intent Interrupt then m else { m with pending_quit = false }
  in
  match m.mode, intent with
  | _, Toggle_sidebar -> toggle_sidebar m, []
  | Output box, _ -> output_mode m box intent
  | mode, _ ->
    (match mode with
     | Output _ -> m, []
     | Editing -> editing m intent
     | Picker { kind; picker = p } -> picker m kind p intent
     | Login_prompt { id; prompt } -> login_prompt m ~id ~prompt intent
     | Text_prompt { action; _ } -> text_prompt m ~action intent
     | Confirm { action; _ } -> confirm m ~action intent
     | Question { call_id; form } -> question m ~call_id ~form intent
     | Sign_in form -> sign_in_mode m form intent
     | Pr_form form -> pr_form_mode m form intent
     | Search _ -> search m intent)
;;

(* ---- backend events --------------------------------------------------- *)

let auth_event m (e : P.Auth_event.t) =
  match e with
  | Auth_url { url; instructions } ->
    let lines = [ "Open this URL to log in:"; "  " ^ url; instructions ] in
    { m with login_lines = m.login_lines @ lines }, [ Command.Open_browser url ]
  | Prompt { id; prompt } ->
    if
      Mode.is_dialog m.mode
      && not
           (match m.mode with
            | Login_prompt _ -> true
            | _ -> false)
    then
      ( warn
          m
          "login prompt arrived while a dialog was open; press Esc to reach it"
      , [ rpc "auth_cancel" ] )
    else (
      match prompt with
      | Select { message; options } ->
        let items =
          List.map options ~f:(fun (id, label) ->
            Picker.Item.create ~id ~detail:id label)
        in
        ( { m with
            mode =
              Picker
                { kind = Auth_select id
                ; picker = Picker.create ~title:message items
                }
          }
        , [] )
      | Secret _ | Manual_code _ | Text _ ->
        let lines =
          String.split_lines (P.Auth_event.Prompt.message prompt)
          @
          match prompt with
          | Manual_code { placeholder; _ } ->
            [ "e.g. " ^ placeholder ^ "?code=..." ]
          | Text { placeholder; default; _ }
            when (not (String.is_empty placeholder))
                 && not (String.equal placeholder default) ->
            [ "e.g. " ^ placeholder ]
          | Secret { allow_empty = true; _ } ->
            [ "(Enter with nothing typed for none)" ]
          | Secret _ | Text _ | Select _ -> []
        in
        let editor =
          match prompt with
          | Text { default; _ } ->
            Editor.set_text (Editor.clear m.editor) default
          | Secret _ | Manual_code _ | Select _ -> Editor.clear m.editor
        in
        ( { m with
            mode = Login_prompt { id; prompt }
          ; editor
          ; login_lines = m.login_lines @ lines
          }
        , [] ))
  | Prompt_cancelled { id } ->
    (match m.mode with
     | Login_prompt { id = current; _ } when String.equal id current ->
       ( { m with
           mode = Editing
         ; editor = Editor.clear m.editor
         ; login_lines = []
         }
       , [] )
     | Picker { kind = Auth_select current; _ } when String.equal id current ->
       { m with mode = Editing; login_lines = [] }, []
     | _ -> m, [])
  | Progress message -> { m with login_lines = m.login_lines @ [ message ] }, []
  | Done { provider; method_ } ->
    let m =
      match m.mode with
      | Login_prompt _ -> { m with mode = Editing }
      | _ -> m
    in
    let m = { m with login_lines = [] } in
    let m = notice m (sprintf "logged in to %s (%s)" provider method_) in
    let same =
      Option.value_map m.state ~default:false ~f:(fun s ->
        String.equal s.model.provider provider)
    in
    ( m
    , rpc "auth_status" ~tag:Auth_refresh
      ::
      (if same
       then []
       else [ rpc "list_models" ~tag:(Models_after_login provider) ]) )
  | Failed { provider; error = e } ->
    let m =
      match m.mode with
      | Login_prompt _ -> { m with mode = Editing }
      | _ -> m
    in
    let m = { m with login_lines = [] } in
    error m (sprintf "login to %s failed: %s" provider e), []
  | Logged_out provider ->
    ( notice m (sprintf "logged out of %s" provider)
    , [ rpc "auth_status" ~tag:Auth_refresh ] )
  | Forge_done { host; user; method_ } ->
    let m =
      match m.mode with
      | Login_prompt _ -> { m with mode = Editing }
      | _ -> m
    in
    let m = { m with login_lines = [] } in
    ( notice
        m
        (sprintf
           "signed in to %s%s (%s)"
           host
           (Option.value_map user ~default:"" ~f:(fun u -> " as " ^ u))
           method_)
    , [ call Forge_status ~tag:Forge_refresh ] )
  | Forge_logged_out host ->
    ( notice m (sprintf "signed out of %s" host)
    , [ call Forge_status ~tag:Forge_refresh ] )
;;

(* A message the user sent a subagent, once it takes it: a quiet line in the
   main transcript. *)
let note_agent_message m ~agent_id (user : P.Message.User.t) =
  let from_user (n : P.Origin.Note.t) =
    P.Origin.Kind.equal n.kind (Other "steer")
    && String.is_prefix n.title ~prefix:"You"
  in
  match
    Option.bind user.origin ~f:(fun o -> List.find o.notes ~f:from_user)
  with
  | None -> m
  | Some note ->
    let text =
      String.split_lines (P.Message.User.body user)
      |> List.map ~f:String.strip
      |> List.filter ~f:(Fn.non String.is_empty)
      |> String.concat ~sep:" "
    in
    let kind =
      if String.is_substring note.title ~substring:"follow-up"
      then " (follow-up)"
      else ""
    in
    with_transcript m ~f:(fun t ->
      Transcript.add
        t
        (Note
           { kind = note.kind
           ; title = sprintf "%s%s: %s" agent_id kind text
           ; text = ""
           ; joined = true
           }))
;;

let event m (e : P.Event.t) =
  let m = with_transcript m ~f:(fun t -> Transcript.apply t e) in
  let m = { m with activity = Activity.apply m.activity e } in
  let m = update_agents m ~f:(fun agents -> Agent_view.apply_all agents e) in
  match e with
  | State state -> set_state m state
  | Queue_update { steer; follow_up } ->
    { m with queued_messages = Queued_message.of_queues ~steer ~follow_up }, []
  | Agent_queues agent_queues -> { m with agent_queues }, []
  | Subagent { agent_id; event = Message_end (User user); _ } ->
    note_agent_message m ~agent_id user, []
  | Config_changed config -> { m with config = Some config }, []
  | Review _ -> m, []
  | Auth a -> auth_event m a
  | Tool_confirm c -> add_pending_confirms m [ c ], []
  | Tool_confirm_done { call_id; agent_id } ->
    let done_ (c : P.Pending.Confirm.t) =
      String.equal c.call_id call_id
      && Option.equal String.equal c.agent_id agent_id
    in
    let m =
      { m with
        pending_confirms = List.filter m.pending_confirms ~f:(Fn.non done_)
      }
    in
    (match m.mode with
     | Confirm { action = Tool_confirm c; _ } when done_ c ->
       notice { m with mode = Editing } "confirmation answered elsewhere", []
     | Text_prompt { action = Deny_reason { confirm; draft }; _ }
       when done_ confirm ->
       ( notice
           { m with mode = Editing; editor = Editor.set_text m.editor draft }
           "confirmation answered elsewhere"
       , [] )
     | _ -> m, [])
  | Ask_user a -> add_pending_questions m [ a ], []
  | Ask_user_done { call_id; timed_out } ->
    let m =
      { m with
        pending_questions =
          List.filter m.pending_questions ~f:(fun q ->
            not (String.equal q.call_id call_id))
      }
    in
    (match m.mode with
     | Question { call_id = shown; _ } when String.equal shown call_id ->
       (* On a timeout the backend's notice says so. *)
       if timed_out
       then { m with mode = Editing }, []
       else notice { m with mode = Editing } "question answered elsewhere", []
     | _ -> m, [])
  | Btw_delta { btw_id; delta } ->
    update_btw m btw_id ~f:(fun box -> Btw_box.add_delta box delta), []
  | Agent_start
  | Agent_end _
  | Turn_start
  | Turn_end _
  | Message_start _
  | Message_update _
  | Message_end _
  | Retry _
  | Tool_start _
  | Tool_output _
  | Tool_end _
  | Compacted _
  | Notice _
  | Subagent_start _
  | Subagent _
  | Subagent_end _
  | Tool_exec _
  | Tool_exec_cancel _
  | Tool_exec_input _
  | Terminal_open _
  | Terminal_frame _
  | Terminal_close _
  | Terminal_kill _ -> m, []
  | Prefs_changed changes ->
    (match
       List.find changes ~f:(fun c -> String.equal c.key history_key)
       |> Option.bind ~f:(fun c -> history_of_json c.value)
     with
     | Some entries -> set_backend_history m entries, []
     | None -> m, [])
  | Nix_gc_progress progress ->
    (match m.nix_gc with
     | Some _ -> { m with nix_gc = Some progress }, []
     | None -> m, [])
  | Sessions_changed ->
    if sessions_picker_shown m || Option.is_some m.sessions
    then m, [ rpc "list_sessions" ~tag:Sessions_refresh ]
    else m, []
;;

(* ---- rpc replies ------------------------------------------------------ *)

let decode_skills json = P.Json.list_field json "skills" ~f:P.Skill.of_json

(* Back in the editor (unless something else is there now) so the name can be
   fixed, and no longer queued. *)
let skill_prompt_failed m text e =
  let queued_messages =
    List.filter m.queued_messages ~f:(fun (q : Queued_message.t) ->
      not (String.equal q.text text))
  in
  let editor =
    if Editor.is_empty m.editor then Editor.set_text m.editor text else m.editor
  in
  error { m with queued_messages; editor } e
;;

let skills_for_autocomplete m =
  match m.mode, m.autocomplete with
  | Editing, Some ac
    when Autocomplete.Source.equal (Autocomplete.source ac) Skill ->
    refresh_autocomplete m
  | _ -> m, []
;;

let mcp_approved m server (list : P.Mcp_list.t) =
  let m =
    match
      List.find list.servers ~f:(fun (s : P.Mcp_server.t) ->
        String.equal s.name server)
    with
    | Some ({ status = Failed; _ } as s) ->
      error
        m
        (sprintf
           "approved %s, but it %s; check its entry in %s, then /mcp reconnect"
           server
           (mcp_status s)
           s.source)
    | Some s -> notice m (sprintf "approved %s: %s" server (mcp_status s))
    | None -> notice m (sprintf "approved %s" server)
  in
  (* Back to the refreshed list, unless the user has moved on meanwhile. *)
  if Mode.is_dialog m.mode || not (Editor.is_empty m.editor)
  then m, []
  else mcp_picker ~problems:false m list, []
;;

(* A missing directory is created only once confirmed; when another dialog
   has opened meanwhile, the notice says how to create it instead. *)
let dir_changed
      m
      (target : P.Dir_change.Target.t)
      (outcome : P.Dir_change.Outcome.t)
  =
  match outcome, target with
  | Missing path, Cwd when Mode.is_dialog m.mode ->
    warn m (sprintf "%s does not exist; /mkdir %s creates it" path path), []
  | Missing path, Host { name; _ } when Mode.is_dialog m.mode ->
    ( warn
        m
        (sprintf "%s does not exist on %s; /host %s asks again" path name name)
    , [] )
  | Missing path, _ ->
    ( { m with
        mode =
          Confirm
            { question = create_dir_question target path
            ; action = Create_dir { target; path }
            }
      ; autocomplete = None
      }
    , [] )
  | Changed { created; cwd }, _ ->
    let created =
      if created
      then sprintf "created %s; " (Option.value cwd ~default:"the directory")
      else ""
    in
    (match target with
     | Cwd -> notice m (created ^ "cwd changed"), []
     | Host _ -> notice m (created ^ "tool host switched"), [])
;;

let rec reply m (tag : Reply_tag.t) (result : (P.Json.t, string) Result.t) =
  let decode json ~f k =
    match f json with
    | Ok v -> k v
    | Error e ->
      log ~severity:Error m ("protocol error: " ^ Error.to_string_hum e), []
  in
  match result with
  | Error e ->
    (match tag with
     | Retry_in_editor { command; hint; then_ = _ } ->
       let editor =
         if Editor.is_empty m.editor
         then Editor.set_text m.editor command
         else m.editor
       in
       ( error
           { m with editor }
           (if String.is_empty hint then e else sprintf "%s; %s" e hint)
       , [] )
     | Ignore | Prefs_history -> m, []
     | Review_named { command; model = Some args; _ } ->
       (* Not a repository: no worktree by that name, the model reviews. *)
       review_action m ~command (Model args)
     | Review_named { command; _ } -> error (restore_command m command) e, []
     | Forge_refresh ->
       (* A backend without git hosting: the hosts stay unlisted. *)
       m, []
     | Btw id -> update_btw m id ~f:(fun box -> Btw_box.fail box e), []
     | Set_model_done _ ->
       (* The backend formats "did you mean"; the picker helps recover. *)
       model_picker (error m e) ~query:"", []
     | Skill_prompt text -> skill_prompt_failed m text e, []
     | Agent_message { agent_id; kind; text; retry } ->
       agent_message_failed m ~agent_id ~kind ~text ~retry e, []
     | Fallback_saved text | Default_dir_saved text ->
       (* Back in the editor, to correct rather than retype. *)
       let editor =
         if Editor.is_empty m.editor
         then Editor.set_text m.editor text
         else m.editor
       in
       error { m with editor } e, []
     | Skills_for_autocomplete key ->
       skills_for_autocomplete
         { m with skills = Location_cache.set m.skills ~key [] }
     | Mcp_approved server ->
       error m (sprintf "approving MCP server %s failed: %s" server e), []
     | Mode_set _ ->
       (* The mode shown at once was not taken. *)
       error m e, [ rpc "get_state" ~tag:Refresh_state ]
     | Worktrees_for_autocomplete key ->
       refresh_autocomplete
         { m with
           worktrees =
             Location_cache.set
               m.worktrees
               ~key
               { root = ""; project_dir = ""; worktrees = [] }
         }
     | Worktree_removed { target; force } ->
       worktree_remove_failed m ~target ~force e
     | Archive_done _ ->
       (* Undo the picker's optimistic change. *)
       error m e, [ rpc "list_sessions" ~tag:Sessions_refresh ]
     | Pr_created ->
       (match m.mode with
        | Pr_form form ->
          { m with mode = Pr_form (Pr_form.failed form ~error:e) }, []
        | _ -> error m e, [])
     | Nix_gc_done -> error { m with nix_gc = None } e, []
     | _ -> error m e, [])
  | Ok json ->
    (match tag with
     | Retry_in_editor { then_; _ } -> reply m then_ result
     | Ignore | Show_error | Reconnect _ -> m, []
     | Skill_prompt _ | Agent_message _ -> m, []
     | Skills_picker ->
       decode json ~f:decode_skills (fun skills -> skills_picker m skills, [])
     | Skills_for_autocomplete key ->
       decode json ~f:decode_skills (fun skills ->
         skills_for_autocomplete
           { m with skills = Location_cache.set m.skills ~key skills })
     | Mcp_picker ->
       decode json ~f:P.Mcp_list.of_json (fun list -> mcp_picker m list, [])
     | Nix_store_shown ->
       decode json ~f:P.Nix_store_status.of_json (fun status ->
         ( output
             m
             ~title:"Nix store"
             (Content.lines
                (String.concat
                   ~sep:"\n"
                   (Nix_gc_command.status_lines ~utc_offset:m.utc_offset status)))
         , [] ))
     | Nix_gc_done ->
       decode json ~f:P.Nix_gc_report.of_json (fun report ->
         ( notice
             { m with nix_gc = None }
             ("Nix garbage collected: " ^ Nix_gc_command.report report)
         , [] ))
     | Mcp_reconnected ->
       decode json ~f:P.Mcp_list.of_json (fun list ->
         let m = mcp_problems m list in
         if List.is_empty list.servers
         then notice m ("no MCP servers; " ^ mcp_config_hint), []
         else output m ~title:"MCP servers" (mcp_summary list), [])
     | Mcp_approved server ->
       decode json ~f:P.Mcp_list.of_json (mcp_approved m server)
     | Pending ->
       decode json ~f:P.Pending.of_json (fun pending ->
         let m =
           { m with
             queued_messages =
               Queued_message.of_queues
                 ~steer:pending.steer_texts
                 ~follow_up:pending.follow_up_texts
           ; agent_queues = pending.agents
           }
         in
         ( add_pending_questions
             (add_pending_confirms m pending.confirms)
             pending.questions
         , [] ))
     | Mode_set text ->
       decode json ~f:P.State.of_json (fun state ->
         let m, commands = set_state m state in
         notice m text, commands)
     | Refresh_state -> decode json ~f:P.State.of_json (set_state m)
     | Subagent_loaded ->
       decode json ~f:P.Subagent_detail.of_json (fun d ->
         subagent_loaded m d, [])
     | Btw id ->
       decode
         json
         ~f:(fun json -> P.Json.string_field json "text")
         (fun text -> update_btw m id ~f:(Btw_box.finish ~text), [])
     | Notice_on_success text -> notice m text, []
     | Reminder_added -> notice m (Remind_command.added json), []
     | Dir_changed target ->
       decode json ~f:P.Dir_change.Outcome.of_json (fun outcome ->
         dir_changed m target outcome)
     | Worktrees_picker ->
       decode json ~f:P.Worktree.Listing.of_json (fun listing ->
         worktrees_picker m listing, [])
     | Worktrees_for_autocomplete key ->
       decode json ~f:P.Worktree.Listing.of_json (fun listing ->
         refresh_autocomplete
           { m with worktrees = Location_cache.set m.worktrees ~key listing })
     | Worktree_target { command; action } ->
       decode json ~f:P.Worktree.Listing.of_json (fun listing ->
         worktree_target m ~command action listing)
     | Worktree_created ->
       decode json ~f:P.Worktree.Created.of_json (fun created ->
         let text = Worktree_command.created created in
         ( notice { m with worktrees = Location_cache.empty } text
         , [ enter_worktree ~notice:text created.worktree.path ] ))
     | Worktree_entered text ->
       decode json ~f:P.Dir_change.Outcome.of_json (function
         | Changed _ -> notice m text, []
         | Missing path ->
           error m (sprintf "%s is gone; /worktree lists the others" path), [])
     | Worktree_session_started { path; notice } ->
       let m, commands = reset_session m in
       follow m, commands @ [ enter_worktree ~notice path ]
     | Worktree_removed _ ->
       decode json ~f:P.Worktree.removed_of_json (fun w ->
         ( notice
             { m with worktrees = Location_cache.empty }
             (Worktree_command.removed w)
         , [] ))
     | Set_model_done key ->
       (match
          List.find m.models ~f:(fun model -> String.equal model.key key)
        with
        | Some model when not (logged_in m model.provider) ->
          ( warn
              m
              (sprintf
                 "model: %s (not logged in; /login %s)"
                 key
                 model.provider)
          , [] )
        | _ -> notice m (sprintf "model: %s" key), [])
     | Compact_done -> notice m "context compacted", []
     | Git_diff ->
       decode json ~f:P.Git_diff.of_json (fun diff ->
         if String.is_empty diff.diff
         then
           ( notice
               m
               (sprintf
                  "no changes in %s against %s"
                  diff.root
                  (Diff_files.against diff))
           , [] )
         else output m ~title:"Changes" (Diff_files.render diff), [])
     | Review_comments { dir; base } ->
       decode json ~f:P.Review.of_json (fun review ->
         let status = Reply_tag.Review_status { review; dir } in
         ( m
         , [ rpc
               "git_diff"
               ~params:(P.Git_diff.params ?dir ?base ())
               ~tag:
                 (match dir, base with
                  | None, None -> status
                  | _ ->
                    Retry_in_editor
                      { command = Review_command.command ?dir ?base ()
                      ; hint = ""
                      ; then_ = status
                      })
           ] ))
     | Review_named { command; name; base; model } ->
       decode json ~f:P.Worktree.Listing.of_json (fun listing ->
         review_action
           (cache_worktrees m listing)
           ~command
           (Review_command.resolve listing ~name ~base ~model))
     | Review_status { review; dir } ->
       decode json ~f:P.Git_diff.of_json (fun diff ->
         let review =
           P.Review.scope review ~root:diff.root ~own:(Option.is_none dir)
         in
         if String.is_empty diff.diff && List.is_empty review.comments
         then
           ( notice
               m
               (sprintf
                  "nothing to review: no changes in %s against %s"
                  (Diff_files.where diff)
                  (Diff_files.against diff))
           , [] )
         else output m ~title:"Review" (File_approval.render diff review), [])
     | Review_started ->
       (* The run's first message shows what started; a busy agent queues it. *)
       (match m.state with
        | Some { running = true; _ } ->
          notice m "queued: it starts when the current run ends", []
        | Some _ | None -> m, [])
     | Abort_done ->
       decode json ~f:decode_restored (fun restored ->
         match restored with
         | [] -> m, []
         | restored ->
           let joined = String.concat ~sep:"\n\n" restored in
           let text =
             if Editor.is_empty m.editor
             then joined
             else joined ^ "\n\n" ^ Editor.text m.editor
           in
           let count = List.length restored in
           let noun = if count = 1 then "message" else "messages" in
           let m =
             { m with
               editor = Editor.set_text m.editor text
             ; queued_messages = []
             }
           in
           ( notice m (sprintf "restored %d queued %s to the editor" count noun)
           , [] ))
     | Initial_state ->
       decode json ~f:P.State.of_json (fun state ->
         let m, commands = set_state m state in
         log_block m [ banner m state ], commands)
     | Initial_messages ->
       decode json ~f:(decode_list ~f:P.Message.of_json) (fun messages ->
         let transcript =
           List.fold messages ~init:m.transcript ~f:Transcript.add_message
         in
         follow { m with transcript }, [])
     | Reload_messages | Reload_messages_notice _ ->
       let m, commands = reset_session m in
       let m =
         match tag with
         | Reload_messages_notice text -> notice m text
         | _ -> m
       in
       follow m, commands
     | Users_list ->
       decode
         json
         ~f:
           (decode_list ~f:(fun j ->
              match j with
              | `String s -> Ok s
              | _ -> Or_error.error_string "expected a user name"))
         (fun users ->
            ( output
                m
                ~title:"Users"
                (Content.lines
                   (String.concat
                      ~sep:"\n"
                      ("users (/setusr NAME to act as one):"
                       :: List.map users ~f:(fun u -> "  " ^ u))))
            , [] ))
     | User_switched ->
       decode
         json
         ~f:P.Hello_reply.of_json
         (fun { client_id; namespace; user; superuser = _ } ->
            let m, session_commands =
              reset_session
                { m with
                  client_id = Some client_id
                ; namespace
                ; user
                ; sessions = None
                }
            in
            let m =
              notice
                m
                (match Model.acting_as m, namespace with
                 | Some other, _ -> sprintf "acting as %s" other
                 | None, Some own -> sprintf "back to %s" own
                 | None, None -> "switched user")
            in
            ( follow m
            , session_commands
              @ [ rpc "auth_status" ~tag:Auth_refresh
                ; call Forge_status ~tag:Forge_refresh
                ; rpc "get_config" ~tag:Config
                ] ))
     | Session_stats ->
       decode json ~f:P.Session_stats.of_json (fun stats ->
         output m ~title:"Session" (format_stats stats), [])
     | Entries_for_fork ->
       decode json ~f:decode_entries (fun (_head, entries) ->
         fork_picker m entries, [])
     | Entries_for_rewind ->
       decode json ~f:decode_entries (fun (_head, entries) ->
         rewind_picker m entries, [])
     | Entries_for_tree ->
       decode json ~f:decode_entries (fun (head, entries) ->
         tree_picker m entries head, [])
     | Export_done ->
       decode
         json
         ~f:(fun j -> P.Json.string_field j "path")
         (fun path -> notice m (sprintf "exported to %s" path), [])
     | Sessions_deleted ->
       decode json ~f:P.Delete_result.of_json (fun result ->
         let severity : Severity.t =
           match result.deleted, result.refused with
           | _, [] -> Info
           | [], _ -> Error
           | _ -> Warn
         in
         let paths =
           List.map result.deleted ~f:(fun (d : P.Delete_result.Deleted.t) ->
             d.path)
         in
         let m = map_sessions_picker m ~f:(Session_selection.remove ~paths) in
         let m =
           { m with
             sessions =
               Option.map m.sessions ~f:(fun sessions ->
                 (Session_selection.remove
                    (Session_selection.create sessions)
                    ~paths)
                   .sessions)
           }
         in
         notice ~severity m (P.Delete_result.summary result), [])
     | Sessions_to_delete args ->
       decode
         json
         ~f:(decode_list ~f:P.Session_summary.of_json)
         (fun sessions ->
            match
              Result.bind
                (Session_selection.resolve sessions args)
                ~f:(Session_selection.deletable ~current:(current_session m))
            with
            | Error e ->
              ( error
                  (restore_command m ("/delete " ^ String.concat ~sep:" " args))
                  e
              , [] )
            | Ok _ when Mode.is_dialog m.mode ->
              warn m "close the current dialog first (Esc)", []
            | Ok found -> confirm_delete m ~back:None found)
     | Archived_to_delete ->
       decode
         json
         ~f:(decode_list ~f:P.Session_summary.of_json)
         (fun sessions ->
            if Mode.is_dialog m.mode
            then warn m "close the current dialog first (Esc)", []
            else
              confirm_delete_archived
                m
                ~back:None
                (Session_selection.create sessions))
     | Archive_done { name; archived } ->
       notice m (sprintf "%s %s" (archive_verb ~archived) name), []
     | Session_to_archive { arg; archived } ->
       decode
         json
         ~f:(decode_list ~f:P.Session_summary.of_json)
         (archive_resolved m ~arg ~archived)
     | Sessions_refresh ->
       decode
         json
         ~f:(decode_list ~f:P.Session_summary.of_json)
         (fun sessions ->
            let m =
              map_sessions_picker m ~f:(fun selection ->
                { selection with sessions })
            in
            let m = { m with sessions = Some sessions } in
            match m.mode, m.autocomplete with
            | Editing, Some _ -> refresh_autocomplete m
            | _ -> m, [])
     | Auth_refresh ->
       decode json ~f:(decode_list ~f:P.Auth_status.of_json) (fun auth ->
         { m with auth }, [])
     | Auth_show ->
       decode json ~f:(decode_list ~f:P.Auth_status.of_json) (fun auth ->
         let m = { m with auth } in
         output m ~title:logins_title (format_auth m), [])
     | Auth_login_picker ->
       decode json ~f:(decode_list ~f:P.Auth_status.of_json) (fun auth ->
         login_picker { m with auth }, [])
     | Auth_logout_picker ->
       decode json ~f:(decode_list ~f:P.Auth_status.of_json) (fun auth ->
         logout_picker { m with auth }, [])
     | Forge_refresh ->
       decode json ~f:P.Forge_account.list_of_json (fun forge ->
         forge_refreshed m forge, [])
     | Prs_picker state ->
       decode json ~f:P.Pr_list.of_json (fun list ->
         prs_picker m ~state list, [])
     | Pr_shown ->
       decode json ~f:P.Pr_detail.of_json (fun detail ->
         ( output
             m
             ~title:(sprintf "Pull request #%d" detail.pr.number)
             (Pr_view.render detail)
         , [] ))
     | Pr_form_defaults title ->
       decode json ~f:P.Pr_defaults.of_json (fun defaults ->
         if Mode.is_dialog m.mode
         then warn m "close the current dialog first (Esc), then /pr new", []
         else
           ( { m with
               mode = Pr_form (Pr_form.create ?title defaults)
             ; autocomplete = None
             }
           , [] ))
     | Pr_created ->
       decode json ~f:P.Pr_detail.of_json (fun { pr; _ } ->
         let m =
           match m.mode with
           | Pr_form _ -> { m with mode = Editing }
           | _ -> m
         in
         notice m (sprintf "#%d created: %s" pr.number pr.url), [])
     | Pr_checked_out number ->
       decode json ~f:P.Pr_checkout.of_json (fun checkout ->
         pr_checked_out m number checkout, [])
     | Config ->
       decode json ~f:P.Config.of_json (fun config ->
         { m with config = Some config }, [])
     | Config_saved ->
       decode json ~f:P.Config.of_json (fun config ->
         let count = List.length config.scoped_models in
         ( notice
             { m with config = Some config }
             (sprintf "scoped models saved (%d)" count)
         , [] ))
     | Default_saved ->
       decode json ~f:P.Config.of_json (fun config ->
         let show = Option.value ~default:"?" in
         ( notice
             { m with config = Some config }
             (sprintf
                "default: %s, thinking %s"
                (show config.default_model)
                (show config.default_thinking))
         , [] ))
     | Fallback_shown | Fallback_saved _ ->
       decode json ~f:P.Config.of_json (fun config ->
         let m = { m with config = Some config } in
         notice m (fallback_summary m config), [])
     | Snippets_saved text ->
       decode json ~f:P.Config.of_json (fun config ->
         notice { m with config = Some config } text, [])
     | Default_dir_shown | Default_dir_saved _ ->
       decode json ~f:P.Config.of_json (fun config ->
         notice { m with config = Some config } (default_dir_summary config), [])
     | Models_catalog ->
       decode json ~f:(decode_list ~f:P.Model.of_json) (fun models ->
         { m with models }, [])
     | Models_for_scoped ->
       decode json ~f:(decode_list ~f:P.Model.of_json) (fun models ->
         scoped_models_picker { m with models }, [])
     | Models_for_picker query ->
       decode json ~f:(decode_list ~f:P.Model.of_json) (fun models ->
         model_picker { m with models } ~query, [])
     | Models_for_switch arg ->
       decode json ~f:(decode_list ~f:P.Model.of_json) (fun models ->
         switch_model { m with models } arg)
     | Models_after_login provider ->
       decode json ~f:(decode_list ~f:P.Model.of_json) (fun models ->
         let m = { m with models } in
         match
           List.find models ~f:(fun model ->
             String.equal model.provider provider)
         with
         | Some model ->
           ( notice m (sprintf "model set to %s; /model to change" model.key)
           , [ set_model_command model.key ] )
         | None -> m, [])
     | Jobs_picker ->
       decode json ~f:(decode_list ~f:P.Job_info.of_json) (jobs_picker m)
     | Job_output ->
       decode
         json
         ~f:(fun json -> P.Json.string_field json "text")
         (fun text -> output m ~title:"Job output" (Content.lines text), [])
     | Job_started ->
       decode
         json
         ~f:(fun json -> P.Json.string_field json "job_id")
         (fun id ->
           ( notice
               m
               (sprintf
                  "started job %s; /jobs shows it, and its exit is reported to \
                   the agent"
                  id)
           , [] ))
     | Sessions_picker ->
       decode
         json
         ~f:(decode_list ~f:P.Session_summary.of_json)
         (fun sessions -> sessions_picker m sessions, [])
     | Sessions_cache ->
       decode
         json
         ~f:(decode_list ~f:P.Session_summary.of_json)
         (fun sessions ->
            refresh_autocomplete { m with sessions = Some sessions })
     | Prefs_history ->
       (* Not worth a word when it is not what we expect. *)
       (match P.Prefs.of_reply json with
        | Error _ -> m, []
        | Ok changes ->
          (match
             List.find changes ~f:(fun c -> String.equal c.key history_key)
             |> Option.bind ~f:(fun c -> history_of_json c.value)
           with
           | Some entries -> set_backend_history m entries, []
           | None ->
             (* The first frontend of this user to ask: this machine's. *)
             (match List.take (Editor.history m.editor) history_max with
              | [] -> m, []
              | entries ->
                ( m
                , [ rpc
                      "set_prefs"
                      ~params:
                        (P.Prefs.set_params
                           [ { key = history_key
                             ; value =
                                 Some
                                   (`Array
                                       (List.map entries ~f:(fun s -> `String s)))
                             }
                           ])
                      ~tag:Ignore
                  ] ))))
     | History ->
       decode json ~f:(decode_list ~f:P.Json.to_string_or_error) (fun lines ->
         { m with editor = Editor.set_history m.editor lines }, [])
     | Editor_text ->
       decode json ~f:P.Json.to_string_or_error (fun text ->
         { m with editor = Editor.set_text m.editor text }, [])
     | Dequeued ->
       (match json with
        | `Null -> notice m "nothing queued", []
        | _ ->
          decode
            json
            ~f:(fun j -> P.Json.string_field j "text")
            (fun text ->
               let editor =
                 if Editor.is_empty m.editor
                 then Editor.set_text m.editor text
                 else
                   Editor.set_text
                     m.editor
                     (text ^ "\n\n" ^ Editor.text m.editor)
               in
               { m with editor }, []))
     | Paths_for_autocomplete prefix ->
       decode json ~f:(decode_list ~f:P.Json.to_string_or_error) (fun paths ->
         let m =
           { m with
             known_paths =
               List.fold paths ~init:m.known_paths ~f:(fun acc p ->
                 Set.add acc p)
           }
         in
         match m.autocomplete with
         | Some ac
           when (match Autocomplete.source ac with
                 | Autocomplete.Source.Path -> true
                 | _ -> false)
                && String.equal (Autocomplete.prefix ac) prefix ->
           let items =
             List.map paths ~f:(fun p -> Picker.Item.create ~id:p p)
           in
           { m with autocomplete = Some (Autocomplete.set_items ac items) }, []
         | _ -> m, [])
     | Dirs_for_autocomplete prefix ->
       decode json ~f:(decode_list ~f:P.Json.to_string_or_error) (fun dirs ->
         match m.autocomplete with
         | Some ac
           when (match Autocomplete.source ac with
                 | Autocomplete.Source.Directory _ -> true
                 | _ -> false)
                && String.equal (Autocomplete.prefix ac) prefix ->
           let items = List.map dirs ~f:(fun d -> Picker.Item.create ~id:d d) in
           { m with autocomplete = Some (Autocomplete.set_items ac items) }, []
         | _ -> m, []))
;;

let stderr_tail_limit = 20

let is_backend_command = function
  | Command.Rpc _ -> true
  | _ -> false
;;

let block_backend_rpc m cmds =
  if Model.backend_gone m && List.exists cmds ~f:is_backend_command
  then (
    let why =
      match m.connection with
      | Disconnected _ ->
        "not connected: /signin signs in, /connect switches server"
      | Switching _ -> "not connected yet"
      | Connected | Reconnecting _ -> "backend is gone"
    in
    error m why, List.filter cmds ~f:(Fn.non is_backend_command))
  else m, cmds
;;

let update m (action : Action.t) =
  if m.quitting
  then m, []
  else (
    let m, cmds =
      match action with
      | Start ->
        (match m.connection with
         | Disconnected _ -> m, [ Command.Load_history ]
         | Connected | Reconnecting _ | Switching _ -> m, start_commands)
      (* The next key press retires the notice it acknowledged; the key's
         own notice, if any, replaces it. *)
      | Key key ->
        (match Keymap.lookup key with
         | Some i -> intent { m with notice = None } i
         | None -> m, [])
      | Intent i -> intent { m with notice = None } i
      | Event e ->
        let m =
          match m.connection, e with
          | Switching s, Notice text ->
            { m with
              connection = Switching { s with notices = text :: s.notices }
            }
          | _ -> m
        in
        let m, cmds = event m e in
        m, cmds @ notify m e
      | Protocol_error text ->
        log ~severity:Error m ("protocol error: " ^ text), []
      | Stderr line ->
        let all = m.stderr_tail @ [ line ] in
        let stderr_tail =
          List.drop all (Int.max 0 (List.length all - stderr_tail_limit))
        in
        log ~severity:Debug { m with stderr_tail } ("backend: " ^ line), []
      | Backend_closed ->
        (match m.connection with
         | Connected -> backend_closed m
         | Switching _ | Disconnected _ -> m, []
         | Reconnecting { attempt; session; _ } ->
           (* An attempt got as far as connecting and then lost the transport
              again; its reply (if any) is now stale. *)
           let attempt = Connection.next_attempt attempt in
           schedule_reconnect
             m
             ~attempt
             ~delay_ms:(Connection.delay_ms ~attempt)
             ~session)
      | Reply (Reconnect generation, result) ->
        reconnect_reply m ~generation result
      | Reply (tag, result) -> reply m tag result
      | Tick ->
        ( { m with
            spinner = m.spinner + 1
          ; notice = Option.bind m.notice ~f:Notice.tick
          }
        , [] )
      | Set_home home -> { m with home = Some home }, []
      | Set_utc_offset utc_offset -> { m with utc_offset }, []
      | Focus focused ->
        { m with terminal_focus = (if focused then Focused else Unfocused) }, []
      | Hello { client_id; namespace; user; superuser } ->
        { m with client_id = Some client_id; namespace; user; superuser }, []
      | Server_link link -> { m with server = Some link }, []
      | Relinked { generation; outcome } -> relinked m ~generation outcome
      | Rejected reason -> signed_out m ~reason, []
      | Resize { width; height } -> { m with width; height }, []
    in
    let m, cmds = block_backend_rpc m cmds in
    maybe_open_pending m, cmds)
;;
