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
    | Deleted_session
    | Paths_for_autocomplete of string
    | Set_model_done of string (** the key requested *)
    | Config
    | Config_saved
    | Config_for_confirm of bool
    | Models_catalog
    | Models_for_scoped
    | Compact_done
    | Abort_done
    | Notice_on_success of string
    | History
    | Dequeued
    | Editor_text
    | Reload_messages_notice of string
    | Reconnect of int (** generation; stale replies are ignored *)
  [@@deriving sexp_of, equal]
end

module Command = struct
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * P.Json.t) list
        ; tag : Reply_tag.t
        }
    | List_paths of
        { prefix : string
        ; cwd : string option
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
        }
  [@@deriving sexp_of, equal]

  let max_delay_ms = 10_000

  (* 250ms, 500ms, 1s, ... capped at 10s. *)
  let delay_ms ~attempt =
    Int.min max_delay_ms (250 * Int.pow 2 (Int.max 0 (attempt - 1)))
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
    | Set_client_id of string
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
    ; transcript : Transcript.t
    ; agents : Agent_view.t list
    ; focus : [ `Main | `Agent of string ]
    ; editor : Editor.t
    ; mode : Mode.t
    ; autocomplete : Autocomplete.t option
    ; sessions : P.Session_summary.t list option
    ; known_paths : String.Set.t
    ; queued : Queue_counts.t
    ; queued_texts : string list
    ; login_lines : string list
    ; viewport : Viewport.t
    ; pending_quit : bool
    ; spinner : int
    ; verbosity : Verbosity.t
    ; config : P.Config.t option
    ; home : string option
    ; client_id : string option (** ours, from [hello] *)
    ; stderr_tail : string list
    ; pending_confirms : (string * string * string) list
    ; connection : Connection.t
    ; reconnect_generation : int
    ; width : int
    ; height : int
    ; quitting : bool
    }
  [@@deriving sexp_of]

  let running t =
    Option.value_map t.state ~default:false ~f:(fun s -> s.running)
  ;;

  let backend_gone t =
    match t.connection with
    | Connected -> false
    | Reconnecting _ -> true
  ;;
end

open Model

let rpc ?(params = []) ?(tag = Reply_tag.Show_error) method_ =
  Command.Rpc { method_; params; tag }
;;

let str = P.Json.str
let transcript_width m = Int.max 1 m.width
let transcript_height m = Int.max 3 m.height

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

let editor_row_count m =
  let inner = Int.max 1 (m.width - 2) in
  List.sum (module Int) (Editor.lines m.editor) ~f:(fun line ->
    let rec count text acc =
      let _, rest = Text_width.take text ~width:inner in
      if String.is_empty rest then acc + 1 else count rest (acc + 1)
    in
    count line 0)
;;

let transcript_rows m =
  Int.max 0 (transcript_height m - (editor_row_count m + 2))
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

let notice ?severity m text =
  with_transcript m ~f:(fun t -> Transcript.notice ?severity t text)
;;

let error m text = notice ~severity:Error m text
let warn m text = notice ~severity:Warn m text

let confirm_question ~name ~summary =
  match name with
  | "bash" -> sprintf "Run bash: %s? (y/n)" summary
  | "write" -> sprintf "Write %s? (y/n)" summary
  | "edit" -> sprintf "Edit %s? (y/n)" summary
  | name -> sprintf "Run %s: %s? (y/n)" name summary
;;

let confirm_tool m ~call_id ~name ~summary =
  { m with
    mode =
      Confirm
        { question = confirm_question ~name ~summary
        ; action = Tool_confirm { call_id; name }
        }
  ; autocomplete = None
  }
;;

let maybe_open_pending m =
  if Model.backend_gone m
  then m
  else (
    match m.mode, m.pending_confirms with
    | Editing, (call_id, name, summary) :: rest ->
      { (confirm_tool m ~call_id ~name ~summary) with pending_confirms = rest }
    | _ -> m)
;;

let verbosity_notice = function
  | Verbosity.Quiet ->
    "view: quiet — intermediate output and thinking are hidden"
  | Normal -> "view: normal — tool output is summarised"
  | Verbose -> "view: verbose — everything is shown"
;;

let block m content =
  with_transcript m ~f:(fun t -> Transcript.add t (Block content))
;;

let follow m = { m with viewport = Viewport.Follow }

let set_verbosity m verbosity =
  follow (notice { m with verbosity } (verbosity_notice verbosity))
;;

let init =
  { state = None
  ; models = []
  ; auth = []
  ; transcript = Transcript.empty
  ; agents = []
  ; focus = `Main
  ; editor = Editor.empty
  ; mode = Editing
  ; autocomplete = None
  ; sessions = None
  ; known_paths = String.Set.empty
  ; queued = Queue_counts.zero
  ; queued_texts = []
  ; login_lines = []
  ; viewport = Viewport.Follow
  ; pending_quit = false
  ; spinner = 0
  ; verbosity = Verbosity.Normal
  ; config = None
  ; home = None
  ; client_id = None
  ; stderr_tail = []
  ; pending_confirms = []
  ; connection = Connected
  ; reconnect_generation = 0
  ; width = 80
  ; height = 24
  ; quitting = false
  }
;;

let start_commands =
  [ rpc "get_state" ~tag:Initial_state
  ; rpc "get_messages" ~tag:Initial_messages
  ; rpc "auth_status" ~tag:Auth_refresh
  ; rpc "get_config" ~tag:Config
  ; rpc "list_models" ~tag:Models_catalog
  ; Command.Load_history
  ]
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

let format_price (c : P.Model.Cost.t) = sprintf "$%g/$%g per M" c.input c.output

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

let login_picker m (statuses : P.Auth_status.t list) =
  let items =
    List.concat_map statuses ~f:(fun s ->
      List.map s.methods ~f:(fun meth ->
        let configured =
          match s.configured with
          | Some c when String.equal c.method_ meth.method_ ->
            sprintf "logged in via %s" c.source
          | _ -> ""
        in
        Picker.Item.create
          ~id:(s.provider ^ " " ^ meth.method_)
          ~detail:(String.strip (meth.label ^ "  " ^ configured))
          ~marked:(not (String.is_empty configured))
          (s.name ^ " (" ^ meth.method_ ^ ")")))
  in
  open_picker m Login (Picker.create ~title:"Log in to" items)
;;

let logout_picker m (statuses : P.Auth_status.t list) =
  let items =
    List.filter_map statuses ~f:(fun s ->
      Option.map s.configured ~f:(fun c ->
        Picker.Item.create
          ~id:s.provider
          ~detail:(sprintf "%s via %s" c.method_ c.source)
          s.name))
  in
  if List.is_empty items
  then notice m "no provider is logged in"
  else open_picker m Logout (Picker.create ~title:"Log out of" items)
;;

let session_picker_items m (sessions : P.Session_summary.t list) =
  let current = Option.map m.state ~f:(fun s -> s.session_path) in
  List.map sessions ~f:(fun s ->
    let name = Option.value s.name ~default:"(unnamed)" in
    let date = String.prefix (Option.value s.updated_at ~default:"") 16 in
    let first =
      Option.value_map s.first_prompt ~default:"(empty)" ~f:(fun p ->
        Text_width.truncate
          (String.concat ~sep:" " (String.split_lines p))
          ~width:60)
    in
    Picker.Item.create
      ~id:s.path
      ~detail:s.cwd
      ~search:(name ^ " " ^ first ^ " " ^ s.cwd ^ " " ^ date)
      ~marked:(Option.value_map current ~default:false ~f:(String.equal s.path))
      (sprintf
         "%s%s ∣ %s ∣ %d msgs ∣ %s"
         (if s.running then "▶ " else if s.live then "● " else "")
         name
         date
         s.message_count
         first))
;;

let sessions_picker
  ?(named_only = false)
  m
  (sessions : P.Session_summary.t list)
  =
  let shown =
    if named_only
    then List.filter sessions ~f:(fun s -> Option.is_some s.name)
    else sessions
  in
  let items = session_picker_items m shown in
  if List.is_empty items
  then notice m "no saved sessions"
  else (
    let title = if named_only then "Sessions (named)" else "Sessions" in
    open_picker
      m
      (Sessions { named_only; sessions })
      (Picker.create ~title items))
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

let message_first_line (m : P.Message.t) =
  match m with
  | P.Message.User text -> first_line text
  | P.Message.Assistant { content; _ } ->
    (match
       List.find_map content ~f:(function
         | P.Content.Text text -> Some text
         | _ -> None)
     with
     | Some text -> first_line text
     | None -> "(assistant)")
  | P.Message.Tool_result { tool_name; text; _ } ->
    let line = first_line text in
    if String.is_empty line then tool_name else line
;;

let user_entries entries =
  List.filter_map entries ~f:(fun (entry : P.Entry.t) ->
    match entry.kind with
    | P.Entry.Kind.Message (P.Message.User text) -> Some (entry, text)
    | _ -> None)
;;

let entry_picker m kind ~title entries =
  let users = user_entries entries in
  let count = List.length users in
  let items =
    List.mapi users ~f:(fun i ((entry : P.Entry.t), text) ->
      Picker.Item.create
        ~id:entry.id
        ~detail:(sprintf "#%d" (i + 1))
        ~marked:(i = count - 1)
        (first_line text))
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
  let entries =
    List.filter entries ~f:(fun (e : P.Entry.t) ->
      match e.kind with
      | P.Entry.Kind.Message _ -> true
      | _ -> false)
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
    match head with
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
      | P.Entry.Kind.Cwd _
      | P.Entry.Kind.System_prompt -> "·"
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
          ~detail:(sprintf "%s  %s" (agent_status_text a) a.model)
          ~marked:
            (match m.focus with
             | `Agent id -> String.equal id a.id
             | `Main -> false)
          a.task)
    in
    open_picker m Agents (Picker.create ~title:"Subagents" items), [])
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

let set_host_command ~host ~cwd =
  rpc
    "set_active_host"
    ~params:[ "host", str host; "cwd", str cwd ]
    ~tag:(Notice_on_success "tool host switched")
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

let format_auth (statuses : P.Auth_status.t list) : Content.t =
  let width =
    List.fold statuses ~init:0 ~f:(fun acc s ->
      Int.max acc (String.length s.provider))
  in
  List.map statuses ~f:(fun s ->
    let methods =
      String.concat
        ~sep:", "
        (List.map s.methods ~f:(fun m -> m.method_ ^ " (" ^ m.label ^ ")"))
    in
    let state : Content.Line.t =
      match s.configured with
      | Some c ->
        [ { text = "logged in"; style = Style.fg Green }
        ; { text = " via " ^ c.source; style = Style.plain }
        ]
      | None -> [ { text = "not configured"; style = Style.fg Gray } ]
    in
    [ { Content.Span.text = Text_width.pad_right s.provider ~width ^ "  "
      ; style = Style.bold Style.plain
      }
    ]
    @ state
    @ [ { text = "  [" ^ methods ^ "]"; style = Style.dim Style.plain } ])
;;

(* ---- reconnection ----------------------------------------------------- *)

let schedule_reconnect m ~attempt ~delay_ms =
  let generation = m.reconnect_generation + 1 in
  ( { m with
      connection = Reconnecting { attempt; generation; delay_ms }
    ; reconnect_generation = generation
    }
  , [ Command.Reconnect
        { generation
        ; delay_ms
        ; session = Option.map m.state ~f:(fun s -> s.session_path)
        }
    ] )
;;

let backend_closed m =
  let m =
    { m with
      pending_confirms = []
    ; state = Option.map m.state ~f:(fun s -> { s with running = false })
    }
  in
  let m =
    error
      m
      "backend connection lost; reconnecting (/retry-backend-connection to \
       retry now, Ctrl+C quits)"
  in
  let m =
    if List.is_empty m.stderr_tail
    then m
    else
      block
        m
        (Content.lines
           ~style:(Style.dim Style.plain)
           (String.concat ~sep:"\n" m.stderr_tail))
  in
  schedule_reconnect { m with stderr_tail = [] } ~attempt:1 ~delay_ms:0
;;

let retry_backend_connection m =
  match m.connection with
  | Connected -> notice m "backend is connected", []
  | Reconnecting { attempt; _ } ->
    schedule_reconnect (notice m "reconnecting…") ~attempt ~delay_ms:0
;;

let reconnect_reply m ~generation result =
  match m.connection with
  | Reconnecting { generation = current; attempt; _ } when generation = current
    ->
    (match result with
     | Error e ->
       let attempt = attempt + 1 in
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
     | Ok json ->
       let client_id =
         match P.Json.field json "client_id" with
         | Some (`String id) -> Some id
         | _ -> m.client_id
       in
       let m =
         { m with
           connection = Connected
         ; client_id
         ; agents = []
         ; focus = `Main
         ; transcript = Transcript.clear m.transcript
         }
       in
       ( follow (notice m "reconnected to the backend")
       , [ rpc "get_state" ~tag:Initial_state
         ; rpc "get_messages" ~tag:Initial_messages
         ; rpc "auth_status" ~tag:Auth_refresh
         ; rpc "get_config" ~tag:Config
         ; rpc "list_models" ~tag:Models_catalog
         ] ))
  | Connected | Reconnecting _ -> m, []
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
    let m =
      error
        m
        (sprintf
           "unknown model %S; did you mean: %s"
           arg
           (String.concat ~sep:", " (List.map suggestions ~f:(fun s -> s.name))))
    in
    model_picker m ~query:arg, []
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

let config_with_confirm config enabled =
  { config with P.Config.confirm_tools = enabled }
;;

let set_config_command config =
  let label =
    sprintf
      "tool confirmation %s"
      (if config.P.Config.confirm_tools then "on" else "off")
  in
  rpc
    "set_config"
    ~params:[ "config", P.Config.to_json config ]
    ~tag:(Notice_on_success label)
;;

let set_confirm m enabled =
  match m.config with
  | Some config ->
    let config = config_with_confirm config enabled in
    { m with config = Some config }, [ set_config_command config ]
  | None -> m, [ rpc "get_config" ~tag:(Config_for_confirm enabled) ]
;;

let run_command m (cmd : Commands.Parsed.t) =
  match cmd.name, cmd.args with
  | "", _ -> m, []
  | "help", [] ->
    let heading text : Content.Line.t =
      [ { text; style = Style.bold (Style.fg Cyan) } ]
    in
    ( block
        m
        ((heading "Commands" :: Commands.help)
         @ ([] :: heading "Keys" :: Keymap.help))
    , [] )
  | "help", name :: _ ->
    (match Commands.find name with
     | Some spec ->
       let usage = String.strip ("/" ^ spec.name ^ " " ^ spec.args) in
       ( block
           m
           [ [ { Content.Span.text = usage; style = Style.bold Style.plain }
             ; { text = "  " ^ spec.help; style = Style.plain }
             ]
           ]
       , [] )
     | None ->
       let hint =
         match Commands.closest name with
         | Some c -> sprintf "; did you mean /%s?" c.name
         | None -> ""
       in
       error m (sprintf "unknown command /%s%s" name hint), [])
  | "hotkeys", _ ->
    let heading text : Content.Line.t =
      [ { text; style = Style.bold (Style.fg Cyan) } ]
    in
    block m (heading "Keys" :: Keymap.help), []
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
    (match m.config with
     | Some config ->
       ( notice
           m
           (sprintf
              "tool confirmation %s"
              (if config.P.Config.confirm_tools then "on" else "off"))
       , [] )
     | None ->
       notice m "tool confirmation: unknown", [ rpc "get_config" ~tag:Config ])
  | "confirm", ("on" | "true") :: _ -> set_confirm m true
  | "confirm", ("off" | "false") :: _ -> set_confirm m false
  | "confirm", other :: _ ->
    error m (sprintf "unknown argument %S; use on or off" other), []
  | "auth", _ -> m, [ rpc "auth_status" ~tag:Auth_show ]
  | "login", [] -> m, [ rpc "auth_status" ~tag:Auth_login_picker ]
  | "login", provider :: rest ->
    let params =
      ("provider", str provider)
      :: Option.to_list
           (Option.map (List.hd rest) ~f:(fun meth -> "method", str meth))
    in
    m, [ rpc "login" ~params ]
  | "logout", [] -> m, [ rpc "auth_status" ~tag:Auth_logout_picker ]
  | "logout", provider :: _ ->
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
  | "compact", _ -> notice m "compacting…", [ rpc "compact" ~tag:Compact_done ]
  | "new", _ -> m, [ rpc "new_session" ~tag:(Notice_on_success "new session") ]
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
  | "agents", _ -> agents_picker m
  | "host", [] -> hosts_picker m
  | "host", _ -> switch_host m cmd.rest
  | "sessions", _ | "switch", [] ->
    m, [ rpc "list_sessions" ~tag:Sessions_picker ]
  | "switch", _ ->
    ( m
    , [ rpc
          "switch_session"
          ~params:[ "path", str cmd.rest ]
          ~tag:Reload_messages
      ] )
  | "cd", [] ->
    ( { m with
        mode = Text_prompt { question = "Change directory to"; action = Cd }
      ; editor = Editor.clear m.editor
      }
    , [] )
  | "cd", _ ->
    ( m
    , [ rpc
          "set_cwd"
          ~params:[ "path", str cmd.rest ]
          ~tag:(Notice_on_success "cwd changed")
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
  | "retry-backend-connection", _ -> retry_backend_connection m
  | "state", _ ->
    let text =
      Option.value_map m.state ~default:"not connected" ~f:(fun s ->
        Sexp.to_string_hum (P.State.sexp_of_t s))
    in
    block m (Content.lines ~style:(Style.fg Gray) text), []
  | "clear", _ ->
    follow { m with transcript = Transcript.clear m.transcript }, []
  | "quit", _ | "exit", _ -> { m with quitting = true }, [ Quit ]
  | name, _ ->
    let hint =
      match Commands.closest name with
      | Some c -> sprintf "; did you mean /%s?" c.name
      | None -> ""
    in
    ( error
        m
        (sprintf "unknown command /%s%s (Tab or / lists commands)" name hint)
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
  if Model.running m
  then warn m "wait for the current turn", []
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

let submit m =
  let text, editor = Editor.submit m.editor in
  let m = follow { m with editor; autocomplete = None } in
  if String.is_empty (String.strip text)
  then m, []
  else (
    match Commands.parse text with
    | Some cmd -> run_command m cmd
    | None ->
      let m, cmds =
        if String.is_prefix text ~prefix:"!"
        then shell_command m text
        else if Model.running m
        then
          ( { m with queued_texts = m.queued_texts @ [ text ] }
          , [ rpc "steer" ~params:(user_params m text) ] )
        else
          ( { m with agents = []; focus = `Main }
          , [ rpc "prompt" ~params:(user_params m text) ] )
      in
      m, cmds @ [ Command.Append_history text ])
;;

let queue_follow_up m =
  if not (Model.running m)
  then submit m
  else (
    let text, editor = Editor.submit m.editor in
    if String.is_empty (String.strip text)
    then m, []
    else (
      let m = follow { m with editor; autocomplete = None } in
      let m = { m with queued_texts = m.queued_texts @ [ text ] } in
      ( m
      , [ rpc "follow_up" ~params:(user_params m text)
        ; Command.Append_history text
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

let refresh_autocomplete m =
  let line, col = current_line m in
  let line_index = (Editor.position m.editor).line in
  match
    Autocomplete.compute
      ~line
      ~col
      ~line_index
      ~models:m.models
      ~auth:m.auth
      ~sessions:m.sessions
      ~logged_in:(logged_in m)
  with
  | None -> { m with autocomplete = None }, []
  | Some ac ->
    (match Autocomplete.source ac with
     | Autocomplete.Source.Path ->
       let same =
         match m.autocomplete with
         | Some prev ->
           (match Autocomplete.source prev with
            | Autocomplete.Source.Path ->
              String.equal (Autocomplete.prefix prev) (Autocomplete.prefix ac)
            | _ -> false)
         | None -> false
       in
       if same
       then m, []
       else (
         let prefix = Autocomplete.prefix ac in
         ( { m with autocomplete = Some ac }
         , [ Command.List_paths
               { prefix
               ; cwd = Option.map m.state ~f:(fun s -> s.cwd)
               ; tag = Paths_for_autocomplete prefix
               }
           ] ))
     | Autocomplete.Source.Argument spec
       when match spec.argument with
            | Some Commands.Argument.Sessions -> Option.is_none m.sessions
            | _ -> false ->
       ( { m with autocomplete = Some ac }
       , [ rpc "list_sessions" ~tag:Sessions_cache ] )
     | _ -> { m with autocomplete = Some ac }, [])
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
       let text = Autocomplete.accept ac ~editor_text:(Editor.text m.editor) in
       let m =
         { m with editor = Editor.set_text m.editor text; autocomplete = None }
       in
       let m, cmds =
         match Autocomplete.source ac, submit_now with
         | Autocomplete.Source.Command, true ->
           (match Commands.find item.id with
            | Some spec when Option.is_some spec.argument ->
              refresh_autocomplete m
            | _ -> submit m)
         | Autocomplete.Source.Command, false -> m, []
         | (Autocomplete.Source.Argument _ | Path), true -> submit m
         | (Autocomplete.Source.Argument _ | Path), false -> m, []
       in
       Some (m, cmds))
;;

(* Denies pending tool confirmations and aborts the running turn. *)
let abort m =
  let denies =
    List.map m.pending_confirms ~f:(fun (call_id, _, _) ->
      rpc
        "tool_confirm_respond"
        ~params:[ "call_id", str call_id; "allow", P.Json.bool false ]
        ~tag:Ignore)
  in
  { m with pending_confirms = [] }, denies @ [ rpc "abort" ~tag:Abort_done ]
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
  else warn { m with pending_quit = true } "press Ctrl+C again to quit", []
;;

let page_size m = Int.max 1 (m.height / 2)

(* Ten rows, fewer on short screens so the separator and status line stay. *)
let picker_rows ~height = Int.max 3 (Int.min 10 (height - 5))
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
         | Viewport.Follow -> m, []))
  | Interrupt -> interrupt m
  | Force_quit -> { m with quitting = true }, [ Quit ]
  | Kill_to_end -> ed Editor.kill_to_end
  | Kill_to_start -> ed Editor.kill_to_start
  | Kill_word -> ed Editor.kill_word
  | Cycle_verbosity -> set_verbosity m (Verbosity.next m.verbosity), []
  | Next_model -> cycle_model m ~step:1
  | Prev_model -> cycle_model m ~step:(-1)
  | Next_thinking -> cycle_thinking m
  | Next_agent -> cycle_focus m, []
  | Focus_agent n -> focus_agent m n, []
  | Queue_follow_up -> queue_follow_up m
  | Dequeue -> m, [ rpc "dequeue" ~tag:Dequeued ]
  | Copy_last -> copy_last m
  | Suspend -> m, [ Command.Suspend ]
  | Path_complete -> path_complete m
  | Edit_externally -> m, [ Command.Edit_externally (Editor.text m.editor) ]
  | Model_picker ->
    if List.is_empty m.models
    then m, [ rpc "list_models" ~tag:(Models_for_picker "") ]
    else model_picker m ~query:"", []
  | Picker_toggle_filter -> m, []
  | Search -> open_search m, []
  | Prev_user_message -> jump_user_message m ~direction:`Prev
  | Next_user_message -> jump_user_message m ~direction:`Next
;;

(* Autocomplete is a sub-state of editing: while it is open it owns a few keys,
   everything else edits the buffer and then recomputes the completion. *)
let editing m (intent : Intent.t) =
  match intent with
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
        | Next_agent
        | Focus_agent _
        | Queue_follow_up
        | Dequeue
        | Copy_last
        | Suspend
        | Path_complete
        | Edit_externally
        | Model_picker
        | Picker_toggle_filter
        | Prev_user_message
        | Next_user_message
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
  | Verbosity ->
    let verbosity =
      Option.value
        (List.find Verbosity.all ~f:(fun v ->
           String.equal (Verbosity.name v) item.id))
        ~default:Verbosity.Normal
    in
    set_verbosity m verbosity, []
  | Login ->
    (match String.split item.id ~on:' ' with
     | [ provider; meth ] ->
       ( m
       , [ rpc "login" ~params:[ "provider", str provider; "method", str meth ]
         ] )
     | _ -> error m "bad login selection", [])
  | Logout ->
    ( { m with
        mode =
          Confirm
            { question =
                sprintf "Log out of %s and delete its credential? (y/n)" item.id
            ; action = Logout item.id
            }
      }
    , [] )
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
     | Some { kind = P.Entry.Kind.Message (P.Message.User text); _ } ->
       ( { m with editor = Editor.set_text m.editor text }
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
  | Hosts ->
    (match
       Option.bind m.state ~f:(fun s ->
         List.find s.hosts ~f:(fun h -> String.equal h.id item.id))
     with
     | Some host -> host_cwd_prompt m host, []
     | None -> error m (sprintf "unknown host %S" item.id), [])
  | Auth_select id ->
    m, [ rpc "auth_respond" ~params:[ "id", str id; "value", str item.id ] ]
;;

let save_scoped_models m picker =
  let checked = Picker.checked picker in
  let scoped_models =
    List.filter_map m.models ~f:(fun (model : P.Model.t) ->
      Option.some_if (Set.mem checked model.key) model.key)
  in
  let config =
    match m.config with
    | Some config -> { config with scoped_models }
    | None -> { P.Config.scoped_models; confirm_tools = false }
  in
  ( { m with mode = Editing }
  , [ rpc
        "set_config"
        ~params:[ "config", P.Config.to_json config ]
        ~tag:Config_saved
    ] )
;;

let picker m (kind : Mode.Picker_kind.t) picker (intent : Intent.t) =
  match intent with
  | Picker_toggle_filter ->
    (match kind with
     | Sessions { named_only; sessions } ->
       let named_only = not named_only in
       let shown =
         if named_only
         then List.filter sessions ~f:(fun s -> Option.is_some s.name)
         else sessions
       in
       let items = session_picker_items m shown in
       let title = if named_only then "Sessions (named)" else "Sessions" in
       ( { m with
           mode =
             Picker
               { kind = Sessions { named_only; sessions }
               ; picker =
                   Picker.create ~query:(Picker.query picker) ~title items
               }
         }
       , [] )
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
     | Sessions { sessions; _ } ->
       (match Picker.selected_item picker with
        | None -> m, []
        | Some item ->
          (match
             List.find sessions ~f:(fun s -> String.equal s.path item.id)
           with
           | None -> m, []
           | Some session ->
             let name =
               Option.value
                 session.name
                 ~default:
                   (String.prefix
                      (Option.value session.updated_at ~default:"")
                      16)
             in
             ( { m with
                 mode =
                   Confirm
                     { question = sprintf "Delete session %s? (y/n)" name
                     ; action = Delete_session session.path
                     }
               }
             , [] )))
     | _ -> { m with quitting = true }, [ Command.Quit ])
  | _ ->
    (match
       Picker.handle
         picker
         (if Intent.equal intent Interrupt then Cancel else intent)
         ~page:(picker_rows ~height:m.height)
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
  let secret =
    match prompt with
    | Secret _ -> true
    | Manual_code _ | Select _ -> false
  in
  match intent with
  | Submit ->
    let value, editor = Editor.submit ~secret m.editor in
    let value = String.strip value in
    if String.is_empty value
    then m, []
    else
      ( { m with editor; mode = Editing }
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
  | Next_agent
  | Focus_agent _
  | Queue_follow_up
  | Dequeue
  | Copy_last
  | Suspend
  | Path_complete
  | Edit_externally
  | Model_picker
  | Picker_toggle_filter
  | Search
  | Prev_user_message
  | Next_user_message
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
    let m = { m with mode = Editing; editor = Editor.clear m.editor } in
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
      if String.is_empty text
      then m, []
      else
        ( m
        , [ rpc
              "set_cwd"
              ~params:[ "path", str text ]
              ~tag:(Notice_on_success "cwd changed")
          ] )
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
      else m, [ set_host_command ~host ~cwd:text ]
  in
  match intent with
  | Submit ->
    let text, _ = Editor.submit m.editor in
    submit (String.strip text)
  | Cancel | Interrupt ->
    { m with editor = Editor.clear m.editor; mode = Editing }, []
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
  | Undo ->
    let m', cmds = editing_intent m intent in
    { m' with mode = m.mode }, cmds
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
  | Next_agent
  | Focus_agent _
  | Queue_follow_up
  | Dequeue
  | Copy_last
  | Suspend
  | Path_complete
  | Edit_externally
  | Model_picker
  | Picker_toggle_filter
  | Search
  | Prev_user_message
  | Next_user_message -> m, []
;;

(* ---- confirm mode ----------------------------------------------------- *)

let confirm m ~(action : Mode.Confirm_action.t) (intent : Intent.t) =
  let yes m =
    match action with
    | Logout provider ->
      m, [ rpc "logout" ~params:[ "provider", str provider ] ]
    | Rewind id ->
      m, [ rpc "rewind" ~params:[ "to", str id ] ~tag:Reload_messages ]
    | Delete_session path ->
      ( m
      , [ rpc "delete_session" ~params:[ "path", str path ] ~tag:Deleted_session
        ] )
    | Tool_confirm { call_id; _ } ->
      ( m
      , [ rpc
            "tool_confirm_respond"
            ~params:[ "call_id", str call_id; "allow", P.Json.bool true ]
            ~tag:Ignore
        ] )
  in
  let no m =
    match action with
    | Tool_confirm { call_id; name } ->
      ( notice m (sprintf "denied %s" name)
      , [ rpc
            "tool_confirm_respond"
            ~params:[ "call_id", str call_id; "allow", P.Json.bool false ]
            ~tag:Ignore
        ] )
    | Logout _ | Rewind _ | Delete_session _ -> notice m "cancelled", []
  in
  match intent with
  | Insert ("y" | "Y") | Submit -> yes { m with mode = Editing }
  | Insert ("n" | "N") | Cancel | Interrupt -> no { m with mode = Editing }
  | Force_quit -> { m with quitting = true }, [ Quit ]
  | _ -> m, []
;;

let intent m (intent : Intent.t) =
  let m =
    if Intent.equal intent Interrupt then m else { m with pending_quit = false }
  in
  match m.mode with
  | Editing -> editing m intent
  | Picker { kind; picker = p } -> picker m kind p intent
  | Login_prompt { id; prompt } -> login_prompt m ~id ~prompt intent
  | Text_prompt { action; _ } -> text_prompt m ~action intent
  | Confirm { action; _ } -> confirm m ~action intent
  | Search _ -> search m intent
;;

(* ---- backend events --------------------------------------------------- *)

let auth_event m (e : P.Auth_event.t) =
  match e with
  | Auth_url { url; instructions } ->
    let lines = [ "Open this URL to log in:"; "  " ^ url; instructions ] in
    { m with login_lines = m.login_lines @ lines }, [ Command.Open_browser url ]
  | Prompt { id; prompt } ->
    if Mode.is_dialog m.mode
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
      | Secret _ | Manual_code _ ->
        let lines =
          P.Auth_event.Prompt.message prompt
          ::
          (match prompt with
           | Manual_code { placeholder; _ } ->
             [ "e.g. " ^ placeholder ^ "?code=..." ]
           | _ -> [])
        in
        ( { m with
            mode = Login_prompt { id; prompt }
          ; editor = Editor.clear m.editor
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
;;

let event m (e : P.Event.t) =
  let m = with_transcript m ~f:(fun t -> Transcript.apply t e) in
  let m = update_agents m ~f:(fun agents -> Agent_view.apply_all agents e) in
  match e with
  | State state -> { m with state = Some state }, []
  | Queue_update { steer; follow_up } ->
    let queued_texts =
      if steer = 0 && follow_up = 0 then [] else m.queued_texts
    in
    { m with queued = { Queue_counts.steer; follow_up }; queued_texts }, []
  | Config_changed config -> { m with config = Some config }, []
  | Auth a -> auth_event m a
  | Tool_confirm { call_id; name; summary } ->
    ( { m with
        pending_confirms = m.pending_confirms @ [ call_id, name, summary ]
      }
    , [] )
  | Agent_start
  | Agent_end _
  | Turn_start
  | Turn_end _
  | Message_start _
  | Message_update _
  | Message_end _
  | Tool_start _
  | Tool_output _
  | Tool_end _
  | Compacted _
  | Notice _
  | Subagent_start _
  | Subagent _
  | Subagent_end _
  | Tool_exec _
  | Tool_exec_cancel _ -> m, []
;;

(* ---- rpc replies ------------------------------------------------------ *)

let reply m (tag : Reply_tag.t) (result : (P.Json.t, string) Result.t) =
  let decode json ~f k =
    match f json with
    | Ok v -> k v
    | Error e -> error m ("protocol error: " ^ Error.to_string_hum e), []
  in
  match result with
  | Error e ->
    (match tag with
     | Ignore -> m, []
     | Set_model_done _ ->
       (* The backend formats "did you mean"; the picker helps recover. *)
       model_picker (error m e) ~query:"", []
     | _ -> error m e, [])
  | Ok json ->
    (match tag with
     | Ignore | Show_error | Reconnect _ -> m, []
     | Notice_on_success text -> notice m text, []
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
           let m = { m with editor = Editor.set_text m.editor text } in
           ( notice m (sprintf "restored %d queued %s to the editor" count noun)
           , [] ))
     | Initial_state ->
       decode json ~f:P.State.of_json (fun state ->
         let m = { m with state = Some state } in
         let m =
           notice
             m
             (sprintf
                "session %s in %s. /help for commands, Esc aborts, Ctrl+C \
                 twice quits."
                state.session_id
                state.cwd)
         in
         m, [])
     | Initial_messages ->
       decode json ~f:(decode_list ~f:P.Message.of_json) (fun messages ->
         let transcript =
           List.fold messages ~init:m.transcript ~f:Transcript.add_message
         in
         follow { m with transcript }, [])
     | Reload_messages | Reload_messages_notice _ ->
       let m =
         { m with
           agents = []
         ; focus = `Main
         ; transcript = Transcript.clear m.transcript
         }
       in
       let m =
         match tag with
         | Reload_messages_notice text -> notice m text
         | _ -> m
       in
       ( follow m
       , [ rpc "get_messages" ~tag:Initial_messages
         ; rpc "get_state" ~tag:Initial_state
         ] )
     | Session_stats ->
       decode json ~f:P.Session_stats.of_json (fun stats ->
         block m (format_stats stats), [])
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
     | Deleted_session ->
       notice m "session deleted", [ rpc "list_sessions" ~tag:Sessions_picker ]
     | Auth_refresh ->
       decode json ~f:(decode_list ~f:P.Auth_status.of_json) (fun auth ->
         { m with auth }, [])
     | Auth_show ->
       decode json ~f:(decode_list ~f:P.Auth_status.of_json) (fun auth ->
         block { m with auth } (format_auth auth), [])
     | Auth_login_picker ->
       decode json ~f:(decode_list ~f:P.Auth_status.of_json) (fun auth ->
         login_picker { m with auth } auth, [])
     | Auth_logout_picker ->
       decode json ~f:(decode_list ~f:P.Auth_status.of_json) (fun auth ->
         logout_picker { m with auth } auth, [])
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
     | Config_for_confirm enabled ->
       decode json ~f:P.Config.of_json (fun config ->
         let config = config_with_confirm config enabled in
         { m with config = Some config }, [ set_config_command config ])
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
                  Editor.set_text m.editor (text ^ "\n\n" ^ Editor.text m.editor)
              in
              let queued_texts =
                List.drop_last m.queued_texts |> Option.value ~default:[]
              in
              { m with editor; queued_texts }, []))
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
         | _ -> m, []))
;;

let stderr_tail_limit = 20

let is_backend_command = function
  | Command.Rpc _ | Command.List_paths _ -> true
  | _ -> false
;;

let block_backend_rpc m cmds =
  if Model.backend_gone m && List.exists cmds ~f:is_backend_command
  then
    error m "backend is gone", List.filter cmds ~f:(Fn.non is_backend_command)
  else m, cmds
;;

let update m (action : Action.t) =
  if m.quitting
  then m, []
  else (
    let m, cmds =
      match action with
      | Start -> m, start_commands
      | Key key ->
        (match Keymap.lookup key with
         | Some i -> intent m i
         | None -> m, [])
      | Intent i -> intent m i
      | Event e -> event m e
      | Protocol_error text -> error m ("protocol error: " ^ text), []
      | Stderr line ->
        let all = m.stderr_tail @ [ line ] in
        let stderr_tail =
          List.drop all (Int.max 0 (List.length all - stderr_tail_limit))
        in
        notice ~severity:Debug { m with stderr_tail } ("backend: " ^ line), []
      | Backend_closed ->
        (match m.connection with
         | Connected -> backend_closed m
         | Reconnecting { attempt; _ } ->
           (* An attempt got as far as connecting and then lost the transport
              again; its reply (if any) is now stale. *)
           let attempt = attempt + 1 in
           schedule_reconnect
             m
             ~attempt
             ~delay_ms:(Connection.delay_ms ~attempt))
      | Reply (Reconnect generation, result) ->
        reconnect_reply m ~generation result
      | Reply (tag, result) -> reply m tag result
      | Tick -> { m with spinner = m.spinner + 1 }, []
      | Set_home home -> { m with home = Some home }, []
      | Set_client_id id -> { m with client_id = Some id }, []
      | Resize { width; height } -> { m with width; height }, []
    in
    let m, cmds = block_backend_rpc m cmds in
    maybe_open_pending m, cmds)
;;
