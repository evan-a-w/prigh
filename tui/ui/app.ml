open! Core
module P = Prigh_protocol

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
    | Paths_for_autocomplete of string
    | Set_model_done
    | Compact_done
    | Abort_done
    | Notice_on_success of string
    | History
    | Dequeued
    | Editor_text
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
        ; tag : Reply_tag.t
        }
    | Open_browser of string
    | Load_history
    | Append_history of string
    | Copy_to_clipboard of string
    | Suspend
    | Edit_externally of string
    | Quit
  [@@deriving sexp_of, equal]
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
    ; viewport : Viewport.t
    ; pending_quit : bool
    ; spinner : int
    ; verbosity : Verbosity.t
    ; width : int
    ; height : int
    ; quitting : bool
    }
  [@@deriving sexp_of]

  let running t =
    Option.value_map t.state ~default:false ~f:(fun s -> s.running)
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
  ; viewport = Viewport.Follow
  ; pending_quit = false
  ; spinner = 0
  ; verbosity = Verbosity.Normal
  ; width = 80
  ; height = 24
  ; quitting = false
  }
;;

let start_commands =
  [ rpc "get_state" ~tag:Initial_state
  ; rpc "get_messages" ~tag:Initial_messages
  ; rpc "auth_status" ~tag:Auth_refresh
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

(* ---- pickers ---------------------------------------------------------- *)

let open_picker m kind picker =
  if Mode.is_dialog m.mode
  then warn m "close the current dialog first (Esc)"
  else { m with mode = Picker { kind; picker }; pending_quit = false }
;;

let format_price (c : P.Model.Cost.t) = sprintf "$%g/$%g per M" c.input c.output

let format_tokens n =
  if n >= 1_000_000
  then sprintf "%.1fM" (Float.of_int n /. 1e6)
  else if n >= 1000
  then sprintf "%.1fk" (Float.of_int n /. 1e3)
  else Int.to_string n
;;

let model_picker m ~query =
  let current = Option.map m.state ~f:(fun s -> s.model.key) in
  let items =
    List.map m.models ~f:(fun (model : P.Model.t) ->
      let logged = logged_in m model.provider in
      Picker.Item.create
        ~id:model.key
        ~detail:
          (String.concat
             ~sep:"  "
             ([ model.key
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
  open_picker m Models (Picker.create ~query ~title:"Model" items)
;;

let thinking_levels = [ "off"; "on"; "low"; "high"; "max" ]

let thinking_picker m =
  let current = Option.map m.state ~f:(fun s -> s.thinking) in
  let items =
    List.map thinking_levels ~f:(fun level ->
      Picker.Item.create
        ~id:level
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

let sessions_picker m (sessions : P.Session_summary.t list) =
  let current = Option.map m.state ~f:(fun s -> s.session_path) in
  let items =
    List.map sessions ~f:(fun s ->
      let first =
        Option.value_map s.first_prompt ~default:"(empty)" ~f:(fun p ->
          Text_width.truncate
            (String.concat ~sep:" " (String.split_lines p))
            ~width:60)
      in
      Picker.Item.create
        ~id:s.path
        ~detail:(sprintf "%d msgs  %s" s.message_count s.cwd)
        ~search:(first ^ " " ^ s.cwd ^ " " ^ s.created_at)
        ~marked:
          (Option.value_map current ~default:false ~f:(String.equal s.path))
        (String.prefix s.created_at 19 ^ "  " ^ first))
  in
  if List.is_empty items
  then notice m "no saved sessions"
  else open_picker m Sessions (Picker.create ~title:"Sessions" items)
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

(* ---- slash commands --------------------------------------------------- *)

let set_model_command key =
  rpc "set_model" ~params:[ "model", str key ] ~tag:Set_model_done
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

let run_command m (cmd : Commands.Parsed.t) =
  match cmd.name, cmd.args with
  | "", _ -> m, []
  | "help", _ ->
    let heading text : Content.Line.t =
      [ { text; style = Style.bold (Style.fg Cyan) } ]
    in
    ( block
        m
        ((heading "Commands" :: Commands.help)
         @ ([] :: heading "Keys" :: Keymap.help))
    , [] )
  | "model", [] ->
    if List.is_empty m.models
    then m, [ rpc "list_models" ~tag:(Models_for_picker "") ]
    else model_picker m ~query:"", []
  | "model", _ ->
    if List.is_empty m.models
    then m, [ rpc "list_models" ~tag:(Models_for_switch cmd.rest) ]
    else switch_model m cmd.rest
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
  | "agents", _ -> agents_picker m
  | "sessions", _ | "switch", [] ->
    m, [ rpc "list_sessions" ~tag:Sessions_picker ]
  | "switch", _ ->
    ( m
    , [ rpc
          "switch_session"
          ~params:[ "path", str cmd.rest ]
          ~tag:Reload_messages
      ] )
  | "cd", [] -> error m "usage: /cd <path>", []
  | "cd", _ ->
    m, [ rpc "set_cwd" ~params:[ "path", str cmd.rest ] ~tag:Show_error ]
  | "fork", _ -> m, [ rpc "fork" ~tag:(Notice_on_success "forked session") ]
  | "abort", _ -> m, [ rpc "abort" ]
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
         , [ Command.List_paths { prefix; tag = Paths_for_autocomplete prefix }
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

let interrupt m =
  if not (Editor.is_empty m.editor)
  then { m with editor = Editor.clear m.editor; pending_quit = false }, []
  else if m.pending_quit
  then { m with quitting = true }, [ Command.Quit ]
  else warn { m with pending_quit = true } "press Ctrl+C again to quit", []
;;

let page_size m = Int.max 1 (m.height / 2)

let scroll_up m =
  match m.viewport with
  | Viewport.Follow ->
    { m with
      viewport =
        Viewport.Anchored
          { top =
              Int.max
                0
                (transcript_line_count m - transcript_rows m - page_size m)
          ; new_lines = 0
          }
    }
  | Viewport.Anchored { top; new_lines } ->
    { m with
      viewport =
        Viewport.Anchored { top = Int.max 0 (top - page_size m); new_lines }
    }
;;

let scroll_down m =
  match m.viewport with
  | Viewport.Follow -> m
  | Viewport.Anchored { top; new_lines } ->
    let top = top + page_size m in
    if top + transcript_rows m >= transcript_line_count m
    then follow m
    else { m with viewport = Viewport.Anchored { top; new_lines } }
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
  | Page_up -> scroll_up m, []
  | Page_down -> scroll_down m, []
  | Complete -> m, []
  | Cancel ->
    (match m.focus with
     | `Agent _ -> set_focus m `Main, []
     | `Main ->
       if Model.running m then m, [ rpc "abort" ~tag:Abort_done ] else m, [])
  | Interrupt -> interrupt m
  | Force_quit -> { m with quitting = true }, [ Quit ]
  | Kill_to_end -> ed Editor.kill_to_end
  | Kill_to_start -> ed Editor.kill_to_start
  | Kill_word -> ed Editor.kill_word
  | Cycle_verbosity -> set_verbosity m (Verbosity.next m.verbosity), []
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
;;

(* Autocomplete is a sub-state of editing: while it is open it owns a few keys,
   everything else edits the buffer and then recomputes the completion. *)
let editing m (intent : Intent.t) =
  match intent with
  | Next_agent -> cycle_focus m, []
  | Focus_agent n -> focus_agent m n, []
  | _ ->
    (match m.autocomplete with
     | Some _ ->
       (match intent with
        | Complete ->
          Option.value (accept_autocomplete m ~submit_now:false) ~default:(m, [])
        | Submit ->
          Option.value
            (accept_autocomplete m ~submit_now:true)
            ~default:(submit m)
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
        | Interrupt
        | Force_quit
        | Kill_to_end
        | Kill_to_start
        | Kill_word
        | Yank
        | Yank_pop
        | Undo
        | Cycle_verbosity
        | Next_agent
        | Focus_agent _
        | Queue_follow_up
        | Dequeue
        | Copy_last
        | Suspend
        | Path_complete
        | Edit_externally
        | Model_picker ->
          let m, cmds = editing_intent m intent in
          let m, more = refresh_autocomplete m in
          m, cmds @ more)
     | None ->
       (match intent with
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
  | Models -> m, [ set_model_command item.id ]
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
  | Sessions ->
    ( m
    , [ rpc
          "switch_session"
          ~params:[ "path", str item.id ]
          ~tag:Reload_messages
      ] )
  | Agents -> set_focus m (`Agent item.id), []
  | Auth_select id ->
    m, [ rpc "auth_respond" ~params:[ "id", str id; "value", str item.id ] ]
;;

let picker m kind picker (intent : Intent.t) =
  match intent with
  | Force_quit -> { m with quitting = true }, [ Command.Quit ]
  | _ ->
    (match
       Picker.handle
         picker
         (if Intent.equal intent Interrupt then Cancel else intent)
         ~page:10
     with
     | Continue picker -> { m with mode = Picker { kind; picker } }, []
     | Selected item -> picker_selected m kind item
     | Cancelled ->
       let m = { m with mode = Editing } in
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
    ( { m with editor = Editor.clear m.editor; mode = Editing }
    , [ rpc "auth_cancel" ] )
  | Force_quit -> { m with quitting = true }, [ Quit ]
  | Up
  | Down
  | Page_up
  | Page_down
  | Complete
  | Cycle_verbosity
  | Next_agent
  | Focus_agent _
  | Queue_follow_up
  | Dequeue
  | Copy_last
  | Suspend
  | Path_complete
  | Edit_externally
  | Model_picker
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

(* ---- confirm mode ----------------------------------------------------- *)

let confirm m ~(action : Mode.Confirm_action.t) (intent : Intent.t) =
  let yes () =
    let m = { m with mode = Editing } in
    match action with
    | Logout provider ->
      m, [ rpc "logout" ~params:[ "provider", str provider ] ]
  in
  match intent with
  | Insert ("y" | "Y") | Submit -> yes ()
  | Insert ("n" | "N") | Cancel | Interrupt ->
    notice { m with mode = Editing } "cancelled", []
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
  | Confirm { action; _ } -> confirm m ~action intent
;;

(* ---- backend events --------------------------------------------------- *)

let auth_event m (e : P.Auth_event.t) =
  match e with
  | Auth_url { url; instructions } ->
    let content : Content.t =
      [ [ { text = "Open this URL to log in:"; style = Style.bold Style.plain }
        ]
      ; [ { text = "  " ^ url; style = Style.underline (Style.fg Cyan) } ]
      ; [ { text = instructions; style = Style.fg Gray } ]
      ]
    in
    block m content, [ Command.Open_browser url ]
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
        let m = notice m (P.Auth_event.Prompt.message prompt) in
        let m =
          match prompt with
          | Manual_code { placeholder; _ } ->
            notice m ("e.g. " ^ placeholder ^ "?code=...")
          | _ -> m
        in
        ( { m with
            mode = Login_prompt { id; prompt }
          ; editor = Editor.clear m.editor
          }
        , [] ))
  | Prompt_cancelled { id } ->
    (match m.mode with
     | Login_prompt { id = current; _ } when String.equal id current ->
       { m with mode = Editing; editor = Editor.clear m.editor }, []
     | Picker { kind = Auth_select current; _ } when String.equal id current ->
       { m with mode = Editing }, []
     | _ -> m, [])
  | Progress message -> notice m message, []
  | Done { provider; method_ } ->
    let m =
      match m.mode with
      | Login_prompt _ -> { m with mode = Editing }
      | _ -> m
    in
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
  | Auth a -> auth_event m a
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
  | Subagent_end _ -> m, []
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
     | Set_model_done ->
       (* The backend formats "did you mean"; the picker helps recover. *)
       model_picker (error m e) ~query:"", []
     | _ -> error m e, [])
  | Ok json ->
    (match tag with
     | Ignore | Show_error -> m, []
     | Notice_on_success text -> notice m text, []
     | Set_model_done -> m, []
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
     | Initial_messages | Reload_messages ->
       decode json ~f:(decode_list ~f:P.Message.of_json) (fun messages ->
         let reload = Reply_tag.equal tag Reload_messages in
         let m = if reload then { m with agents = []; focus = `Main } else m in
         let transcript =
           if reload then Transcript.clear m.transcript else m.transcript
         in
         let transcript =
           List.fold messages ~init:transcript ~f:Transcript.add_message
         in
         ( follow { m with transcript }
         , if reload then [ rpc "get_state" ~tag:Initial_state ] else [] ))
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

let update m (action : Action.t) =
  if m.quitting
  then m, []
  else (
    match action with
    | Start -> m, start_commands
    | Key key ->
      (match Keymap.lookup key with
       | Some i -> intent m i
       | None -> m, [])
    | Intent i -> intent m i
    | Event e -> event m e
    | Protocol_error text -> error m ("protocol error: " ^ text), []
    | Stderr line -> notice ~severity:Warn m ("backend: " ^ line), []
    | Backend_closed ->
      error { m with quitting = true } "backend exited", [ Quit ]
    | Reply (tag, result) -> reply m tag result
    | Tick -> { m with spinner = m.spinner + 1 }, []
    | Resize { width; height } -> { m with width; height }, [])
;;
