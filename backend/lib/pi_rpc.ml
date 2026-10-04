open! Core
open! Import
module P = Pi_protocol

let str s = `String s
let int i = `Number (Int.to_string i)
let field name json = Option.value (Json.member name json) ~default:`Null

let string_field name json =
  Option.value (Json.string (field name json)) ~default:""
;;

let opt_string_field name json = Json.string (field name json)

let list_field name json =
  Option.value (Json.list (field name json)) ~default:[]
;;

module Tool_state = struct
  type t =
    { name : string
    ; args : Json.t
    ; output : Buffer.t
    }
end

module Watch = struct
  (** The subagent whose transcript the frontend shows: its events are
      forwarded as [prigh_subagent_event]s, with message timestamps
      continuing the transcript's indices. *)
  type t =
    { agent_id : string
    ; session_id : string
    ; mutable next_ts : int
    ; mutable current_ts : int
    ; tools : Tool_state.t String.Table.t
    ; mutable info : Json.t
    }
end

type t =
  { server : Rpc_server.t
  ; client : Rpc_server.Client.t
  ; write : Json.t -> unit
  ; now : unit -> int
  ; mutable seq : int
  ; mutable dialog_seq : int
  ; mutable next_ts : int
  ; mutable current_ts : int
  ; mutable last_assistant_ts : int
  ; result_ts : int String.Table.t
  ; tools : Tool_state.t String.Table.t
  ; mutable widget_runs : Json.t option
    (** the agents rail's runs as last sent, [None] when empty *)
  ; mutable watch : Watch.t option
  ; mutable messages_session : string option
    (** the session the last [get_messages] answered for *)
  ; mutable last_state : Json.t option
  ; mutable quiet_state_changes : int
    (** > 0 while running a command after which the frontend re-syncs itself *)
  ; dialogs : (Json.t -> unit) String.Table.t
    (** pending [extension_ui_request]s by id, answered with the response *)
  }

let call t meth params =
  t.seq <- t.seq + 1;
  let request =
    `Object
      [ "id", str (sprintf "pi-%d" t.seq)
      ; "method", str meth
      ; "params", `Object params
      ]
  in
  let response = Rpc_server.handle t.server t.client request in
  match field "ok" response with
  | `True -> Ok (field "result" response)
  | _ -> Or_error.error_string (string_field "error" response)
;;

let event t name fields = t.write (P.event name fields)

let take_ts t =
  let ts = t.next_ts in
  t.next_ts <- ts + 1;
  ts
;;

let show t ~kind text =
  event
    t
    "message_end"
    [ "message", P.custom_message ~timestamp:(take_ts t) ~kind text ]
;;

let notify t ?kind message = t.write (P.notify ?kind message)

(* A [null] [statusText] removes the entry. *)
let set_status t key text =
  t.write
    (P.ui_request
       ~id:("status-" ^ key)
       ~meth:"setStatus"
       [ "statusKey", str key
       ; "statusText", Option.value_map text ~default:`Null ~f:str
       ])
;;

let dialog t ~meth fields ~on_response =
  t.dialog_seq <- t.dialog_seq + 1;
  let id = sprintf "dialog-%d" t.dialog_seq in
  Hashtbl.set t.dialogs ~key:id ~data:on_response;
  t.write (P.ui_request ~id ~meth fields)
;;

(* A [select] whose options are labels; the answer is mapped back to the
   value behind the chosen label. *)
let select t ~title options ~f =
  dialog
    t
    ~meth:"select"
    [ "title", str title
    ; "options", `Array (List.map options ~f:(fun (label, _) -> str label))
    ]
    ~on_response:(fun response ->
      match opt_string_field "value" response with
      | Some label ->
        (match List.Assoc.find options ~equal:String.equal label with
         | Some value -> f value
         | None -> notify t ~kind:"error" ("unknown choice " ^ label))
      | None -> ())
;;

let report t result =
  match result with
  | Ok () -> ()
  | Error e -> notify t ~kind:"error" (Error.to_string_hum e)
;;

(* ---------------------------------------------------------------------- *)
(* Subagents                                                               *)

let first_line s =
  let line = Option.value (List.hd (String.split_lines s)) ~default:s in
  if String.length line > 60 then String.prefix line 57 ^ "..." else line
;;

let current_session t =
  Option.value_map t.last_state ~default:"" ~f:(string_field "session_id")
;;

(* A [Subagent_log] summary as a node of pi-subagents' status snapshot. *)
let subagent_node_fields s =
  let opt_ms key name =
    Option.value_map
      (Json.int (field name s))
      ~default:[]
      ~f:(fun v -> [ key, int v ])
  in
  [ "id", str (string_field "id" s)
  ; "kind", str "subagent"
  ; "label", str (first_line (string_field "task" s))
  ; "state", str (string_field "state" s)
  ]
  @ opt_ms "startedAt" "started_at_ms"
  @ opt_ms "updatedAt" "updated_at_ms"
  @ opt_ms "endedAt" "ended_at_ms"
  @ [ ( "activity"
      , `Object
          ([ "turnCount", field "turns" s; "toolCount", field "tool_calls" s ]
           @ Option.value_map
               (opt_string_field "current_tool" s)
               ~default:[]
               ~f:(fun tool -> [ "currentTool", str tool ])
           @ opt_ms "currentToolStartedAt" "current_tool_started_at_ms") )
    ]
;;

let rec subagent_node ~all s =
  let id = string_field "id" s in
  let children =
    List.filter all ~f:(fun c ->
      Option.equal String.equal (opt_string_field "parent" c) (Some id))
  in
  `Object
    (subagent_node_fields s
     @
     if List.is_empty children
     then []
     else [ "children", `Array (List.map children ~f:(subagent_node ~all)) ])
;;

(* What the subagent view shows about the subagent besides its transcript. *)
let subagent_info s =
  let result =
    match field "result" s with
    | `Object _ as r ->
      `Object
        [ "text", field "text" r
        ; ( "isError"
          , if Json.exactly_equal (field "is_error" r) `True
            then `True
            else `False )
        ]
    | _ -> `Null
  in
  `Object
    (subagent_node_fields s
     @ [ "task", field "task" s
       ; "model", field "model" s
       ; "callId", field "call_id" s
       ; "parentId", field "parent" s
       ; "result", result
       ])
;;

let watched t =
  Option.filter t.watch ~f:(fun (w : Watch.t) ->
    String.equal w.session_id (current_session t))
;;

let subagent_event t (w : Watch.t) name fields =
  t.write
    (P.event
       "prigh_subagent_event"
       [ "agentId", str w.agent_id; "event", P.event name fields ])
;;

(* Re-reads the subagents: the agents rail widget is sent when its runs
   changed (or, on [resync], whenever there are any: the frontend cleared
   it), and the watched subagent's info when that changed. *)
let refresh_subagents ?(resync = false) t =
  let all =
    match call t "list_subagents" [] with
    | Ok summaries -> Option.value (Json.list summaries) ~default:[]
    | Error _ -> []
  in
  let visible =
    List.filter all ~f:(fun s ->
      not (Json.exactly_equal (field "stale" s) `True))
  in
  let runs =
    List.filter_map visible ~f:(fun s ->
      if Option.is_none (opt_string_field "parent" s)
      then Some (subagent_node ~all:visible s)
      else None)
  in
  let runs = if List.is_empty runs then None else Some (`Array runs) in
  let send =
    if resync
    then Option.is_some runs
    else not (Option.equal Json.exactly_equal runs t.widget_runs)
  in
  t.widget_runs <- runs;
  if send
  then (
    let lines =
      Option.value_map runs ~default:`Null ~f:(fun runs ->
        let snapshot =
          `Object
            [ "generatedAt", int (t.now ())
            ; ( "omitted"
              , `Object
                  [ "runs", int 0
                  ; "children", int 0
                  ; "byteLimitExceeded", `False
                  ] )
            ; "runs", runs
            ]
        in
        `Array [ str ("PI_SUBAGENT_ASYNC_JSON:" ^ Json.to_string snapshot) ])
    in
    t.write
      (P.ui_request
         ~id:"widget-subagents"
         ~meth:"setWidget"
         [ "widgetKey", str "subagents"; "widgetLines", lines ]));
  Option.iter (watched t) ~f:(fun w ->
    Option.iter
      (List.find all ~f:(fun s -> String.equal (string_field "id" s) w.agent_id))
      ~f:(fun s ->
        let info = subagent_info s in
        if not (Json.exactly_equal info w.info)
        then (
          w.info <- info;
          subagent_event t w "subagent_info" [ "subagent", info ])))
;;

let watch_subagent t key =
  Or_error.map
    (call t "get_subagent" [ "id", str key ])
    ~f:(fun result ->
      let s = field "subagent" result in
      let messages = list_field "messages" result in
      let n = List.length messages in
      let info = subagent_info s in
      t.watch
      <- Some
           { agent_id = string_field "id" s
           ; session_id = current_session t
           ; next_ts = n
           ; current_ts = n
           ; tools = String.Table.create ()
           ; info
           };
      `Object
        [ "subagent", info
        ; ( "messages"
          , `Array (List.mapi messages ~f:(fun i m -> P.message ~timestamp:i m))
          )
        ])
;;

(* ---------------------------------------------------------------------- *)
(* Dialogs                                                                 *)

let confirm_dialog t json =
  let call_id = string_field "call_id" json in
  let dialog_id = "confirm-" ^ call_id in
  Hashtbl.set t.dialogs ~key:dialog_id ~data:(fun response ->
    let allow = Json.exactly_equal (field "confirmed" response) `True in
    report
      t
      (call
         t
         "tool_confirm_respond"
         [ "call_id", str call_id; ("allow", if allow then `True else `False) ]
       |> Or_error.ignore_m));
  t.write
    (P.ui_request
       ~id:dialog_id
       ~meth:"confirm"
       [ "title", str (sprintf "Run %s?" (string_field "name" json))
       ; "message", str (string_field "summary" json)
       ])
;;

(* Tool confirmations belong to the session that asked: answering one after
   switching would go to the new session. *)
let cancel_confirm_dialogs t =
  Hashtbl.keys t.dialogs
  |> List.filter ~f:(String.is_prefix ~prefix:"confirm-")
  |> List.sort ~compare:String.compare
  |> List.iter ~f:(fun id ->
    Hashtbl.remove t.dialogs id;
    event t "extension_ui_cancel" [ "id", str id ])
;;

(* ---------------------------------------------------------------------- *)
(* State                                                                   *)

let status_texts state =
  let active = string_field "active_host" state in
  let hosts = list_field "hosts" state in
  let host =
    if String.equal active Agent.Host.backend_id
    then None
    else (
      match
        List.find hosts ~f:(fun h -> String.equal (string_field "id" h) active)
      with
      | Some h -> Some ("tools: " ^ string_field "name" h)
      | None -> Some "tools: offline")
  in
  let branch =
    Option.map (opt_string_field "git_branch" state) ~f:(fun b -> "⎇ " ^ b)
  in
  [ "host", host; "branch", branch ]
;;

let apply_state t state =
  let previous = t.last_state in
  t.last_state <- Some state;
  let changed name =
    match previous with
    | None -> true
    | Some p -> not (Json.exactly_equal (field name p) (field name state))
  in
  List.iter (status_texts state) ~f:(fun (key, text) ->
    let before =
      Option.bind previous ~f:(fun p ->
        List.Assoc.find (status_texts p) ~equal:String.equal key)
      |> Option.join
    in
    if Option.is_none previous || not (Option.equal String.equal before text)
    then set_status t key text);
  match previous with
  | None -> ()
  | Some p ->
    if changed "session_id"
    then (
      t.watch <- None;
      cancel_confirm_dialogs t;
      refresh_subagents t);
    if changed "session_name"
    then
      event
        t
        "session_info_changed"
        [ ( "name"
          , Option.value_map
              (opt_string_field "session_name" state)
              ~default:`Null
              ~f:str )
        ];
    if changed "thinking"
    then
      event
        t
        "thinking_level_changed"
        [ ( "level"
          , str (P.Thinking_level.of_prigh (string_field "thinking" state)) )
        ];
    let was_running = Json.exactly_equal (field "running" p) `True in
    let running = Json.exactly_equal (field "running" state) `True in
    if was_running && not running then event t "agent_settled" [];
    if
      t.quiet_state_changes = 0
      && (changed "session_id"
          || changed "cwd"
          || not
               (Json.exactly_equal
                  (field "key" (field "model" p))
                  (field "key" (field "model" state))))
    then event t "session_reloaded" []
;;

(* Runs [f] with state diffs not turning into [session_reloaded]: the
   frontend re-syncs after these commands on its own. *)
let quietly t f =
  t.quiet_state_changes <- t.quiet_state_changes + 1;
  protect ~f ~finally:(fun () ->
    t.quiet_state_changes <- t.quiet_state_changes - 1)
;;

let refresh_state t =
  match call t "get_state" [] with
  | Ok state -> quietly t (fun () -> apply_state t state)
  | Error _ -> ()
;;

(* The session's live state a re-syncing frontend has just cleared: status
   entries, queued messages, unanswered confirmations, the agents rail. *)
let resync t state =
  List.iter (status_texts state) ~f:(fun (key, text) ->
    if Option.is_some text then set_status t key text);
  (match call t "get_pending" [] with
   | Ok pending ->
     let steer = list_field "steer_texts" pending in
     let follow_up = list_field "follow_up_texts" pending in
     if not (List.is_empty steer && List.is_empty follow_up)
     then
       event
         t
         "queue_update"
         [ "steering", `Array steer; "followUp", `Array follow_up ];
     List.iter (list_field "confirms" pending) ~f:(confirm_dialog t)
   | Error _ -> ());
  refresh_subagents ~resync:true t
;;

(* ---------------------------------------------------------------------- *)
(* Events                                                                  *)

let is_shell_call id = String.is_prefix id ~prefix:"shell-"

let tool_fields tools call_id =
  match Hashtbl.find tools call_id with
  | Some (tool : Tool_state.t) ->
    [ "toolCallId", str call_id; "toolName", str tool.name; "args", tool.args ]
  | None ->
    [ "toolCallId", str call_id; "toolName", str "tool"; "args", `Object [] ]
;;

let tool_start tools call =
  let id = string_field "id" call in
  Hashtbl.set
    tools
    ~key:id
    ~data:
      { Tool_state.name = string_field "name" call
      ; args = P.arguments_object (string_field "arguments" call)
      ; output = Buffer.create 256
      };
  tool_fields tools id
;;

let tool_output tools json =
  let id = string_field "call_id" json in
  Option.map (Hashtbl.find tools id) ~f:(fun (tool : Tool_state.t) ->
    Buffer.add_string tool.output (string_field "chunk" json);
    tool_fields tools id
    @ [ ( "partialResult"
        , `Object
            [ "content", P.tool_result_content (Buffer.contents tool.output) ] )
      ])
;;

let tool_end tools json =
  let id = string_field "id" (field "call" json) in
  let result = field "result" json in
  let fields =
    tool_fields tools id
    @ [ ( "result"
        , `Object
            [ "content", P.tool_result_content (string_field "text" result) ] )
      ; "isError", field "is_error" result
      ]
  in
  Hashtbl.remove tools id;
  fields
;;

(* A nested [subagent] event's innermost agent and its own event. *)
let rec innermost json =
  let inner = field "inner" json in
  if String.equal (string_field "event" inner) "subagent"
  then innermost inner
  else string_field "agent_id" json, inner
;;

let forward_to_watch t (w : Watch.t) inner =
  let message ts =
    [ ( "message"
      , P.message
          ~timestamp:ts
          (field
             (if String.equal (string_field "event" inner) "message_update"
              then "partial"
              else "message")
             inner) )
    ]
  in
  match string_field "event" inner with
  | "message_start" ->
    w.current_ts <- w.next_ts;
    subagent_event t w "message_start" (message w.current_ts)
  | "message_update" ->
    subagent_event t w "message_update" (message w.current_ts)
  | "message_end" ->
    subagent_event t w "message_end" (message w.current_ts);
    w.next_ts <- w.current_ts + 1
  | "tool_start" ->
    subagent_event
      t
      w
      "tool_execution_start"
      (tool_start w.tools (field "call" inner))
  | "tool_output" ->
    Option.iter
      (tool_output w.tools inner)
      ~f:(subagent_event t w "tool_execution_update")
  | "tool_end" ->
    subagent_event t w "tool_execution_end" (tool_end w.tools inner)
  | _ -> ()
;;

let on_subagent_event t json =
  let agent_id, inner =
    match string_field "event" json with
    | "subagent" -> innermost json
    | _ -> "", json
  in
  Option.iter (watched t) ~f:(fun w ->
    if String.equal w.agent_id agent_id then forward_to_watch t w inner);
  match string_field "event" inner with
  | "subagent_start" | "subagent_end" | "turn_start" | "tool_start" | "tool_end"
    -> refresh_subagents t
  | _ -> ()
;;

let auth_event t json =
  let provider = string_field "provider" json in
  match string_field "kind" json with
  | "auth_url" ->
    let url = string_field "url" json in
    show
      t
      ~kind:"login"
      (sprintf "%s\n\n[%s](%s)" (string_field "instructions" json) url url)
  | "prompt" ->
    let id = string_field "id" json in
    let message = string_field "message" json in
    let respond value =
      report
        t
        (call t "auth_respond" [ "id", str id; "value", str value ]
         |> Or_error.ignore_m)
    in
    let cancel () = report t (call t "auth_cancel" [] |> Or_error.ignore_m) in
    let on_response response =
      match opt_string_field "value" response with
      | Some value -> respond value
      | None -> cancel ()
    in
    let dialog_id = "auth-" ^ id in
    (match string_field "prompt" json with
     | "select" ->
       let options =
         List.map (list_field "options" json) ~f:(fun o ->
           string_field "label" o, string_field "id" o)
       in
       Hashtbl.set t.dialogs ~key:dialog_id ~data:(fun response ->
         match opt_string_field "value" response with
         | Some label ->
           (match List.Assoc.find options ~equal:String.equal label with
            | Some value -> respond value
            | None -> respond label)
         | None -> cancel ());
       t.write
         (P.ui_request
            ~id:dialog_id
            ~meth:"select"
            [ "title", str message
            ; ( "options"
              , `Array (List.map options ~f:(fun (label, _) -> str label)) )
            ])
     | _ ->
       Hashtbl.set t.dialogs ~key:dialog_id ~data:on_response;
       t.write
         (P.ui_request
            ~id:dialog_id
            ~meth:"input"
            [ "title", str message
            ; "placeholder", str (string_field "placeholder" json)
            ]))
  | "prompt_cancelled" ->
    let dialog_id = "auth-" ^ string_field "id" json in
    Hashtbl.remove t.dialogs dialog_id;
    event t "extension_ui_cancel" [ "id", str dialog_id ]
  | "progress" -> notify t (string_field "message" json)
  | "done" ->
    notify
      t
      (sprintf "logged in to %s (%s)" provider (string_field "method" json))
  | "failed" ->
    notify
      t
      ~kind:"error"
      (sprintf "login to %s failed: %s" provider (string_field "error" json))
  | "logged_out" -> notify t (sprintf "logged out of %s" provider)
  | _ -> ()
;;

let on_event t json =
  match string_field "event" json with
  | "state" -> apply_state t (field "state" json)
  | "agent_start" ->
    (* Finished subagents stay on the rail until the next run. *)
    refresh_subagents t;
    event t "agent_start" []
  | "agent_end" ->
    let messages = list_field "messages" json in
    let n = List.length messages in
    event
      t
      "agent_end"
      [ ( "messages"
        , `Array
            (List.mapi messages ~f:(fun i m ->
               P.message ~timestamp:(t.next_ts - n + i) m)) )
      ; "willRetry", `False
      ]
  | "turn_start" -> event t "turn_start" []
  | "turn_end" ->
    let results = list_field "tool_results" json in
    event
      t
      "turn_end"
      [ ( "message"
        , P.message ~timestamp:t.last_assistant_ts (field "assistant" json) )
      ; ( "toolResults"
        , `Array
            (List.map results ~f:(fun r ->
               let ts =
                 Option.value
                   (Hashtbl.find t.result_ts (string_field "tool_call_id" r))
                   ~default:0
               in
               P.message ~timestamp:ts r)) )
      ]
  | "message_start" ->
    t.current_ts <- take_ts t;
    event
      t
      "message_start"
      [ "message", P.message ~timestamp:t.current_ts (field "message" json) ]
  | "message_update" ->
    event
      t
      "message_update"
      [ "message", P.message ~timestamp:t.current_ts (field "partial" json)
      ; ( "assistantMessageEvent"
        , `Object [ "type", str (string_field "type" (field "delta" json)) ] )
      ]
  | "message_end" ->
    let message = field "message" json in
    (match string_field "role" message with
     | "assistant" -> t.last_assistant_ts <- t.current_ts
     | "tool_result" ->
       Hashtbl.set
         t.result_ts
         ~key:(string_field "tool_call_id" message)
         ~data:t.current_ts
     | _ -> ());
    event
      t
      "message_end"
      [ "message", P.message ~timestamp:t.current_ts message ]
  | "tool_start" ->
    let call = field "call" json in
    if not (is_shell_call (string_field "id" call))
    then event t "tool_execution_start" (tool_start t.tools call)
  | "tool_output" ->
    Option.iter (tool_output t.tools json) ~f:(event t "tool_execution_update")
  | "tool_end" ->
    if not (is_shell_call (string_field "id" (field "call" json)))
    then event t "tool_execution_end" (tool_end t.tools json)
  | "tool_confirm" -> confirm_dialog t json
  | "subagent_start" | "subagent" | "subagent_end" -> on_subagent_event t json
  | "compacted" ->
    let tokens_before =
      Option.value_map t.last_state ~default:0 ~f:(fun s ->
        Option.value (Json.int (field "context_tokens" s)) ~default:0)
    in
    event
      t
      "compaction_end"
      [ "reason", str "manual"
      ; ( "result"
        , `Object
            [ "summary", str (string_field "summary" json)
            ; "tokensBefore", int tokens_before
            ] )
      ; "aborted", `False
      ; "willRetry", `False
      ]
  | "notice" ->
    let text = string_field "text" json in
    notify
      t
      ~kind:
        (if String.is_substring text ~substring:"failed"
         then "warning"
         else "info")
      text
  | "queue_update" ->
    event
      t
      "queue_update"
      [ "steering", field "steer_texts" json
      ; "followUp", field "follow_up_texts" json
      ]
  | "auth" -> auth_event t json
  | _ -> ()
;;

(* ---------------------------------------------------------------------- *)
(* Slash commands run here                                                 *)

let markdown_escape s = String.substr_replace_all s ~pattern:"|" ~with_:"\\|"

let session_label s =
  let title =
    List.find_map [ "name"; "description"; "first_prompt" ] ~f:(fun k ->
      opt_string_field k s)
    |> Option.value ~default:"(empty)"
    |> first_line
  in
  let live =
    if Json.exactly_equal (field "live" s) `True then " · live" else ""
  in
  sprintf
    "%s · %d msgs%s · %s"
    title
    (Option.value (Json.int (field "message_count" s)) ~default:0)
    live
    (string_field "id" s)
;;

let switch_session t key =
  let result =
    quietly t (fun () -> call t "switch_session" [ "path", str key ])
  in
  Or_error.map result ~f:(fun _ ->
    refresh_state t;
    event t "session_reloaded" [])
;;

let auth_status t =
  Or_error.map (call t "auth_status" []) ~f:(fun statuses ->
    let lines =
      List.map
        (Option.value (Json.list statuses) ~default:[])
        ~f:(fun s ->
          let configured =
            match field "configured" s with
            | `Object _ as c ->
              sprintf
                "%s (%s)"
                (string_field "method" c)
                (string_field "source" c)
            | _ -> "not logged in"
          in
          sprintf
            "- **%s** `%s`: %s"
            (string_field "name" s)
            (string_field "provider" s)
            configured)
    in
    show t ~kind:"auth" (String.concat ~sep:"\n" ("Providers:" :: lines)))
;;

let login t args =
  match args with
  | provider :: rest ->
    let method_ =
      Option.value_map (List.hd rest) ~default:[] ~f:(fun m ->
        [ "method", str m ])
    in
    call t "login" (("provider", str provider) :: method_) |> Or_error.ignore_m
  | [] ->
    Or_error.map (call t "auth_status" []) ~f:(fun statuses ->
      let options =
        List.concat_map
          (Option.value (Json.list statuses) ~default:[])
          ~f:(fun s ->
            List.map (list_field "methods" s) ~f:(fun m ->
              ( string_field "label" m
              , (string_field "provider" s, string_field "method" m) )))
      in
      select t ~title:"Log in to" options ~f:(fun (provider, method_) ->
        report
          t
          (call t "login" [ "provider", str provider; "method", str method_ ]
           |> Or_error.ignore_m)))
;;

let pick_session t =
  Or_error.map (call t "list_sessions" []) ~f:(fun sessions ->
    let options =
      List.map
        (Option.value (Json.list sessions) ~default:[])
        ~f:(fun s -> session_label s, string_field "path" s)
    in
    select t ~title:"Switch to session" options ~f:(fun path ->
      report t (switch_session t path)))
;;

let pick_host t =
  Or_error.map (call t "get_state" []) ~f:(fun state ->
    let active = string_field "active_host" state in
    let options =
      List.map (list_field "hosts" state) ~f:(fun h ->
        let id = string_field "id" h in
        ( sprintf
            "%s (%s)%s"
            (string_field "name" h)
            (string_field "cwd" h)
            (if String.equal id active then " · active" else "")
        , (id, string_field "cwd" h) ))
    in
    select t ~title:"Run tools on" options ~f:(fun (host, cwd) ->
      report
        t
        (call t "set_active_host" [ "host", str host; "cwd", str cwd ]
         |> Or_error.ignore_m)))
;;

let change_default t =
  Or_error.map (call t "change_default" []) ~f:(fun config ->
    notify
      t
      (sprintf
         "default: %s, thinking %s"
         (string_field "default_model" config)
         (string_field "default_thinking" config)))
;;

let help t =
  let lines =
    List.map (list_field "commands" P.commands) ~f:(fun c ->
      sprintf
        "- `/%s%s` — %s"
        (string_field "name" c)
        (Option.value_map
           (opt_string_field "argumentHint" c)
           ~default:""
           ~f:(fun h -> " " ^ h))
        (markdown_escape (string_field "description" c)))
  in
  show t ~kind:"help" (String.concat ~sep:"\n" lines)
;;

let slash_command t text =
  let words =
    String.split (String.strip text) ~on:' '
    |> List.filter ~f:(Fn.non String.is_empty)
  in
  match words with
  | [] -> Or_error.error_string "empty command"
  | name :: args ->
    (match String.chop_prefix_exn name ~prefix:"/", args with
     | "login", args -> login t args
     | "logout", provider :: _ ->
       call t "logout" [ "provider", str provider ] |> Or_error.ignore_m
     | "logout", [] -> Or_error.error_string "usage: /logout <provider>"
     | "auth", _ -> auth_status t
     | "sessions", _ -> pick_session t
     | "switch", key :: _ -> switch_session t key
     | "switch", [] -> Or_error.error_string "usage: /switch <id|path>"
     | "host", _ -> pick_host t
     | "change_default", _ -> change_default t
     | "help", _ -> Ok (help t)
     | name, _ -> Or_error.errorf "unknown command /%s" name)
;;

(* ---------------------------------------------------------------------- *)
(* Commands                                                                *)

let unit_ok result = Or_error.map result ~f:(fun _ -> `Object [])

let messages t =
  Or_error.map (call t "get_messages" []) ~f:(fun messages ->
    Option.value (Json.list messages) ~default:[])
;;

let entries t =
  Or_error.map (call t "get_entries" []) ~f:(fun result ->
    list_field "entries" result)
;;

let user_entries entries =
  List.filter_map entries ~f:(fun e ->
    let message = field "message" e in
    if
      String.equal (string_field "kind" e) "message"
      && String.equal (string_field "role" message) "user"
    then Some (e, string_field "text" message)
    else None)
;;

let fork t entry_id =
  Or_error.bind (entries t) ~f:(fun entries ->
    match
      List.find entries ~f:(fun e ->
        String.equal (string_field "id" e) entry_id)
    with
    | None -> Or_error.errorf "no entry %S" entry_id
    | Some entry ->
      let text = string_field "text" (field "message" entry) in
      let forked =
        quietly t (fun () ->
          match opt_string_field "parent" entry with
          | Some parent -> call t "fork" [ "at", str parent ]
          | None -> call t "new_session" [])
      in
      Or_error.map forked ~f:(fun _ ->
        refresh_state t;
        `Object [ "text", str text; "cancelled", `False ]))
;;

let last_assistant_text messages =
  List.rev messages
  |> List.find_map ~f:(fun m ->
    if String.equal (string_field "role" m) "assistant"
    then
      Some
        (List.filter_map (list_field "content" m) ~f:(fun block ->
           if String.equal (string_field "type" block) "text"
           then Some (string_field "text" block)
           else None)
         |> String.concat ~sep:"\n")
    else None)
;;

let thinking t =
  Or_error.map (call t "get_state" []) ~f:(fun state ->
    let supports =
      Json.exactly_equal (field "supports_thinking" (field "model" state)) `True
    in
    supports, P.Thinking_level.of_prigh (string_field "thinking" state))
;;

(* The level pi asked for may not exist in prigh ([xhigh] becomes [max]):
   answer with the level actually set. *)
let set_thinking t level =
  let prigh_level = P.Thinking_level.to_prigh level in
  quietly t (fun () -> call t "set_thinking" [ "thinking", str prigh_level ])
  |> Or_error.map ~f:(fun _ ->
    `Object [ "level", str (P.Thinking_level.of_prigh prigh_level) ])
;;

let run_command t json =
  let arg name = string_field name json in
  match string_field "type" json with
  | "prompt" ->
    let text = arg "message" in
    if
      String.is_prefix text ~prefix:"/"
      && List.mem
           P.server_commands
           (List.hd_exn (String.split (String.strip text) ~on:' ')
            |> String.chop_prefix_exn ~prefix:"/")
           ~equal:String.equal
    then unit_ok (slash_command t text)
    else (
      let meth =
        match arg "streamingBehavior" with
        | "steer" -> "steer"
        | "followUp" -> "follow_up"
        | _ -> "prompt"
      in
      unit_ok (call t meth [ "text", str text ]))
  | "steer" -> unit_ok (call t "steer" [ "text", str (arg "message") ])
  | "follow_up" -> unit_ok (call t "follow_up" [ "text", str (arg "message") ])
  | "abort" -> unit_ok (call t "abort" [])
  | "get_state" ->
    Or_error.map (call t "get_state" []) ~f:(fun state ->
      quietly t (fun () -> apply_state t state);
      resync t state;
      P.session_state state)
  | "get_messages" ->
    Or_error.map (messages t) ~f:(fun messages ->
      t.next_ts <- Int.max t.next_ts (List.length messages);
      let session = current_session t in
      if not (Option.equal String.equal t.messages_session (Some session))
      then (
        t.messages_session <- Some session;
        (* A message streaming in this session started before the client
           joined: give it a slot after the history. *)
        if
          Option.exists t.last_state ~f:(fun s ->
            Json.exactly_equal (field "running" s) `True)
        then t.current_ts <- take_ts t);
      `Object
        [ ( "messages"
          , `Array (List.mapi messages ~f:(fun i m -> P.message ~timestamp:i m))
          )
        ])
  | "get_commands" -> Ok P.commands
  | "get_session_stats" ->
    Or_error.bind (call t "get_state" []) ~f:(fun state ->
      Or_error.map (call t "session_stats" []) ~f:(fun stats ->
        P.session_stats ~state ~stats))
  | "set_model" ->
    quietly t (fun () ->
      call t "set_model" [ "model", str (arg "provider" ^ "/" ^ arg "modelId") ])
    |> unit_ok
  | "set_thinking_level" -> set_thinking t (arg "level")
  | "cycle_thinking_level" ->
    Or_error.bind (thinking t) ~f:(fun (supports_thinking, current) ->
      if not supports_thinking
      then Ok `Null
      else set_thinking t (P.Thinking_level.next ~supports_thinking ~current))
  | "get_available_thinking_levels" ->
    Or_error.map (thinking t) ~f:(fun (supports_thinking, _) ->
      `Object
        [ ( "levels"
          , `Array
              (List.map (P.Thinking_level.available ~supports_thinking) ~f:str)
          )
        ])
  | "compact" -> Or_error.map (call t "compact" []) ~f:Fn.id
  | "set_session_name" ->
    unit_ok (call t "set_session_name" [ "name", str (arg "name") ])
  | "new_session" ->
    quietly t (fun () -> call t "new_session" [])
    |> Or_error.map ~f:(fun _ ->
      refresh_state t;
      `Object [])
  | "get_available_models" ->
    Or_error.map (call t "list_models" []) ~f:(fun models ->
      `Object
        [ ( "models"
          , `Array
              (List.map
                 (Option.value (Json.list models) ~default:[])
                 ~f:P.model) )
        ])
  | "bash" ->
    Or_error.map
      (call
         t
         "shell"
         [ "command", str (arg "command"); "add_to_context", `True ])
      ~f:(fun result ->
        let is_error = Json.exactly_equal (field "is_error" result) `True in
        `Object
          [ "output", str (string_field "text" result)
          ; "exitCode", int (if is_error then 1 else 0)
          ; "cancelled", `False
          ; "truncated", `False
          ])
  | "export_html" ->
    let path =
      Option.value_map
        (opt_string_field "outputPath" json)
        ~default:[]
        ~f:(fun p -> [ "path", str p ])
    in
    call t "export" (("format", str "markdown") :: path)
  | "get_last_assistant_text" ->
    Or_error.map (messages t) ~f:(fun messages ->
      `Object
        [ ( "text"
          , Option.value_map
              (last_assistant_text messages)
              ~default:`Null
              ~f:str )
        ])
  | "get_fork_messages" ->
    Or_error.map (entries t) ~f:(fun entries ->
      `Object
        [ ( "messages"
          , `Array
              (List.map (user_entries entries) ~f:(fun (e, text) ->
                 `Object
                   [ "entryId", str (string_field "id" e); "text", str text ]))
          )
        ])
  | "fork" -> fork t (arg "entryId")
  | "clone" ->
    quietly t (fun () -> call t "clone" [])
    |> Or_error.map ~f:(fun _ ->
      refresh_state t;
      `Object [ "cancelled", `False ])
  | "change_cwd" ->
    let cwd = arg "cwd" in
    quietly t (fun () -> call t "set_cwd" [ "path", str cwd ])
    |> Or_error.map ~f:(fun _ ->
      refresh_state t;
      `Object [ "cancelled", `False; "cwd", str cwd ])
  | "list_sessions" ->
    Or_error.map (call t "list_sessions" []) ~f:(fun sessions ->
      let current =
        Option.value_map t.last_state ~default:`Null ~f:(fun s ->
          field "session_id" s)
      in
      `Object [ "sessions", sessions; "current", current ])
  | "switch_session" -> unit_ok (switch_session t (arg "path"))
  | "watch_subagent" ->
    (match
       opt_string_field "agentId" json, opt_string_field "toolCallId" json
     with
     | Some key, _ | None, Some key -> watch_subagent t key
     | None, None ->
       t.watch <- None;
       Ok (`Object []))
  | "" -> Or_error.error_string "command must have a string \"type\""
  | command -> Or_error.errorf "command %S is not supported by prigh" command
;;

let ui_response t json =
  let id = string_field "id" json in
  match Hashtbl.find_and_remove t.dialogs id with
  | Some f -> f json
  | None -> ()
;;

let handle t json =
  match string_field "type" json with
  | "extension_ui_response" -> ui_response t json
  | command ->
    let result =
      match run_command t json with
      | result -> result
      | exception exn ->
        Or_error.error_s [%message "internal error" (exn : exn)]
    in
    t.write (P.response ~id:(field "id" json) ~command result)
;;

let default_now () = Float.to_int (Core_unix.gettimeofday () *. 1000.)

let serve_lines
      server
      ?(now = default_now)
      ?token
      ?user
      ?session
      ?(name = "pi-web")
      ~read_line
      ~write_line
      ()
  =
  Switch.run
  @@ fun sw ->
  let outbox : string option Eio.Stream.t = Eio.Stream.create 1024 in
  let write json = Eio.Stream.add outbox (Some (Json.to_string json)) in
  Fiber.fork ~sw (fun () ->
    let rec loop () =
      match Eio.Stream.take outbox with
      | None -> ()
      | Some line ->
        (match write_line line with
         | () -> loop ()
         | exception _ -> ())
    in
    loop ());
  let t_ref = ref None in
  let client =
    Rpc_server.connect server ~send:(fun json ->
      Option.iter !t_ref ~f:(fun t -> on_event t json))
  in
  let t =
    { server
    ; client
    ; write
    ; now
    ; seq = 0
    ; dialog_seq = 0
    ; next_ts = 0
    ; current_ts = 0
    ; last_assistant_ts = 0
    ; result_ts = String.Table.create ()
    ; tools = String.Table.create ()
    ; widget_runs = None
    ; watch = None
    ; messages_session = None
    ; last_state = None
    ; quiet_state_changes = 0
    ; dialogs = String.Table.create ()
    }
  in
  let hello =
    call
      t
      "hello"
      ([ "name", str name; "tools", `False ]
       @ Option.value_map token ~default:[] ~f:(fun v -> [ "token", str v ])
       @ Option.value_map user ~default:[] ~f:(fun v -> [ "user", str v ])
       @ Option.value_map session ~default:[] ~f:(fun v -> [ "session", str v ])
      )
  in
  (match hello with
   | Error e ->
     write
       (P.event "prigh_hello_failed" [ "error", str (Error.to_string_hum e) ])
   | Ok result ->
     apply_state t (field "state" result);
     t_ref := Some t;
     let rec loop () =
       match read_line () with
       | None -> ()
       | Some line ->
         if not (String.is_empty (String.strip line))
         then (
           match Json.parse line with
           | Error e ->
             write
               (P.response
                  ~id:`Null
                  ~command:"invalid"
                  (Or_error.errorf "invalid JSON: %s" (Error.to_string_hum e)))
           | Ok json -> Fiber.fork ~sw (fun () -> handle t json));
         loop ()
     in
     loop ());
  Rpc_server.disconnect server client;
  Eio.Stream.add outbox None
;;

let serve_websocket router ~query ws =
  let param name = List.Assoc.find query ~equal:String.equal name in
  let token = param "token" in
  let user = param "user" in
  match Rpc_router.lookup router ?user ~token () with
  | None ->
    Websocket.send_text
      ws
      (Json.to_string
         (P.event
            "prigh_hello_failed"
            [ "error", `String Rpc_server.unauthorised ]))
  | Some server ->
    serve_lines
      server
      ?token
      ?user
      ?session:(param "session")
      ?name:(param "name")
      ~read_line:(fun () -> Websocket.read_text ws)
      ~write_line:(Websocket.send_text ws)
      ()
;;
