open! Core
open! Import
module Error = Json_rpc.Error
module Reply = Json_rpc.Reply

module Tool_host_config = struct
  type t =
    { host_id : string
    ; name : string
    ; cwd : string
    }
end

module Config = struct
  type t =
    { connector : Rpc_client.Connector.t
    ; token : string option
    ; user : string option
    ; tool_host : Tool_host_config.t option
    }
end

let str s = `String s
let int i = `Number (Int.to_string i)
let bool b = if b then `True else `False
let field name json = Option.value (Json.member name json) ~default:`Null

let string_field name json =
  Option.value (Json.string (field name json)) ~default:""
;;

let opt_string name json = Json.string (field name json)

let list_field name json =
  Option.value (Json.list (field name json)) ~default:[]
;;

let is_true name json = Json.exactly_equal (field name json) `True

(* What the client said it can do in [initialize]. *)
module Caps = struct
  type t =
    { read_text_file : bool
    ; write_text_file : bool
    ; elicitation_form : bool
    ; notices : bool
    ; prigh_state : bool (** [_meta.prigh.state]: it reads [_prigh/state] *)
    }

  let none =
    { read_text_file = false
    ; write_text_file = false
    ; elicitation_form = false
    ; notices = false
    ; prigh_state = false
    }
  ;;

  let present path json =
    match List.fold path ~init:json ~f:(fun json key -> field key json) with
    | `Null | `False -> false
    | _ -> true
  ;;

  let of_json caps =
    { read_text_file = is_true "readTextFile" (field "fs" caps)
    ; write_text_file = is_true "writeTextFile" (field "fs" caps)
    ; elicitation_form = present [ "elicitation"; "form" ] caps
    ; notices = present [ "session"; "notices" ] caps
    ; prigh_state = present [ "_meta"; "prigh"; "state" ] caps
    }
  ;;
end

(* A tool call the client has been told about. *)
module Tool_view = struct
  type t =
    { name : string
    ; mutable args : Json.t
    ; output : Buffer.t
    ; mutable progress : string list
      (** a subagent's tool calls, newest first *)
    }

  let create name =
    { name; args = `Object []; output = Buffer.create 0; progress = [] }
  ;;
end

module Stop = struct
  type t =
    | End_turn
    | Max_tokens
    | Aborted
    | Failed of string

  let of_message message =
    let reason = field "stop_reason" message in
    match string_field "type" reason with
    | "length" -> Max_tokens
    | "aborted" -> Aborted
    | "error" -> Failed (string_field "message" reason)
    | _ -> End_turn
  ;;
end

(* A [session/prompt] waiting for the session to stop running. *)
module Turn = struct
  type t =
    { mutable running_seen : bool
    ; mutable cancelled : bool
    ; mutable stop : Stop.t
    ; finished : unit Promise.t
    ; finish : unit Promise.u
    }

  let create ~running =
    let finished, finish = Promise.create () in
    { running_seen = running
    ; cancelled = false
    ; stop = End_turn
    ; finished
    ; finish
    }
  ;;

  let finish t =
    if not (Promise.is_resolved t.finished) then Promise.resolve t.finish ()
  ;;
end

(* What the client was last told, so that only changes are sent. *)
module Sent = struct
  type t =
    { mutable mode : string
    ; mutable config : Json.t
    ; mutable plan : Json.t
    ; mutable usage : Json.t
    ; mutable title : Json.t
    ; mutable prigh_state : Json.t
    }
end

module Session = struct
  type t =
    { id : string
    ; link : Rpc_client.t
    ; events : Json.t option Eio.Stream.t
    ; mutable state : Json.t
    ; mutable queued : int * int
    ; sent : Sent.t
    ; tools : Tool_view.t String.Table.t
    ; diffs : Json.t list String.Table.t
      (** per tool call, the files it wrote (newest first), seen by this
          tool host *)
    ; mutable turn : Turn.t option
    ; open_requests : Json.t String.Table.t
      (** our permission and question requests not answered yet *)
    ; mutable closed : bool
    ; mutable own_prompt : bool
      (** the prompt we sent has not started yet: its message is not echoed *)
    }

  let running t = is_true "running" t.state
  let cwd t = string_field "cwd" t.state
end

module Host = struct
  type t =
    { link : Rpc_client.t
    ; worker : Tool_host.Worker.t
    }
end

type t =
  { env : Env.t
  ; sw : Switch.t
  ; rpc : Json_rpc.t
  ; config : Config.t
  ; mutable caps : Caps.t
  ; mutable client_name : string
  ; sessions : Session.t String.Table.t
  ; mutable host : Host.t option
  ; mutable control : Rpc_client.t option
  ; mutable models : Json.t list option
  ; log : string -> unit
  }

let log t fmt = ksprintf t.log fmt

let update t (s : Session.t) fields =
  if not s.closed
  then
    Json_rpc.notify
      t.rpc
      "session/update"
      (`Object [ "sessionId", str s.id; "update", `Object fields ])
;;

let text_block text = `Object [ "type", str "text"; "text", str text ]

let message_chunk t s ?(kind = "agent_message_chunk") text =
  if not (String.is_empty text)
  then update t s [ "sessionUpdate", str kind; "content", text_block text ]
;;

(* Errors from prigh name the TUI's slash commands; in an editor the CLI
   (or the TUI) is where one logs in. *)
let login_hint message =
  if String.is_substring message ~substring:"/login "
  then
    String.substr_replace_all message ~pattern:"/login " ~with_:"prigh login "
    ^ " (in a terminal; or /login in the prigh TUI)"
  else message
;;

let needs_login message = String.is_substring message ~substring:"/login "
let rpc_error ?(code = Error.internal) e = code (Core.Error.to_string_hum e)

(* ---------------------------------------------------------------------- *)
(* Connections to prigh                                                    *)

let credentials t =
  List.filter_opt
    [ Option.map t.config.token ~f:(fun token -> "token", str token)
    ; Option.map t.config.user ~f:(fun user -> "user", str user)
    ]
;;

let client_label t = sprintf "%s (ACP)" t.client_name

let open_link t ~on_event ~on_close hello =
  Or_error.bind (t.config.connector ~on_event ~on_close) ~f:(fun link ->
    match Rpc_client.call link "hello" (credentials t @ hello) with
    | Ok result -> Ok (link, result)
    | Error e ->
      Rpc_client.close link;
      Error
        (if String.is_prefix (Core.Error.to_string_hum e) ~prefix:"unauthorised"
         then
           Core.Error.of_string
             (Core.Error.to_string_hum e
              ^ ": check prigh acp's -token (or $PRIGH_TOKEN) and -user \
                 against the backend's")
         else e))
;;

let diff_files t (s : Session.t) ~call_id =
  let cache = String.Table.create () in
  let read path =
    let contents =
      if t.caps.read_text_file
      then (
        match
          Json_rpc.request
            t.rpc
            "fs/read_text_file"
            (`Object [ "sessionId", str s.id; "path", str path ])
        with
        | Ok result -> string_field "content" result
        | Error e ->
          log
            t
            "fs/read_text_file %s failed (%s): reading the disk"
            path
            e.message;
          In_channel.read_all path)
      else In_channel.read_all path
    in
    Hashtbl.set cache ~key:path ~data:contents;
    contents
  in
  let write path ~data =
    let old_text =
      match Hashtbl.find cache path with
      | Some contents -> Some contents
      | None ->
        if Sys_unix.file_exists_exn path
        then Option.try_with (fun () -> read path)
        else None
    in
    if t.caps.write_text_file
    then (
      match
        Json_rpc.request
          t.rpc
          "fs/write_text_file"
          (`Object
              [ "sessionId", str s.id; "path", str path; "content", str data ])
      with
      | Ok _ -> ()
      | Error e -> failwithf "the editor did not write %s: %s" path e.message ())
    else Out_channel.write_all path ~data;
    Hashtbl.set cache ~key:path ~data;
    Option.iter call_id ~f:(fun call_id ->
      Hashtbl.add_multi
        s.diffs
        ~key:call_id
        ~data:(Acp_tool_call.diff ~path ~old_text ~new_text:data))
  in
  File_access.create ~read ~write
;;

let files t ~session_id ~call_id =
  match Option.bind session_id ~f:(Hashtbl.find t.sessions) with
  | Some s -> diff_files t s ~call_id
  | None -> File_access.local
;;

let host t =
  match t.host, t.config.tool_host with
  | Some host, _ -> Ok (Some host)
  | None, None -> Ok None
  | None, Some config ->
    let worker = ref None in
    Or_error.map
      (open_link
         t
         ~on_event:(fun json ->
           Option.iter !worker ~f:(fun w ->
             Tool_host.Worker.handle_event w json))
         ~on_close:(fun reason ->
           log t "tool host connection closed: %s" reason)
         [ "name", str config.name
         ; "cwd", str config.cwd
         ; "tools", `True
         ; "host_id", str config.host_id
         ; "features", `Array (List.map Tool_host.features ~f:str)
         ])
      ~f:(fun (link, _) ->
        let w =
          Tool_host.Worker.create
            ~env:t.env
            ~sw:t.sw
            ~terminals:(lazy (Terminals.create ~env:t.env ~sw:t.sw ()))
            ~mcp:(Mcp_hub.create ~env:t.env ~sw:t.sw ())
            ~default_cwd:config.cwd
            ~files:(files t)
            ~send:(fun reply ->
              match
                Rpc_client.call
                  link
                  (Tool_host.Reply.method_ reply)
                  (Tool_host.Reply.fields reply)
              with
              | Ok _ -> ()
              | Error e ->
                let message = Core.Error.to_string_hum e in
                (* The result of an exec the backend gave up on (aborted). *)
                if
                  not
                    (String.is_substring message ~substring:"no tool execution")
                then
                  log t "%s failed: %s" (Tool_host.Reply.method_ reply) message)
            ()
        in
        worker := Some w;
        let host = { Host.link; worker = w } in
        t.host <- Some host;
        Some host)
;;

(* For requests about no session in particular (listing sessions and
   models). *)
let control t =
  Or_error.bind (host t) ~f:(function
    | Some host -> Ok host.link
    | None ->
      (match t.control with
       | Some link -> Ok link
       | None ->
         Or_error.map
           (open_link
              t
              ~on_event:ignore
              ~on_close:(fun _ -> t.control <- None)
              [ "name", str (client_label t) ])
           ~f:(fun (link, _) ->
             t.control <- Some link;
             link)))
;;

let call (s : Session.t) meth params = Rpc_client.call s.link meth params

(* ---------------------------------------------------------------------- *)
(* Modes and config options                                                *)

let modes =
  [ "ask", "Ask", "ask before every tool that changes something"
  ; "auto-edit", "Auto-edit", "edit files without asking; ask before commands"
  ; "auto", "Auto", "run every tool without asking"
  ; ( "plan"
    , "Plan"
    , "investigate and plan, changing no files, until you approve the plan" )
  ]
;;

let mode_id state =
  if is_true "plan_mode" state
  then "plan"
  else string_field "approval_mode" state
;;

let mode_state state =
  `Object
    [ "currentModeId", str (mode_id state)
    ; ( "availableModes"
      , `Array
          (List.map modes ~f:(fun (id, name, description) ->
             `Object
               [ "id", str id
               ; "name", str name
               ; "description", str description
               ])) )
    ]
;;

let thinking_levels = [ "off"; "on"; "low"; "high"; "max" ]

(* The models of the providers prigh has credentials for. *)
let refresh_models t =
  match control t with
  | Error _ -> ()
  | Ok link ->
    (match
       ( Rpc_client.call link "list_models" []
       , Rpc_client.call link "auth_status" [] )
     with
     | Ok (`Array models), Ok (`Array statuses) ->
       let configured =
         List.filter_map statuses ~f:(fun s ->
           match field "configured" s with
           | `Null -> None
           | _ -> Some (string_field "provider" s))
       in
       t.models
       <- Some
            (List.filter models ~f:(fun m ->
               List.mem
                 configured
                 (string_field "provider" m)
                 ~equal:String.equal))
     | _ -> ())
;;

let select ~id ~name ~category ~current options =
  `Object
    [ "id", str id
    ; "name", str name
    ; "category", str category
    ; "type", str "select"
    ; "currentValue", str current
    ; "options", `Array options
    ]
;;

let option ?description ~value name =
  `Object
    ([ "value", str value; "name", str name ]
     @ Option.value_map description ~default:[] ~f:(fun d ->
       [ "description", str d ]))
;;

let config_options t state =
  let current_model = field "model" state in
  let models =
    let listed = Option.value t.models ~default:[] in
    let key m = string_field "key" m in
    if List.exists listed ~f:(fun m -> String.equal (key m) (key current_model))
    then listed
    else current_model :: listed
  in
  `Array
    [ select
        ~id:"mode"
        ~name:"Mode"
        ~category:"mode"
        ~current:(mode_id state)
        (List.map modes ~f:(fun (value, name, description) ->
           option ~value ~description name))
    ; select
        ~id:"model"
        ~name:"Model"
        ~category:"model"
        ~current:(string_field "key" current_model)
        (List.map models ~f:(fun m ->
           option
             ~value:(string_field "key" m)
             ~description:(string_field "key" m)
             (string_field "name" m)))
    ; select
        ~id:"thinking"
        ~name:"Thinking"
        ~category:"thought_level"
        ~current:(string_field "thinking" state)
        (List.map thinking_levels ~f:(fun level -> option ~value:level level))
    ]
;;

(* ---------------------------------------------------------------------- *)
(* Derived updates: plan, mode, options, usage, title, _prigh/state        *)

let plan_entries state =
  match field "plan" state, field "goal" state with
  | (`Object _ as plan), _ ->
    List.map (list_field "steps" plan) ~f:(fun step ->
      `Object
        [ "content", str (string_field "step" step)
        ; "priority", str "medium"
        ; "status", str (string_field "status" step)
        ])
  | _, (`Object _ as goal)
    when String.equal (string_field "status" goal) "active" ->
    [ `Object
        [ "content", str ("Goal: " ^ string_field "text" goal)
        ; "priority", str "high"
        ; "status", str "in_progress"
        ]
    ]
  | _ -> []
;;

let usage state =
  `Object
    [ "used", field "context_tokens" state
    ; "size", field "context_window" (field "model" state)
    ; ( "cost"
      , `Object [ "amount", field "cost_usd" state; "currency", str "USD" ] )
    ]
;;

let prigh_state (s : Session.t) =
  let st = s.state in
  let model = field "model" st in
  let active = string_field "active_host" st in
  let host_name =
    List.find_map (list_field "hosts" st) ~f:(fun h ->
      if String.equal (string_field "id" h) active
      then opt_string "name" h
      else None)
  in
  let steer, follow_up = s.queued in
  let count name = int (List.length (list_field name st)) in
  `Object
    [ "sessionId", str s.id
    ; "model", field "key" model
    ; "thinking", field "thinking" st
    ; "running", field "running" st
    ; "context_tokens", field "context_tokens" st
    ; "context_window", field "context_window" model
    ; "cost_usd", field "cost_usd" st
    ; "budget_usd", field "budget_usd" st
    ; "approval_mode", field "approval_mode" st
    ; "plan_mode", field "plan_mode" st
    ; "queued", `Object [ "steer", int steer; "follow_up", int follow_up ]
    ; ( "subagents"
      , `Array
          (List.map (list_field "subagents" st) ~f:(fun a ->
             `Object
               [ "id", field "id" a
               ; "label", field "task" a
               ; "running", field "running" a
               ])) )
    ; "jobs", count "jobs"
    ; ( "goal"
      , match field "goal" st with
        | `Object _ as goal
          when String.equal (string_field "status" goal) "active" ->
          field "text" goal
        | _ -> `Null )
    ; "reminders", count "reminders"
    ; "active_host", Option.value_map host_name ~default:(str active) ~f:str
    ]
;;

let send_prigh_state t (s : Session.t) =
  let state = prigh_state s in
  if not (Json.exactly_equal state s.sent.prigh_state)
  then (
    s.sent.prigh_state <- state;
    if t.caps.prigh_state && not s.closed
    then Json_rpc.notify t.rpc "_prigh/state" state)
;;

let send_derived t (s : Session.t) =
  let state = s.state in
  let mode = mode_id state in
  if not (String.equal mode s.sent.mode)
  then (
    s.sent.mode <- mode;
    update
      t
      s
      [ "sessionUpdate", str "current_mode_update"; "currentModeId", str mode ]);
  let config = config_options t state in
  if not (Json.exactly_equal config s.sent.config)
  then (
    s.sent.config <- config;
    update
      t
      s
      [ "sessionUpdate", str "config_option_update"; "configOptions", config ]);
  let plan = `Array (plan_entries state) in
  if not (Json.exactly_equal plan s.sent.plan)
  then (
    s.sent.plan <- plan;
    update t s [ "sessionUpdate", str "plan"; "entries", plan ]);
  let usage = usage state in
  if not (Json.exactly_equal usage s.sent.usage)
  then (
    s.sent.usage <- usage;
    update
      t
      s
      (("sessionUpdate", str "usage_update")
       ::
       (match usage with
        | `Object fields -> fields
        | _ -> [])));
  let title = field "session_name" state in
  if not (Json.exactly_equal title s.sent.title)
  then (
    s.sent.title <- title;
    update t s [ "sessionUpdate", str "session_info_update"; "title", title ]);
  send_prigh_state t s
;;

(* ---------------------------------------------------------------------- *)
(* Tool calls                                                              *)

let tool_fields (s : Session.t) (view : Tool_view.t) ~call_id =
  let cwd = Session.cwd s in
  [ "toolCallId", str call_id
  ; "title", str (Acp_tool_call.title ~name:view.name ~args:view.args ~cwd)
  ; "kind", str (Acp_tool_call.kind view.name)
  ]
;;

let announce t (s : Session.t) ~call_id ~name =
  match Hashtbl.find s.tools call_id with
  | Some view -> view
  | None ->
    let view = Tool_view.create name in
    Hashtbl.set s.tools ~key:call_id ~data:view;
    update
      t
      s
      ((("sessionUpdate", str "tool_call") :: tool_fields s view ~call_id)
       @ [ "status", str "pending" ]);
    view
;;

let tool_start t (s : Session.t) call =
  let call_id = string_field "id" call in
  let name = string_field "name" call in
  let args = Acp_tool_call.parse_args (string_field "arguments" call) in
  let kind, view =
    match Hashtbl.find s.tools call_id with
    | Some view -> "tool_call_update", view
    | None ->
      let view = Tool_view.create name in
      Hashtbl.set s.tools ~key:call_id ~data:view;
      "tool_call", view
  in
  view.args <- args;
  let locations = Acp_tool_call.locations ~name ~args ~cwd:(Session.cwd s) in
  update
    t
    s
    ((("sessionUpdate", str kind) :: tool_fields s view ~call_id)
     @ [ "status", str "in_progress"; "rawInput", args ]
     @ if List.is_empty locations then [] else [ "locations", `Array locations ]
    )
;;

let output_tail = 8192

let tool_output t (s : Session.t) ~call_id chunk =
  Option.iter (Hashtbl.find s.tools call_id) ~f:(fun view ->
    Buffer.add_string view.output chunk;
    let output = Buffer.contents view.output in
    let output =
      if String.length output > output_tail
      then "…" ^ String.suffix output output_tail
      else output
    in
    update
      t
      s
      [ "sessionUpdate", str "tool_call_update"
      ; "toolCallId", str call_id
      ; ( "content"
        , `Array
            [ Acp_tool_call.text ~kind:(Acp_tool_call.kind view.name) output ] )
      ])
;;

let tool_end t (s : Session.t) ~call ~result =
  let call_id = string_field "id" call in
  let name = string_field "name" call in
  let args = Acp_tool_call.parse_args (string_field "arguments" call) in
  Hashtbl.remove s.tools call_id;
  let is_error = is_true "is_error" result in
  let text = string_field "text" result in
  let recorded =
    List.rev
      (Option.value (Hashtbl.find_and_remove s.diffs call_id) ~default:[])
  in
  let kind = Acp_tool_call.kind name in
  let content =
    if is_error
    then [ Acp_tool_call.text text ]
    else if Acp_tool_call.edits_files name
    then (
      match recorded with
      | _ :: _ -> recorded
      | [] ->
        (match Acp_tool_call.diffs_of_args ~name ~args ~cwd:(Session.cwd s) with
         | [] -> [ Acp_tool_call.text text ]
         | diffs -> diffs))
    else [ Acp_tool_call.text ~kind text ]
  in
  update
    t
    s
    [ "sessionUpdate", str "tool_call_update"
    ; "toolCallId", str call_id
    ; "status", str (if is_error then "failed" else "completed")
    ; "content", `Array content
    ]
;;

let subagent_progress t (s : Session.t) ~call_id ~line =
  Option.iter (Hashtbl.find s.tools call_id) ~f:(fun view ->
    view.progress <- List.take (line :: view.progress) 12;
    update
      t
      s
      [ "sessionUpdate", str "tool_call_update"
      ; "toolCallId", str call_id
      ; ( "content"
        , `Array
            [ Acp_tool_call.text (String.concat_lines (List.rev view.progress))
            ] )
      ])
;;

(* ---------------------------------------------------------------------- *)
(* Confirmations and questions                                             *)

let ask t (s : Session.t) ~key method_ params =
  let result =
    Json_rpc.request
      ~on_id:(fun id -> Hashtbl.set s.open_requests ~key ~data:id)
      t.rpc
      method_
      params
  in
  (* Answered elsewhere (another frontend, a timeout) meanwhile: ours no
     longer counts. *)
  if Hashtbl.mem s.open_requests key
  then (
    Hashtbl.remove s.open_requests key;
    Some result)
  else None
;;

let withdraw t (s : Session.t) ~key =
  Option.iter (Hashtbl.find_and_remove s.open_requests key) ~f:(fun id ->
    Json_rpc.cancel_request t.rpc id)
;;

let selected_option = function
  | Ok result ->
    let outcome = field "outcome" result in
    if String.equal (string_field "outcome" outcome) "selected"
    then opt_string "optionId" outcome
    else None
  | Error (_ : Error.t) -> None
;;

let report_failure t what = function
  | Ok (_ : Json.t) -> ()
  | Error e -> log t "%s failed: %s" what (Core.Error.to_string_hum e)
;;

let confirm t (s : Session.t) json =
  let call_id = string_field "call_id" json in
  let agent_id = opt_string "agent_id" json in
  let name = string_field "name" json in
  let summary = string_field "summary" json in
  let rules = List.filter_map (list_field "rules" json) ~f:Json.string in
  let view = Hashtbl.find s.tools call_id in
  let tool_call =
    match view with
    | Some view ->
      tool_fields s view ~call_id
      @ [ "status", str "pending"; "rawInput", view.args ]
    | None ->
      [ "toolCallId", str call_id
      ; "title", str summary
      ; "kind", str (Acp_tool_call.kind name)
      ; "status", str "pending"
      ]
  in
  let options =
    [ Some
        (`Object
            [ "optionId", str "allow"
            ; "name", str "Allow"
            ; "kind", str "allow_once"
            ])
    ; (if List.is_empty rules
       then None
       else
         Some
           (`Object
               [ "optionId", str "always"
               ; "name", str ("Always allow " ^ String.concat ~sep:", " rules)
               ; "kind", str "allow_always"
               ]))
    ; Some
        (`Object
            [ "optionId", str "reject"
            ; "name", str "Reject"
            ; "kind", str "reject_once"
            ])
    ]
  in
  match
    ask
      t
      s
      ~key:("confirm:" ^ call_id)
      "session/request_permission"
      (`Object
          [ "sessionId", str s.id
          ; "toolCall", `Object tool_call
          ; "options", `Array (List.filter_opt options)
          ])
  with
  | None -> ()
  | Some result ->
    let allow, always =
      match selected_option result with
      | Some "allow" -> true, false
      | Some "always" -> true, true
      | Some _ | None -> false, false
    in
    call
      s
      "tool_confirm_respond"
      ([ "call_id", str call_id; "allow", bool allow; "always", bool always ]
       @ Option.value_map agent_id ~default:[] ~f:(fun id ->
         [ "agent_id", str id ]))
    |> report_failure t "tool_confirm_respond"
;;

let question t (s : Session.t) json =
  let call_id = string_field "call_id" json in
  let key = "ask:" ^ call_id in
  let respond params =
    call s "ask_user_respond" (("call_id", str call_id) :: params)
    |> report_failure t "ask_user_respond"
  in
  let decline = [ "decline", `True ] in
  match Acp_question.of_event (field "questions" json) with
  | Error e ->
    respond
      [ ( "unavailable"
        , str ("unreadable questions: " ^ Core.Error.to_string_hum e) )
      ]
  | Ok questions ->
    if t.caps.elicitation_form
    then (
      match
        ask
          t
          s
          ~key
          "elicitation/create"
          (`Object
              [ "sessionId", str s.id
              ; "toolCallId", str call_id
              ; "mode", str "form"
              ; "message", str (Acp_question.message questions)
              ; "requestedSchema", Acp_question.schema questions
              ])
      with
      | None -> ()
      | Some (Ok result)
        when String.equal (string_field "action" result) "accept" ->
        respond
          [ ( "answers"
            , `Array
                (List.map
                   (Acp_question.answers questions (field "content" result))
                   ~f:(fun answer -> `Array (List.map answer ~f:str))) )
          ]
      | Some (Ok _ | Error _) -> respond decline)
    else (
      match Acp_question.single_choice questions with
      | Some choices ->
        let options =
          List.mapi choices ~f:(fun i (c : User_question.Choice.t) ->
            `Object
              [ "optionId", str (sprintf "choice-%d" i)
              ; ( "name"
                , str
                    (match c.description with
                     | Some d -> sprintf "%s: %s" c.label d
                     | None -> c.label) )
              ; "kind", str "allow_once"
              ])
          @ [ `Object
                [ "optionId", str "decline"
                ; "name", str "Decline to answer"
                ; "kind", str "reject_once"
                ]
            ]
        in
        (match
           ask
             t
             s
             ~key
             "session/request_permission"
             (`Object
                 [ "sessionId", str s.id
                 ; ( "toolCall"
                   , `Object
                       [ "toolCallId", str call_id
                       ; "title", str (Acp_question.message questions)
                       ; "kind", str "other"
                       ; "status", str "pending"
                       ] )
                 ; "options", `Array options
                 ])
         with
         | None -> ()
         | Some result ->
           (match
              Option.bind (selected_option result) ~f:(fun id ->
                Option.bind
                  (String.chop_prefix id ~prefix:"choice-")
                  ~f:Int.of_string_opt)
            with
            | Some i when i >= 0 && i < List.length choices ->
              respond
                [ ( "answers"
                  , `Array [ `Array [ str (List.nth_exn choices i).label ] ] )
                ]
            | Some _ | None -> respond decline))
      | None ->
        respond
          [ ( "unavailable"
            , str
                (sprintf
                   "%s cannot show these questions (it has no forms): decide \
                    yourself, or ask them in your reply"
                   t.client_name) )
          ])
;;

(* ---------------------------------------------------------------------- *)
(* Session events                                                          *)

let is_injected message =
  match field "source" (field "origin" message) with
  | `Object _ -> true
  | _ -> false
;;

(* What a user message shows: what was typed (without the attachments), or
   what prigh injected. *)
let user_text message =
  let origin = field "origin" message in
  match field "source" origin, opt_string "body" origin with
  | (`Object _ as source), _ -> Some (string_field "title" source)
  | _, Some body -> Some body
  | _, None ->
    (match origin with
     | `Object _ -> None
     | _ -> Some (string_field "text" message))
;;

let finish_turn (s : Session.t) = Option.iter s.turn ~f:Turn.finish

let handle_event t (s : Session.t) json =
  match string_field "event" json with
  | "message_update" ->
    let delta = field "delta" json in
    (match string_field "type" delta with
     | "text_delta" -> message_chunk t s (string_field "text" delta)
     | "thinking_delta" ->
       message_chunk t s ~kind:"agent_thought_chunk" (string_field "text" delta)
     | "tool_call_start" ->
       ignore
         (announce
            t
            s
            ~call_id:(string_field "id" delta)
            ~name:(string_field "name" delta)
          : Tool_view.t)
     | _ -> ())
  | "message_start" ->
    let message = field "message" json in
    if String.equal (string_field "role" message) "user"
    then
      if s.own_prompt && not (is_injected message)
      then s.own_prompt <- false
      else
        (* From another frontend, or prigh's (a reminder, a goal's
           continuation). *)
        Option.iter
          (user_text message)
          ~f:(message_chunk t s ~kind:"user_message_chunk")
  | "message_end" ->
    let message = field "message" json in
    if String.equal (string_field "role" message) "assistant"
    then
      Option.iter s.turn ~f:(fun turn -> turn.stop <- Stop.of_message message)
  | "tool_start" -> tool_start t s (field "call" json)
  | "tool_output" ->
    tool_output
      t
      s
      ~call_id:(string_field "call_id" json)
      (string_field "chunk" json)
  | "tool_end" ->
    tool_end t s ~call:(field "call" json) ~result:(field "result" json)
  | "tool_confirm" -> Fiber.fork ~sw:t.sw (fun () -> confirm t s json)
  | "tool_confirm_done" ->
    withdraw t s ~key:("confirm:" ^ string_field "call_id" json)
  | "ask_user" -> Fiber.fork ~sw:t.sw (fun () -> question t s json)
  | "ask_user_done" -> withdraw t s ~key:("ask:" ^ string_field "call_id" json)
  | "subagent_start" ->
    subagent_progress
      t
      s
      ~call_id:(string_field "call_id" json)
      ~line:(sprintf "subagent %s started" (string_field "agent_id" json))
  | "subagent" ->
    let inner = field "inner" json in
    if String.equal (string_field "event" inner) "tool_start"
    then (
      let call = field "call" inner in
      let name = string_field "name" call in
      subagent_progress
        t
        s
        ~call_id:(string_field "call_id" json)
        ~line:
          ("· "
           ^ Acp_tool_call.title
               ~name
               ~args:(Acp_tool_call.parse_args (string_field "arguments" call))
               ~cwd:(Session.cwd s)))
  | "state" ->
    s.state <- field "state" json;
    Option.iter s.turn ~f:(fun turn ->
      if Session.running s
      then turn.running_seen <- true
      else if turn.running_seen
      then Turn.finish turn);
    send_derived t s
  | "queue_update" ->
    s.queued
    <- ( Option.value (Json.int (field "steer" json)) ~default:0
       , Option.value (Json.int (field "follow_up" json)) ~default:0 );
    send_prigh_state t s
  | "notice" ->
    let text = string_field "text" json in
    if t.caps.prigh_state
    then
      Json_rpc.notify
        t.rpc
        "_prigh/notice"
        (`Object [ "sessionId", str s.id; "text", str text ])
    else if t.caps.notices
    then
      update
        t
        s
        [ "sessionUpdate", str "notice"
        ; "severity", str "info"
        ; "title", str text
        ]
    else log t "notice: %s" text
  | "$started" ->
    Option.iter s.turn ~f:(fun turn ->
      if not turn.running_seen then Turn.finish turn)
  | "$closed" ->
    Option.iter s.turn ~f:(fun turn ->
      turn.stop <- Failed (string_field "reason" json);
      Turn.finish turn)
  | _ -> ()
;;

let run_events t (s : Session.t) =
  Fiber.fork ~sw:t.sw (fun () ->
    let rec loop () =
      match Eio.Stream.take s.events with
      | None -> ()
      | Some json ->
        (try handle_event t s json with
         | Eio.Cancel.Cancelled _ as exn -> raise exn
         | exn ->
           log
             t
             "handling %s failed: %s"
             (Json.to_string json)
             (Exn.to_string exn));
        loop ()
    in
    loop ())
;;

(* Replays [session/load]'s history. *)
let replay t (s : Session.t) messages =
  let calls = String.Table.create () in
  List.iter messages ~f:(fun message ->
    match string_field "role" message with
    | "user" ->
      Option.iter
        (user_text message)
        ~f:(message_chunk t s ~kind:"user_message_chunk")
    | "assistant" ->
      List.iter (list_field "content" message) ~f:(fun block ->
        match string_field "type" block with
        | "text" -> message_chunk t s (string_field "text" block)
        | "thinking" ->
          message_chunk
            t
            s
            ~kind:"agent_thought_chunk"
            (string_field "text" block)
        | "tool_call" ->
          let call_id = string_field "id" block in
          let name = string_field "name" block in
          let args =
            Acp_tool_call.parse_args (string_field "arguments" block)
          in
          Hashtbl.set calls ~key:call_id ~data:(name, args);
          let view = { (Tool_view.create name) with args } in
          let locations =
            Acp_tool_call.locations ~name ~args ~cwd:(Session.cwd s)
          in
          update
            t
            s
            ((("sessionUpdate", str "tool_call") :: tool_fields s view ~call_id)
             @ [ "status", str "in_progress"; "rawInput", args ]
             @
             if List.is_empty locations
             then []
             else [ "locations", `Array locations ])
        | _ -> ())
    | "tool_result" ->
      let call_id = string_field "tool_call_id" message in
      let name, args =
        Option.value
          (Hashtbl.find calls call_id)
          ~default:(string_field "tool_name" message, `Object [])
      in
      tool_end
        t
        s
        ~call:
          (`Object
              [ "id", str call_id
              ; "name", str name
              ; "arguments", str (Json.to_string args)
              ])
        ~result:message
    | _ -> ())
;;

(* ---------------------------------------------------------------------- *)
(* Sessions                                                                *)

let unknown_session id =
  Error.resource_not_found
    (sprintf
       "unknown session %S: open one with session/new or session/load \
        (session/list lists them)"
       id)
;;

let session_param t params =
  match Json.member "sessionId" params with
  | Some (`String id) ->
    (match Hashtbl.find t.sessions id with
     | Some s -> Ok s
     | None -> Error (unknown_session id))
  | _ -> Error (Error.invalid_params "missing sessionId (a string)")
;;

let close_session (s : Session.t) =
  if not s.closed
  then (
    s.closed <- true;
    finish_turn s;
    Eio.Stream.add s.events None;
    Rpc_client.close s.link)
;;

(* Connects a prigh client to session [id] (or a fresh session) and
   registers it, its events waiting until [run_events]. *)
let attach t ?id ~cwd () =
  let events = Eio.Stream.create Int.max_value in
  let push json = Eio.Stream.add events (Some json) in
  Or_error.bind
    (open_link
       t
       ~on_event:push
       ~on_close:(fun reason ->
         log t "connection closed: %s" reason;
         push (`Object [ "event", str "$closed"; "reason", str reason ]))
       ([ "name", str (client_label t); "cwd", str cwd ]
        @ Option.value_map id ~default:[] ~f:(fun id -> [ "session", str id ])))
    ~f:(fun (link, hello) ->
      let state = field "state" hello in
      let id = string_field "session_id" state in
      Option.iter (Hashtbl.find t.sessions id) ~f:(fun old ->
        close_session old;
        Hashtbl.remove t.sessions id);
      let s =
        { Session.id
        ; link
        ; events
        ; state
        ; queued = 0, 0
        ; sent =
            { mode = mode_id state
            ; config = config_options t state
            ; plan = `Array (plan_entries state)
            ; usage = usage state
            ; title = field "session_name" state
            ; prigh_state = `Null
            }
        ; tools = String.Table.create ()
        ; diffs = String.Table.create ()
        ; turn = None
        ; open_requests = String.Table.create ()
        ; closed = false
        ; own_prompt = false
        }
      in
      Hashtbl.set t.sessions ~key:id ~data:s;
      Ok s)
;;

let session_result t (s : Session.t) extra =
  `Object
    (extra
     @ [ "modes", mode_state s.state
       ; "configOptions", config_options t s.state
       ])
;;

let available_commands t (s : Session.t) =
  let skills =
    match call s "list_skills" [] with
    | Ok result ->
      List.map (list_field "skills" result) ~f:(fun skill ->
        { Acp_commands.Command.name = string_field "name" skill
        ; description = string_field "description" skill
        ; hint = Some "[arguments]"
        })
    | Error _ -> []
  in
  update
    t
    s
    [ "sessionUpdate", str "available_commands_update"
    ; ( "availableCommands"
      , `Array
          (List.map
             (Acp_commands.builtin @ skills)
             ~f:Acp_commands.Command.to_json) )
    ]
;;

(* Once the client knows the session: its events flow, the commands and
   prigh's state are sent, and what waits for an answer is asked. *)
let start_session t (s : Session.t) =
  run_events t s;
  available_commands t s;
  send_prigh_state t s;
  match call s "get_pending" [] with
  | Ok pending ->
    List.iter (list_field "confirms" pending) ~f:(fun confirm ->
      Eio.Stream.add
        s.events
        (Some
           (`Object
               (("event", str "tool_confirm")
                ::
                (match confirm with
                 | `Object fields -> fields
                 | _ -> [])))));
    List.iter (list_field "questions" pending) ~f:(fun q ->
      Eio.Stream.add
        s.events
        (Some
           (`Object
               (("event", str "ask_user")
                ::
                (match q with
                 | `Object fields -> fields
                 | _ -> [])))))
  | Error _ -> ()
;;

let cwd_param params =
  match Json.member "cwd" params with
  | Some (`String cwd) when Filename.is_absolute cwd -> Ok cwd
  | Some (`String cwd) ->
    Error (Error.invalid_params (sprintf "cwd %S must be an absolute path" cwd))
  | _ -> Error (Error.invalid_params "missing cwd (an absolute path)")
;;

let warn_mcp t params =
  match Json.member "mcpServers" params with
  | Some (`Array (_ :: _ as servers)) ->
    log
      t
      "ignoring %d MCP server(s) from the client: prigh reads them from its \
       own config (see prigh's README, MCP servers)"
      (List.length servers)
  | _ -> ()
;;

let ( let* ) = Result.( >>= )

let or_internal = function
  | Ok x -> Ok x
  | Error e -> Error (rpc_error e)
;;

let new_session t params =
  let* cwd = cwd_param params in
  warn_mcp t params;
  refresh_models t;
  let* s =
    or_internal
      (Or_error.bind (host t) ~f:(function
         | Some host ->
           Or_error.bind
             (Rpc_client.call host.link "new_session" [])
             ~f:(fun result ->
               let state = field "state" result in
               let id = string_field "session_id" state in
               Or_error.bind
                 (if String.equal (string_field "cwd" state) cwd
                  then Ok `Null
                  else Rpc_client.call host.link "set_cwd" [ "path", str cwd ])
                 ~f:(fun (_ : Json.t) -> attach t ~id ~cwd ()))
         | None ->
           Or_error.bind (attach t ~cwd ()) ~f:(fun s ->
             if String.equal (Session.cwd s) cwd
             then Ok s
             else (
               match call s "set_cwd" [ "path", str cwd ] with
               | Ok _ -> Ok s
               | Error e ->
                 close_session s;
                 Hashtbl.remove t.sessions s.id;
                 Error e))))
  in
  (* [set_cwd] went through another client: the state is fetched again. *)
  Result.iter (call s "get_state" []) ~f:(fun state -> s.state <- state);
  s.sent.mode <- mode_id s.state;
  s.sent.config <- config_options t s.state;
  Ok
    (Reply.Result_then
       ( session_result t s [ "sessionId", str s.id ]
       , fun () -> start_session t s ))
;;

(* [session/load] and [session/resume]: the session is opened through the
   tool host first, so that it runs here if it ran on the backend. *)
let open_session t params ~replay:with_history =
  let* id =
    match Json.member "sessionId" params with
    | Some (`String id) -> Ok id
    | _ -> Error (Error.invalid_params "missing sessionId (a string)")
  in
  let* cwd = cwd_param params in
  warn_mcp t params;
  refresh_models t;
  let not_found e =
    let message = Core.Error.to_string_hum e in
    if
      String.is_substring message ~substring:"no session"
      || String.is_substring message ~substring:"unknown session"
      || String.is_substring message ~substring:"not found"
    then unknown_session id
    else Error.internal message
  in
  let* () =
    match host t with
    | Ok (Some host) ->
      (match Rpc_client.call host.link "switch_session" [ "path", str id ] with
       | Ok _ -> Ok ()
       | Error e -> Error (not_found e))
    | Ok None -> Ok ()
    | Error e -> Error (rpc_error e)
  in
  let* s =
    match attach t ~id ~cwd () with
    | Ok s -> Ok s
    | Error e -> Error (not_found e)
  in
  if with_history
  then (
    match call s "get_messages" [] with
    | Ok (`Array messages) -> replay t s messages
    | Ok _ -> ()
    | Error e -> log t "get_messages failed: %s" (Core.Error.to_string_hum e));
  Ok (Reply.Result_then (session_result t s [], fun () -> start_session t s))
;;

let first_line s =
  let line = Option.value (List.hd (String.split_lines s)) ~default:"" in
  if String.length line > 80 then String.prefix line 79 ^ "…" else line
;;

let iso_time s = String.substr_replace_first s ~pattern:" " ~with_:"T"

let list_sessions t params =
  let cwd = Option.bind (Json.member "cwd" params) ~f:Json.string in
  let* link = or_internal (control t) in
  let* listed = or_internal (Rpc_client.call link "list_sessions" []) in
  let sessions =
    Option.value (Json.list listed) ~default:[]
    |> List.filter ~f:(fun s ->
      (not (is_true "archived" s))
      && Option.value (Json.int (field "message_count" s)) ~default:0 > 0
      && Option.for_all cwd ~f:(String.equal (string_field "cwd" s)))
    |> List.map ~f:(fun s ->
      let title =
        match opt_string "name" s, opt_string "first_prompt" s with
        | Some name, _ -> Some name
        | None, Some prompt -> Some (first_line prompt)
        | None, None -> None
      in
      `Object
        ([ "sessionId", field "id" s; "cwd", field "cwd" s ]
         @ Option.value_map title ~default:[] ~f:(fun t -> [ "title", str t ])
         @ [ "updatedAt", str (iso_time (string_field "updated_at" s)) ]))
  in
  Ok (Reply.Result (`Object [ "sessions", `Array sessions ]))
;;

let close_session_request t params =
  let* s = session_param t params in
  close_session s;
  Hashtbl.remove t.sessions s.id;
  Ok (Reply.Result (`Object []))
;;

(* ---------------------------------------------------------------------- *)
(* Prompts                                                                 *)

let stop_reason = function
  | `End_turn -> str "end_turn"
  | `Max_tokens -> str "max_tokens"
  | `Cancelled -> str "cancelled"
;;

let stopped reason =
  Ok (Reply.Result (`Object [ "stopReason", stop_reason reason ]))
;;

let turn_error message =
  if needs_login message
  then Error.auth_required (login_hint message)
  else Error.internal message
;;

(* Starts a run with [start] and waits for the session to stop running. *)
let run_turn (s : Session.t) start =
  let turn = Turn.create ~running:false in
  s.turn <- Some turn;
  let clear () =
    match s.turn with
    | Some current when phys_equal current turn -> s.turn <- None
    | Some _ | None -> ()
  in
  match start () with
  | Error e ->
    clear ();
    Error (turn_error (Core.Error.to_string_hum e))
  | Ok (_ : Json.t) ->
    (* Behind the events of the run it started, if it started one. *)
    Eio.Stream.add s.events (Some (`Object [ "event", str "$started" ]));
    Promise.await turn.finished;
    clear ();
    if turn.cancelled
    then stopped `Cancelled
    else (
      match turn.stop with
      | End_turn -> stopped `End_turn
      | Max_tokens -> stopped `Max_tokens
      | Aborted -> stopped `Cancelled
      | Failed message -> Error (turn_error message))
;;

let send_prompt (s : Session.t) (p : Acp_prompt.t) =
  s.own_prompt <- true;
  let params =
    [ "text", str p.text ]
    @ (if List.is_empty p.attachments
       then []
       else [ "attachments", `Array (List.map p.attachments ~f:str) ])
    @ if List.is_empty p.images then [] else [ "images", `Array p.images ]
  in
  let result =
    match call s "prompt" params with
    | Error e
      when String.is_substring
             (Core.Error.to_string_hum e)
             ~substring:"a run is already in progress" ->
      (* Started from elsewhere (the TUI): ours runs after it. *)
      call s "follow_up" params
    | result -> result
  in
  if Result.is_error result then s.own_prompt <- false;
  result
;;

let prompt t params =
  let* s = session_param t params in
  let* blocks =
    match Json.member "prompt" params with
    | Some (`Array blocks) -> Ok blocks
    | _ ->
      Error (Error.invalid_params "missing prompt (an array of content blocks)")
  in
  let* p =
    Result.map_error (Acp_prompt.of_blocks blocks) ~f:(fun e ->
      Error.invalid_params (Core.Error.to_string_hum e))
  in
  let* () =
    match s.turn with
    | Some turn when not (Promise.is_resolved turn.finished) ->
      Error
        (Error.invalid_request
           "this session is already answering a prompt: wait for it to end, or \
            cancel it (session/cancel)")
    | Some _ | None -> Ok ()
  in
  let action =
    if List.is_empty p.attachments && List.is_empty p.images
    then Acp_commands.parse ~state:s.state p.text
    else Prompt
  in
  match action with
  | Prompt -> run_turn s (fun () -> send_prompt s p)
  | Reply text ->
    message_chunk t s text;
    stopped `End_turn
  | Call { method_; params; turn = true; ack = _ } ->
    run_turn s (fun () -> call s method_ params)
  | Call { method_; params; turn = false; ack } ->
    (match call s method_ params with
     | Ok result ->
       message_chunk t s (ack result);
       stopped `End_turn
     | Error e -> Error (turn_error (Core.Error.to_string_hum e)))
;;

let cancel t params =
  match session_param t params with
  | Error e -> log t "session/cancel: %s" e.message
  | Ok s ->
    Option.iter s.turn ~f:(fun turn -> turn.cancelled <- true);
    call s "abort" [] |> report_failure t "abort"
;;

(* ---------------------------------------------------------------------- *)
(* Modes and config options                                                *)

let mode_ids = List.map modes ~f:(fun (id, _, _) -> id)

let set_mode_params mode =
  match mode with
  | "plan" -> Ok [ "plan_mode", `True ]
  | mode when List.mem mode_ids mode ~equal:String.equal ->
    Ok [ "approval_mode", str mode; "plan_mode", `False ]
  | mode ->
    Error
      (Error.invalid_params
         (sprintf
            "unknown mode %S: the modes are %s"
            mode
            (String.concat ~sep:", " mode_ids)))
;;

let string_param name params =
  match Json.member name params with
  | Some (`String value) -> Ok value
  | _ -> Error (Error.invalid_params (sprintf "missing %s (a string)" name))
;;

let set_mode t params =
  let* s = session_param t params in
  let* mode = string_param "modeId" params in
  let* mode_params = set_mode_params mode in
  let* (_ : Json.t) = or_internal (call s "set_mode" mode_params) in
  Ok (Reply.Result (`Object []))
;;

let config_ids = [ "mode"; "model"; "thinking" ]

let set_config_option t params =
  let* s = session_param t params in
  let* id = string_param "configId" params in
  let* value = string_param "value" params in
  let* method_, method_params =
    match id with
    | "mode" -> Result.map (set_mode_params value) ~f:(fun p -> "set_mode", p)
    | "model" -> Ok ("set_model", [ "model", str value ])
    | "thinking" ->
      if List.mem thinking_levels value ~equal:String.equal
      then Ok ("set_thinking", [ "thinking", str value ])
      else
        Error
          (Error.invalid_params
             (sprintf
                "unknown thinking level %S: the levels are %s"
                value
                (String.concat ~sep:", " thinking_levels)))
    | id ->
      Error
        (Error.invalid_params
           (sprintf
              "unknown config option %S: prigh's are %s"
              id
              (String.concat ~sep:", " config_ids)))
  in
  let* (_ : Json.t) =
    Result.map_error (call s method_ method_params) ~f:(fun e ->
      Error.invalid_params (Core.Error.to_string_hum e))
  in
  let* state = or_internal (call s "get_state" []) in
  let config = config_options t state in
  (* The client has them now: they are not sent again when the state comes. *)
  s.sent.config <- config;
  Ok (Reply.Result (`Object [ "configOptions", config ]))
;;

(* ---------------------------------------------------------------------- *)
(* Initialization                                                          *)

let extensions = [ "_prigh/state"; "_prigh/notice" ]

let prigh_meta =
  `Object
    [ ( "prigh"
      , `Object
          [ "version", int 1
          ; "extensions", `Array (List.map extensions ~f:str)
          ] )
    ]
;;

let login_method_id = "prigh-login"

let login_advice =
  "run `prigh login` in a terminal (or /login in the prigh TUI), then try again"
;;

let initialize t params =
  let caps = field "clientCapabilities" params in
  t.caps <- Caps.of_json caps;
  let info = field "clientInfo" params in
  Option.iter
    (Option.first_some (opt_string "title" info) (opt_string "name" info))
    ~f:(fun name -> t.client_name <- name);
  Reply.Result
    (`Object
        [ "protocolVersion", int 1
        ; ( "agentCapabilities"
          , `Object
              [ "loadSession", `True
              ; ( "promptCapabilities"
                , `Object
                    [ "image", `True
                    ; "audio", `False
                    ; "embeddedContext", `True
                    ] )
              ; "mcpCapabilities", `Object [ "http", `False; "sse", `False ]
              ; ( "sessionCapabilities"
                , `Object
                    [ "list", `Object []
                    ; "resume", `Object []
                    ; "close", `Object []
                    ] )
              ; "_meta", prigh_meta
              ] )
        ; ( "authMethods"
          , `Array
              [ `Object
                  [ "id", str login_method_id
                  ; "name", str "prigh login"
                  ; ( "description"
                    , str ("Log in to a model provider: " ^ login_advice) )
                  ]
              ] )
        ; ( "agentInfo"
          , `Object
              [ "name", str "prigh"
              ; "title", str "prigh"
              ; "version", str Version.to_string
              ] )
        ; "_meta", prigh_meta
        ])
;;

let authenticate t params =
  let* method_id = string_param "methodId" params in
  let* () =
    if String.equal method_id login_method_id
    then Ok ()
    else
      Error
        (Error.invalid_params
           (sprintf
              "unknown auth method %S: prigh's is %S"
              method_id
              login_method_id))
  in
  let* link = or_internal (control t) in
  let* statuses = or_internal (Rpc_client.call link "auth_status" []) in
  if
    List.exists
      (Option.value (Json.list statuses) ~default:[])
      ~f:(fun s -> not (Json.exactly_equal (field "configured" s) `Null))
  then Ok (Reply.Result (`Object []))
  else
    Error
      (Error.auth_required
         ("prigh is not logged in to any provider: " ^ login_advice))
;;

(* ---------------------------------------------------------------------- *)
(* Dispatch                                                                *)

let methods =
  [ "initialize"
  ; "authenticate"
  ; "session/new"
  ; "session/load"
  ; "session/resume"
  ; "session/list"
  ; "session/close"
  ; "session/prompt"
  ; "session/set_mode"
  ; "session/set_config_option"
  ]
;;

let dispatch t ~method_ ~params =
  let reply = function
    | Ok reply -> reply
    | Error e -> Reply.Error e
  in
  match params with
  | `Object _ ->
    (match method_ with
     | "initialize" -> initialize t params
     | "authenticate" -> reply (authenticate t params)
     | "session/new" -> reply (new_session t params)
     | "session/load" -> reply (open_session t params ~replay:true)
     | "session/resume" -> reply (open_session t params ~replay:false)
     | "session/list" -> reply (list_sessions t params)
     | "session/close" -> reply (close_session_request t params)
     | "session/prompt" -> reply (prompt t params)
     | "session/set_mode" -> reply (set_mode t params)
     | "session/set_config_option" -> reply (set_config_option t params)
     | method_ ->
       Reply.Error
         (Error.method_not_found
            (sprintf
               "prigh does not implement %S; its methods are %s"
               method_
               (String.concat ~sep:", " methods))))
  | _ ->
    Reply.Error (Error.invalid_params (method_ ^ ": params must be an object"))
;;

let on_notification t ~method_ ~params =
  match method_ with
  | "session/cancel" -> cancel t params
  | "$/cancel_request" -> ()
  | method_ -> log t "ignoring the notification %s" method_
;;

let shutdown t =
  Hashtbl.iter t.sessions ~f:close_session;
  Hashtbl.clear t.sessions;
  Option.iter t.host ~f:(fun host ->
    Tool_host.Worker.stop host.worker;
    Rpc_client.close host.link);
  t.host <- None;
  Option.iter t.control ~f:Rpc_client.close;
  t.control <- None
;;

let serve
      ?(log = fun s -> eprintf "prigh acp: %s\n%!" s)
      ~env
      config
      ~read_line
      ~write_line
  =
  try
    Switch.run (fun sw ->
      let rpc = Json_rpc.create ~sw ~write_line in
      let t =
        { env
        ; sw
        ; rpc
        ; config
        ; caps = Caps.none
        ; client_name = "editor"
        ; sessions = String.Table.create ()
        ; host = None
        ; control = None
        ; models = None
        ; log
        }
      in
      Json_rpc.serve
        rpc
        ~read_line
        ~on_request:(dispatch t)
        ~on_notification:(on_notification t);
      shutdown t;
      (* The answers to what was still being handled, if they come soon. *)
      ignore
        (Eio.Time.with_timeout (Eio.Stdenv.clock env) 5. (fun () ->
           Json_rpc.flush rpc;
           Ok ())
         : (unit, [ `Timeout ]) Result.t);
      Switch.fail sw Exit)
  with
  | Exit -> ()
;;
