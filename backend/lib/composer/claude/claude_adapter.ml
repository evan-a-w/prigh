open! Core
open! Import

type t =
  { codec : Claude_codec.t
  ; driver : Claude_codec.Response.t Adapter.Driver.t
  ; sw : Switch.t
  ; injected : Injected_tools.t option
  ; mutable settings : Adapter.Settings.t (* what the process runs with *)
  ; mutable models : Model.t list
  }

let id = "claude"
let display_name = "Claude Code"

let capabilities : Capabilities.t =
  { fork = By_handoff
  ; rewind = Unsupported
  ; context_edit = false
  ; compact = Native
  ; steer = Between_turns
  ; plan_mode = true
  ; btw = false
  ; subagents = Observe
  ; model_fallback = false
  ; images = true
  ; ask_user = true
  }
;;

let current_capabilities _ = capabilities
let native_questions = true

let permission_mode ~approval ~plan : Claude_permission_mode.t =
  if plan
  then Plan
  else
    (* Smart mode has no equivalent: Claude Code asks, and prigh's approval
       model answers. *)
    Option.value
      (Claude_permission_mode.of_approval_mode approval)
      ~default:Default
;;

let effort : Thinking.t -> string option = function
  | Off | On None -> None
  | On (Some (Minimal | Low)) -> Some "low"
  | On (Some Medium) -> Some "medium"
  | On (Some High) -> Some "high"
  | On (Some Xhigh) -> Some "xhigh"
  | On (Some Max) -> Some "max"
;;

(* Claude Code's thinking budget per level (its [maxThinkingTokens]),
   which it takes mid-session; [None] leaves it to Claude Code. *)
let thinking_tokens : Thinking.t -> int option = function
  | Off -> Some 0
  | On None -> None
  | On (Some (Minimal | Low)) -> Some 4_000
  | On (Some Medium) -> Some 10_000
  | On (Some High) -> Some 32_000
  | On (Some (Xhigh | Max)) -> Some 63_999
;;

let thinking_levels = Thinking.Level.[ Low; Medium; High; Max ]

let model_of (m : Claude_codec.Model_info.t) =
  External_model.model
    ~adapter:id
    ~name:m.display_name
    ~thinking_levels
    ~can_disable_thinking:true
    m.value
;;

(* Claude Code's models, its default first (as [default]). *)
let models_of (infos : Claude_codec.Model_info.t list) =
  match infos with
  | [] -> []
  | _ ->
    let default, others =
      List.partition_tf infos ~f:(fun (m : Claude_codec.Model_info.t) ->
        String.equal m.value External_model.default_id)
    in
    let default =
      match default with
      | m :: _ -> { (model_of m) with name = m.display_name }
      | [] ->
        External_model.model
          ~adapter:id
          ~thinking_levels
          ~can_disable_thinking:true
          External_model.default_id
    in
    default :: List.map others ~f:model_of
;;

let mcp_config ~injected (start : Adapter.Start.t) =
  let sdk =
    if injected
    then (
      match Claude_launch.sdk_mcp_config [ Injected_tools.server_name ] with
      | `Object [ (_, `Object servers) ] -> servers
      | _ -> [])
    else []
  in
  let stdio =
    List.map start.mcp_servers ~f:(fun (name, config) ->
      ( name
      , match config with
        | `Object fields -> `Object (("type", `String "stdio") :: fields)
        | other -> other ))
  in
  match sdk @ stdio with
  | [] -> None
  | servers -> Some (`Object [ "mcpServers", `Object servers ])
;;

let launch_config ~injected (start : Adapter.Start.t) : Claude_launch.t =
  { Claude_launch.default with
    model = start.model
  ; permission_mode =
      Some (permission_mode ~approval:start.approval ~plan:start.plan)
  ; resume =
      (match start.resume with
       | None -> New { session_id = None }
       | Some session_id -> Resume { session_id; fork = start.fork; at = None })
  ; append_system_prompt = start.developer_instructions
  ; mcp_config = mcp_config ~injected start
  ; max_thinking_tokens = thinking_tokens start.thinking
  ; effort = effort start.thinking
  }
;;

let launch (start : Adapter.Start.t) : Adapter.Launch.t =
  { argv = Claude_launch.argv (launch_config ~injected:true start)
  ; env_set = Claude_launch.env_set
  ; env_unset = Claude_launch.env_unset
  ; mcp = false
  ; cwd = start.cwd
  }
;;

let dispatch t (output : Claude_codec.Output.t) =
  match output with
  | Event e -> Adapter.Driver.push t.driver e
  | Send line -> Adapter.Driver.write t.driver line
  | Response r -> Adapter.Driver.answer t.driver r
  | Mcp request ->
    (match t.injected with
     | Some injected ->
       (* A tool may wait for the user: the other lines go on meanwhile. *)
       Fiber.fork ~sw:t.sw (fun () ->
         Adapter.Driver.write
           t.driver
           (Injected_tools.claude_reply injected request))
     | None ->
       Adapter.Driver.write
         t.driver
         (Claude_codec.mcp_reply
            ~request_id:request.request_id
            (Some
               (`Object
                   [ "jsonrpc", `String "2.0"
                   ; ( "id"
                     , Option.value
                         (Json.member "id" request.message)
                         ~default:`Null )
                   ; ( "error"
                     , `Object
                         [ "code", `Number "-32601"
                         ; "message", `String "no in-band MCP server here"
                         ] )
                   ]))))
;;

(* Claude Code asks in a turn of its own (after a background task), which
   no prigh turn is there to answer. *)
let refuse_orphan t (event : Adapter_event.t) =
  match event with
  | Approval_request { request_id; _ } ->
    Result.iter
      (Claude_codec.respond_approval t.codec ~request_id Cancel)
      ~f:(Adapter.Driver.write t.driver)
  | User_question { request_id; _ } ->
    Result.iter
      (Claude_codec.answer t.codec ~request_id Cancelled)
      ~f:(Adapter.Driver.write t.driver)
  | _ -> ()
;;

let start
      ~sw
      ?interrupt_grace
      ?request_timeout
      ?injected
      transport
      (start : Adapter.Start.t)
  =
  let codec = Claude_codec.create () in
  let driver =
    Adapter.Driver.create
      ~sw
      ~name:display_name
      ?interrupt_grace
      ?request_timeout
      transport
  in
  let t =
    { codec
    ; driver
    ; sw
    ; injected
    ; settings = Adapter.Settings.of_start start
    ; models = []
    }
  in
  Adapter.Driver.on_orphan_request driver (refuse_orphan t);
  Adapter.Driver.run
    driver
    ~handle_line:(fun line ->
      List.iter (Claude_codec.handle_line codec line) ~f:(dispatch t))
    ~on_exit:(fun exit ->
      List.iter
        (Claude_codec.process_exited
           codec
           ~code:exit.code
           ~stderr_tail:exit.stderr_tail)
        ~f:(dispatch t));
  let sdk_mcp_servers =
    if Option.is_some injected then [ Injected_tools.server_name ] else []
  in
  let%bind.Or_error init =
    Adapter.Driver.request
      driver
      (Claude_codec.initialize codec ~sdk_mcp_servers)
      ~matches:(function
      | Initialized _ | Failed { command = Initialize; _ } -> true
      | Done _ | Failed _ -> false)
    |> Result.map_error ~f:(Error.tag ~tag:"Claude Code did not start")
  in
  match init with
  | Failed { message; _ } ->
    Or_error.errorf "Claude Code refused to start: %s" message
  | Initialized info ->
    t.models <- models_of info.models;
    Ok t
  | Done _ -> Ok t
;;

let next_event t = Adapter.Driver.next_event t.driver

(* Brings the process's settings up to [s]: model, permission mode and
   thinking change without a restart. *)
let apply t (s : Adapter.Settings.t) =
  let old = t.settings in
  t.settings <- s;
  let write = Adapter.Driver.write t.driver in
  if not (Option.equal String.equal old.model s.model)
  then write (Claude_codec.set_model t.codec s.model);
  let mode = permission_mode ~approval:s.approval ~plan:s.plan in
  if
    not
      (Claude_permission_mode.equal
         mode
         (permission_mode ~approval:old.approval ~plan:old.plan))
  then write (Claude_codec.set_permission_mode t.codec mode);
  if
    not
      (Option.equal
         Int.equal
         (thinking_tokens old.thinking)
         (thinking_tokens s.thinking))
  then
    write
      (Claude_codec.set_max_thinking_tokens
         t.codec
         (thinking_tokens s.thinking))
;;

let send t settings (input : Adapter.Input.t) =
  Adapter.Driver.await_idle t.driver;
  if not (Adapter.Driver.alive t.driver)
  then Or_error.error_string "Claude Code is not running"
  else if Adapter.Driver.in_turn t.driver
  then Or_error.error_string "a Claude Code turn is already running"
  else (
    apply t settings;
    Adapter.Driver.begin_turn t.driver;
    Adapter.Driver.write
      t.driver
      (Claude_codec.user_message
         t.codec
         ~text:input.text
         ~images:input.images
         ());
    Ok ())
;;

(* A message sent mid-turn is queued as a turn of its own, whose events
   would arrive after this one ended: steering waits for the turn's end. *)
let steer _ (_ : Adapter.Input.t) =
  Or_error.error_string "Claude Code takes new messages between turns"
;;

let interrupt t =
  if Adapter.Driver.in_turn t.driver
  then (
    Adapter.Driver.write t.driver (Claude_codec.interrupt t.codec);
    Adapter.Driver.watch_interrupt t.driver)
;;

let reply_approval t ~request_id reply =
  Result.iter
    (Claude_codec.respond_approval t.codec ~request_id reply)
    ~f:(Adapter.Driver.write t.driver)
;;

let answer t ~request_id reply =
  Result.iter
    (Claude_codec.answer t.codec ~request_id reply)
    ~f:(Adapter.Driver.write t.driver)
;;

let thread_id t = Claude_codec.session_id t.codec
let in_turn t = Adapter.Driver.in_turn t.driver
let alive t = Adapter.Driver.alive t.driver
let stop t = Adapter.Driver.stop t.driver
let models t = t.models

let compact t settings ~instructions =
  Adapter.Driver.await_idle t.driver;
  if not (Adapter.Driver.alive t.driver)
  then Or_error.error_string "Claude Code is not running"
  else if Adapter.Driver.in_turn t.driver
  then Or_error.error_string "a Claude Code turn is already running"
  else (
    apply t settings;
    Adapter.Driver.begin_turn t.driver;
    Adapter.Driver.write t.driver (Claude_codec.compact t.codec ~instructions);
    Ok ())
;;
