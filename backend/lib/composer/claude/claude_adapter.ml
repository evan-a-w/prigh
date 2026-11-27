open! Core
open! Import

type t =
  { codec : Claude_codec.t
  ; driver : Claude_codec.Response.t Adapter.Driver.t
  ; sw : Switch.t
  ; injected : Injected_tools.t option
  ; mutable settings : Adapter.Settings.t (* what the process runs with *)
  ; mutable models : Model.t list
  ; mcp_calls : unit Promise.u String.Table.t (* by request id: abandons it *)
  ; mutable on_change : unit -> unit
  ; mutable login : Claude_login.t option
    (** prigh's login, when the process runs on it *)
  ; mutable refused : bool (** a turn was refused with its token *)
  }

let id = "claude"
let display_name = "Claude Code"

let capabilities : Capabilities.t =
  { fork = Native
  ; rewind = Native
  ; context_edit = false
  ; compact = Native
  ; steer = Between_turns
  ; plan_mode = true
  ; btw = false
  ; subagents = Observe
  ; model_fallback = false
  ; images = true
  ; ask_user = true
  ; rewind_note = None
  }
;;

let current_capabilities _ = capabilities
let native_questions = true
let starts_early = false

(* Claude Code asks ([can_use_tool]) before every tool that is not
   read-only, in every prigh mode: prigh's approval (the session's mode, its
   deny and ask rules, Smart's judge) decides, as for the harness's own
   tools. Its own modes that decide by themselves (acceptEdits,
   bypassPermissions) would skip those rules. *)
let permission_mode ~plan : Claude_permission_mode.t =
  if plan then Plan else Default
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

(* What Claude Code calls a model: the head of its description ("Sonnet 5 ·
   Efficient for…" is "Sonnet 5") when that names it, else its name. *)
let name_of (m : Claude_codec.Model_info.t) =
  let head =
    Option.map m.description ~f:(fun d ->
      match String.substr_index d ~pattern:" · " with
      | Some i -> String.prefix d i
      | None -> d)
  in
  let first_word s = List.hd (String.split s ~on:' ') in
  match head with
  | Some head
    when Option.equal String.equal (first_word head) (first_word m.display_name)
         && String.exists head ~f:Char.is_digit -> head
  | _ -> m.display_name
;;

let model_of (m : Claude_codec.Model_info.t) =
  let thinking_levels =
    match m.efforts with
    | Some (_ :: _ as efforts) ->
      List.filter_map efforts ~f:Thinking.Level.of_string
    | Some [] | None -> thinking_levels
  in
  External_model.model
    ~adapter:id
    ~name:(name_of m)
    ~thinking_levels
    ~can_disable_thinking:true
    m.value
;;

(* "Use the default model (currently Opus 5 (1M context)) · …": the model
   in the brackets after "currently". *)
let currently description =
  let pattern = "currently " in
  let%bind.Option i = String.substr_index description ~pattern in
  let rest = String.drop_prefix description (i + String.length pattern) in
  let rec close depth i =
    if i >= String.length rest
    then None
    else (
      match rest.[i] with
      | '(' -> close (depth + 1) (i + 1)
      | ')' when depth = 0 -> Some (String.prefix rest i)
      | ')' -> close (depth - 1) (i + 1)
      | _ -> close depth (i + 1))
  in
  close 0 0 |> Option.filter ~f:(fun s -> not (String.is_empty s))
;;

(* Claude Code's models, its default first, listed once: its [default] row
   stands for a model, listed under that model's own row (else its id). *)
let models_of (infos : Claude_codec.Model_info.t list) =
  let default, others =
    List.partition_tf infos ~f:(fun (m : Claude_codec.Model_info.t) ->
      String.equal m.value External_model.default_id)
  in
  match default with
  | [] -> List.map others ~f:model_of
  | d :: _ ->
    let same, rest =
      List.partition_tf others ~f:(fun (m : Claude_codec.Model_info.t) ->
        Option.is_some d.resolved
        && Option.equal String.equal m.resolved d.resolved)
    in
    let name =
      match same, Option.bind d.description ~f:currently with
      | _, Some current -> current
      | m :: _, None -> name_of m
      | [], None -> d.display_name
    in
    let first =
      match same with
      | m :: _ -> model_of m
      | [] ->
        { (model_of d) with
          id = Option.value d.resolved ~default:External_model.default_id
        }
    in
    { first with name } :: List.map rest ~f:model_of
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
  ; permission_mode = Some (permission_mode ~plan:start.plan)
  ; resume =
      (match start.resume with
       | None -> New { session_id = None }
       | Some session_id ->
         Resume
           { session_id
           ; fork = start.fork
           ; at = Option.bind start.at ~f:(fun at -> at.mark)
           })
  ; mcp_config = mcp_config ~injected start
  ; max_thinking_tokens = thinking_tokens start.thinking
  ; effort = effort start.thinking
  }
;;

let launch (start : Adapter.Start.t) : Adapter.Launch.t =
  let login : Claude_login.Env.t =
    match start.login with
    | None -> { set = []; unset = []; secret = None }
    | Some login -> Claude_login.env login ~refused:start.login_refused
  in
  { argv = Claude_launch.argv (launch_config ~injected:true start)
  ; env_set = Claude_launch.env_set @ login.set
  ; env_unset = Claude_launch.env_unset @ login.unset
  ; mcp = false
  ; cwd = start.cwd
  ; secret = login.secret
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
       let abandoned, abandon = Promise.create () in
       Hashtbl.set t.mcp_calls ~key:request.request_id ~data:abandon;
       Fiber.fork ~sw:t.sw (fun () ->
         Fiber.first
           (fun () ->
              let reply = Injected_tools.claude_reply injected request in
              Hashtbl.remove t.mcp_calls request.request_id;
              Adapter.Driver.write t.driver reply)
           (fun () -> Promise.await abandoned))
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
  | Mcp_cancelled request_id ->
    Option.iter
      (Hashtbl.find_and_remove t.mcp_calls request_id)
      ~f:(fun abandon -> Promise.resolve abandon ())
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

(* Whether the process runs on the login of prigh's it was launched with
   ([launch]), saying so when it does not. *)
let take_login t (start : Adapter.Start.t) (info : Claude_codec.Init_info.t) =
  let notice message = Adapter.Driver.push t.driver (Notice message) in
  Option.iter start.login ~f:(fun credential ->
    match Claude_login.of_launch credential ~refused:start.login_refused with
    | Error why -> notice why
    | Ok login ->
      (match
         Claude_login.overridden
           login
           ~api_key_source:
             (Option.bind info.account ~f:(fun a -> a.api_key_source))
       with
       | Some why -> notice why
       | None -> t.login <- Some login))
;;

(* The process's driver, reading its lines, and the handshake. *)
let open_process
      ~sw
      ?interrupt_grace
      ?request_timeout
      ?injected
      transport
      (start : Adapter.Start.t)
  =
  let codec = Claude_codec.create ?cost_total:start.cost_total () in
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
    ; mcp_calls = String.Table.create ()
    ; on_change = Fn.id
    ; login = None
    ; refused = false
    }
  in
  Adapter.Driver.on_orphan_request driver (refuse_orphan t);
  Adapter.Driver.run
    driver
    ~handle_line:(fun line ->
      let limits = Claude_codec.limits codec in
      List.iter (Claude_codec.handle_line codec line) ~f:(dispatch t);
      if
        not
          (Option.equal
             Subscription_usage.Status.equal
             limits
             (Claude_codec.limits codec))
      then t.on_change ())
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
      (Claude_codec.initialize
         codec
         ?append_system_prompt:start.developer_instructions
         ~sdk_mcp_servers
         ())
      ~matches:(function
      | Initialized _ | Failed { command = Initialize; _ } -> true
      | _ -> false)
    |> Result.map_error ~f:(Adapter.Driver.start_failed driver)
  in
  match init with
  | Failed { message; _ } ->
    Or_error.errorf "Claude Code refused to start: %s" message
  | Initialized info ->
    t.models <- models_of info.models;
    take_login t start info;
    Ok t
  | _ -> Ok t
;;

(* The login's plan limits, asked: none for an API key's login. *)
let read_limits t =
  match
    Adapter.Driver.request
      t.driver
      (Claude_codec.get_usage t.codec)
      ~matches:(function
      | Usage _ | Failed { command = Get_usage; _ } -> true
      | _ -> false)
  with
  | Ok (Usage status) -> Some status
  | Ok (Failed { message; _ }) ->
    Some
      (Failed
         (sprintf
            "Claude Code: %s (claude /login, on the session's tool host)"
            message))
  | Ok _ | Error _ -> None
;;

let catalog ~sw ?request_timeout transport start =
  let%map.Or_error t = open_process ~sw ?request_timeout transport start in
  { Adapter.Catalog.models = t.models; limits = read_limits t }
;;

let start ~sw ?interrupt_grace ?request_timeout ?injected transport start =
  open_process ~sw ?interrupt_grace ?request_timeout ?injected transport start
;;

(* A turn refused with prigh's token: the next one gets a newer one. *)
let next_event t : Adapter_event.t =
  match Adapter.Driver.next_event t.driver, t.login with
  | ( Turn_end { turn_id; stop = Failed { kind = Auth_required _; message } }
    , Some login ) ->
    t.refused <- true;
    Turn_end
      { turn_id
      ; stop =
          Failed
            { kind = Auth_required { hint = Claude_login.refused_hint login }
            ; message
            }
      }
  | event, _ -> event
;;

(* Brings the process's settings up to [s]: model, permission mode and
   thinking change without a restart. *)
let apply t (s : Adapter.Settings.t) =
  let old = t.settings in
  t.settings <- s;
  let write = Adapter.Driver.write t.driver in
  if not (Option.equal String.equal old.model s.model)
  then write (Claude_codec.set_model t.codec s.model);
  let mode = permission_mode ~plan:s.plan in
  if not (Claude_permission_mode.equal mode (permission_mode ~plan:old.plan))
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

let rollback = Adapter.Rollback.Resume_at
let fork_native = true

let revert _ ~before_turn_id:_ =
  Or_error.error_string
    "Claude Code rolls back when it next starts, not while it runs"
;;

let mark t = Claude_codec.last_assistant t.codec
let thread_id t = Claude_codec.session_id t.codec
let cost_total t = Claude_codec.cost_total t.codec
let in_turn t = Adapter.Driver.in_turn t.driver
let has_open_work _ = false
let alive t = Adapter.Driver.alive t.driver
let await_exit t = Adapter.Driver.await_exit t.driver
let login t = Option.map t.login ~f:Claude_login.credential

let renewal t : Adapter.Login_renewal.t option =
  match t.login with
  | None -> None
  | Some login when t.refused -> Some (Refused (Claude_login.token login))
  | Some login ->
    Option.some_if
      (Claude_login.expiring login ~now_ms:(Credential.now_ms ()))
      Adapter.Login_renewal.Expiring
;;

let stop t = Adapter.Driver.stop t.driver
let models t = t.models
let limits t = Claude_codec.limits t.codec
let on_change t f = t.on_change <- f
let on_autonomous t f = Adapter.Driver.on_autonomous t.driver f

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
