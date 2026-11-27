open! Core
open! Import

type t =
  { codec : Codex_codec.t
  ; driver : Codex_codec.Response.t Adapter.Driver.t
  ; sw : Switch.t
  ; cwd : string
  ; sandbox : bool
    (** [false]: it cannot run on the host ([Start.no_sandbox]) *)
  ; mutable models : Model.t list
  ; mutable on_change : unit -> unit
  ; mutable version : string option
  ; mutable login : Codex_login.t option
    (** prigh's login, once Codex took it *)
  }

let id = "codex"
let display_name = "Codex"

let capabilities : Capabilities.t =
  { fork = Native
  ; rewind = Native
  ; context_edit = false
  ; compact = Native
  ; steer = In_run
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

let launch (start : Adapter.Start.t) : Adapter.Launch.t =
  { argv = [ "codex"; "app-server" ]
  ; env_set = []
  ; env_unset = []
  ; cwd = start.cwd
  ; mcp = true
  ; secret = None
  }
;;

let launch_complete (start : Adapter.Start.t) : Adapter.Launch.t =
  { (launch start) with mcp = false }
;;

(* The model's efforts in Codex's names, once Codex has listed its models. *)
let efforts models (s : Adapter.Settings.t) =
  (match s.model with
   | Some id -> List.find models ~f:(fun (m : Model.t) -> String.equal m.id id)
   | None -> List.hd models)
  |> Option.map ~f:(fun (m : Model.t) ->
    (if m.can_disable_thinking then [ "none" ] else [])
    @ List.map m.thinking_levels ~f:Thinking.Level.to_string)
;;

let settings ~cwd ~sandbox ~models (s : Adapter.Settings.t)
  : Codex_codec.Settings.t
  =
  { cwd
  ; model = s.model
  ; thinking = s.thinking
  ; approval = s.approval
  ; plan = s.plan
  ; efforts = efforts models s
  ; sandbox
  ; writable_roots = s.extra_dirs
  }
;;

let inputs (input : Adapter.Input.t) : Codex_codec.Input.t list =
  (if String.is_empty input.text
   then []
   else [ Codex_codec.Input.Text input.text ])
  @ List.map input.images ~f:(fun i -> Codex_codec.Input.Image i)
;;

let dispatch t (output : Codex_codec.Output.t) =
  match output with
  | Event e -> Adapter.Driver.push t.driver e
  | Send line -> Adapter.Driver.write t.driver line
  | Response r -> Adapter.Driver.answer t.driver r
  | Token_refresh { request_id; previous_account_id = _ } ->
    (* Fetching may refresh over the network: not on the reader's fiber. *)
    Guarded.fork ~sw:t.sw ~what:"Codex token refresh" (fun () ->
      let tokens =
        match t.login with
        | Some login -> Codex_login.tokens login ~refused:true
        | None ->
          Or_error.error_string
            "prigh handed this Codex no login: it runs on its own"
      in
      Option.iter
        (Codex_codec.reply_token_refresh t.codec ~request_id tokens)
        ~f:(Adapter.Driver.write t.driver))
  | Tool_call { request_id; tool; _ } ->
    (* No dynamic tools are offered: prigh's come over MCP. *)
    Option.iter
      (Codex_codec.reply_tool_call
         t.codec
         ~request_id
         ~output:(sprintf "unknown tool %s" tool)
         ~success:false)
      ~f:(Adapter.Driver.write t.driver)
;;

let failed (r : Codex_codec.Response.t) ~what =
  match r with
  | Failed { message; code; _ } ->
    Or_error.errorf "Codex refused %s (%d): %s" what code message
  | _ -> Ok ()
;;

let level_of_effort = function
  | "xhigh" -> Some Thinking.Level.Xhigh
  | e -> Thinking.Level.of_string e
;;

let model_of (m : Codex_codec.Model_info.t) ~id =
  External_model.model
    ~adapter:id
    ~name:m.display_name
    ~thinking_levels:
      (List.filter_map m.efforts ~f:level_of_effort
       |> List.dedup_and_sort ~compare:Thinking.Level.compare)
    ~can_disable_thinking:(List.mem m.efforts "none" ~equal:String.equal)
    ~images:m.images
    m.id
;;

(* The models Codex offers (its default first, as [default]): asked once
   the thread is open; none when it cannot say. *)
let list_models driver codec =
  let rec pages cursor acc n =
    match
      Adapter.Driver.request
        driver
        (Codex_codec.list_models codec ~cursor)
        ~matches:(function
        | Codex_codec.Response.Models _ | Failed { kind = Model_list; _ } ->
          true
        | _ -> false)
    with
    | Ok (Codex_codec.Response.Models { models; next_cursor = Some cursor })
      when n < 10 -> pages (Some cursor) (acc @ models) (n + 1)
    | Ok (Codex_codec.Response.Models { models; next_cursor = _ }) ->
      acc @ models
    | Ok _ | Error _ -> acc
  in
  let infos =
    List.filter (pages None [] 0) ~f:(fun (m : Codex_codec.Model_info.t) ->
      not m.hidden)
  in
  match infos with
  | [] -> []
  | _ ->
    (* Its default first. *)
    let default, others =
      List.partition_tf infos ~f:(fun (m : Codex_codec.Model_info.t) ->
        m.is_default)
    in
    List.map (default @ others) ~f:(model_of ~id)
;;

(* Codex asks in a turn of its own, which no prigh turn is there to answer. *)
let refuse_orphan t (event : Adapter_event.t) =
  match event with
  | Approval_request { request_id; _ } ->
    Option.iter
      (Codex_codec.reply_approval t.codec ~request_id Cancel)
      ~f:(Adapter.Driver.write t.driver)
  | User_question { request_id; _ } ->
    Option.iter
      (Codex_codec.answer_question t.codec ~request_id Cancelled)
      ~f:(Adapter.Driver.write t.driver)
  | _ -> ()
;;

(* The process's driver, reading its lines, and the handshake. *)
let open_process
      ~sw
      ?interrupt_grace
      ?request_timeout
      ?(sandbox = true)
      transport
      ~cwd
  =
  let codec = Codex_codec.create () in
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
    ; cwd
    ; sandbox
    ; models = []
    ; on_change = Fn.id
    ; version = None
    ; login = None
    }
  in
  Adapter.Driver.on_orphan_request driver (refuse_orphan t);
  Adapter.Driver.run
    driver
    ~handle_line:(fun line ->
      let limits = Codex_codec.limits codec in
      List.iter (Codex_codec.handle_line codec line) ~f:(dispatch t);
      if
        not
          (Option.equal
             Subscription_usage.Status.equal
             limits
             (Codex_codec.limits codec))
      then t.on_change ())
    ~on_exit:(fun exit ->
      if Adapter.Driver.in_turn driver
      then
        Adapter.Driver.push
          driver
          (Turn_end
             { turn_id = Codex_codec.turn_id codec
             ; stop =
                 Failed
                   { kind = Crashed { stderr_tail = exit.stderr_tail }
                   ; message =
                       Adapter.Driver.exit_message
                         ~tail:false
                         ~display_name
                         exit
                   }
             }));
  let fail_with_exit e = Adapter.Driver.start_failed driver e in
  let open Or_error.Let_syntax in
  let%bind init =
    Adapter.Driver.request
      driver
      (Codex_codec.initialize
         codec
         ~client_name:"prigh"
         ~client_version:Version.to_string)
      ~matches:(function
      | Initialized _ | Failed { kind = Initialize; _ } -> true
      | _ -> false)
    |> Result.map_error ~f:fail_with_exit
  in
  let%map () = failed init ~what:"initialize" in
  (match init with
   | Initialized { version; _ } -> t.version <- version
   | _ -> ());
  t
;;

let notice t message = Adapter.Driver.push t.driver (Notice message)

let login_request t tokens =
  Adapter.Driver.request
    t.driver
    (Codex_codec.login_tokens t.codec tokens)
    ~matches:(function
    | Logged_in | Failed { kind = Login; _ } -> true
    | _ -> false)
;;

(* Hands Codex prigh's login; when it cannot take it, it runs on its own,
   saying so. *)
let hand_login t login =
  let fallback why =
    notice t (Codex_login.fallback_notice login ~version:t.version why)
  in
  match Codex_login.tokens login ~refused:false with
  | Error e -> fallback (`No_token e)
  | Ok tokens ->
    (match login_request t tokens with
     | Ok Logged_in -> t.login <- Some login
     | Ok (Failed { message; _ }) -> fallback (`Refused message)
     | Ok _ -> ()
     | Error e -> fallback (`Refused (Error.to_string_hum e)))
;;

(* Codex does not refresh a token it was handed before it expires, only
   once refused: a turn starts with one that lasts. *)
let renew_login t =
  match t.login with
  | Some login when Codex_login.expiring login ~now_ms:(Credential.now_ms ()) ->
    let label = Cli_credential.label (Codex_login.credential login) in
    (match Codex_login.tokens login ~refused:false with
     | Error e ->
       notice
         t
         (sprintf
            "prigh could not refresh ChatGPT login %S for Codex: %s"
            label
            (Error.to_string_hum e))
     | Ok tokens ->
       (match login_request t tokens with
        | Ok Logged_in | Ok _ -> ()
        | Error e ->
          notice
            t
            (sprintf
               "Codex did not take a fresh token of ChatGPT login %S: %s"
               label
               (Error.to_string_hum e))))
  | Some _ | None -> ()
;;

(* The login's limits, asked: none for a login without them (an API key's
   answer is an error). *)
let read_limits t =
  match
    Adapter.Driver.request
      t.driver
      (Codex_codec.read_rate_limits t.codec)
      ~matches:(function
      | Rate_limits _ | Failed { kind = Rate_limits_read; _ } -> true
      | _ -> false)
  with
  | Ok (Rate_limits status) -> Some status
  | Ok (Failed { message; _ }) ->
    Some
      (Failed
         (sprintf "Codex: %s (codex login, on the session's tool host)" message))
  | Ok _ | Error _ -> None
;;

let catalog ~sw ?request_timeout transport (start : Adapter.Start.t) =
  let%map.Or_error t =
    open_process ~sw ?request_timeout transport ~cwd:start.cwd
  in
  Option.iter start.login ~f:(fun login ->
    hand_login t (Codex_login.create login));
  let models = list_models t.driver t.codec in
  let limits = read_limits t in
  { Adapter.Catalog.models; limits }
;;

let complete
      ~sw:_
      ?(timeout = External_complete.default_timeout)
      ?request_timeout
      ?on_delta
      ~cancel
      transport
      (start : Adapter.Start.t)
      (input : Adapter.Complete_input.t)
  =
  Switch.run (fun sw ->
    Exn.protect
      ~finally:(fun () -> Line_transport.stop transport)
      ~f:(fun () ->
        let open Or_error.Let_syntax in
        let%bind t, s, reported_model =
          External_complete.with_cancel_timeout ~cancel ~timeout (fun () ->
            let%bind t =
              open_process
                ~sw
                ?request_timeout
                ~sandbox:(Option.is_none start.no_sandbox)
                transport
                ~cwd:start.cwd
            in
            Option.iter start.login ~f:(fun login ->
              hand_login t (Codex_login.create login));
            let models =
              if not (List.is_empty input.catalog_models)
              then input.catalog_models
              else if Option.is_some input.model
              then []
              else list_models t.driver t.codec
            in
            t.models <- models;
            let chosen =
              match input.model with
              | Some req ->
                let resolved =
                  External_model.resolve ~adapter:id ~models:(Some models) req
                  |> Result.ok
                  |> Option.value ~default:req
                in
                Some resolved
              | None -> External_complete.pick_fast_model models
            in
            let s =
              settings
                ~cwd:start.cwd
                ~sandbox:t.sandbox
                ~models
                { model = chosen
                ; thinking = Off
                ; approval = Ask
                ; plan = false
                ; extra_dirs = []
                }
            in
            let options : Codex_codec.Thread_options.t =
              { Codex_codec.Thread_options.empty with
                developer_instructions =
                  Some (External_complete.system_with_no_tools input.system)
              ; mcp_servers = []
              ; ephemeral = true
              }
            in
            let%bind thread =
              Adapter.Driver.request
                t.driver
                (Codex_codec.start_thread t.codec s options)
                ~matches:(function
                | Thread _ | Failed { kind = Thread_start; _ } -> true
                | _ -> false)
            in
            let%map () = failed thread ~what:"the thread" in
            let thread_model =
              match thread with
              | Thread { model; _ } -> model
              | _ -> None
            in
            let reported_model =
              Option.first_some chosen thread_model
              |> Option.map ~f:(fun m ->
                External_model.name ~adapter:id (Some m))
            in
            t, s, reported_model)
        in
        Exn.protect
          ~finally:(fun () -> Adapter.Driver.stop t.driver)
          ~f:(fun () ->
            let%bind line =
              Codex_codec.start_turn
                t.codec
                s
                [ Codex_codec.Input.Text input.prompt ]
            in
            Adapter.Driver.begin_turn t.driver;
            Adapter.Driver.write t.driver line;
            External_complete.drive
              ~timeout
              ?on_delta
              ~cancel
              ~next_event:(fun () -> Adapter.Driver.next_event t.driver)
              ~interrupt:(fun () ->
                List.iter (Codex_codec.cancel_pending t.codec) ~f:(dispatch t);
                match Codex_codec.interrupt t.codec with
                | `Send line -> Adapter.Driver.write t.driver line
                | `Deferred | `Idle -> ())
              ~reply_approval:(fun ~request_id reply ->
                Option.iter
                  (Codex_codec.reply_approval t.codec ~request_id reply)
                  ~f:(Adapter.Driver.write t.driver))
              ~answer:(fun ~request_id reply ->
                Option.iter
                  (Codex_codec.answer_question t.codec ~request_id reply)
                  ~f:(Adapter.Driver.write t.driver))
              ~model:reported_model
              ())))
;;

let start
      ~sw
      ?interrupt_grace
      ?request_timeout
      ?injected:_
      transport
      (start : Adapter.Start.t)
  =
  let open Or_error.Let_syntax in
  let%bind t =
    open_process
      ~sw
      ?interrupt_grace
      ?request_timeout
      ~sandbox:(Option.is_none start.no_sandbox)
      transport
      ~cwd:start.cwd
  in
  Option.iter start.login ~f:(fun login ->
    hand_login t (Codex_login.create login));
  let codec = t.codec
  and driver = t.driver in
  let s =
    settings
      ~cwd:start.cwd
      ~sandbox:t.sandbox
      ~models:[]
      (Adapter.Settings.of_start start)
  in
  let options : Codex_codec.Thread_options.t =
    { Codex_codec.Thread_options.empty with
      developer_instructions = start.developer_instructions
    ; mcp_servers = start.mcp_servers
    }
  in
  let line =
    match start.resume with
    | None -> Codex_codec.start_thread codec s options
    | Some thread_id when start.fork ->
      Codex_codec.fork_thread
        codec
        ~thread_id
        ~last_turn_id:(Option.bind start.at ~f:(fun at -> at.turn_id))
        s
        options
    | Some thread_id -> Codex_codec.resume_thread codec ~thread_id s options
  in
  let%bind thread =
    Adapter.Driver.request driver line ~matches:(function
      | Thread _ -> true
      | Failed { kind = Thread_start | Thread_resume | Thread_fork; _ } -> true
      | _ -> false)
  in
  let%map () = failed thread ~what:"the thread" in
  (* Not waited for: a turn need not wait for the list. *)
  Fiber.fork_daemon ~sw (fun () ->
    Guarded.run ~what:"Codex model list" (fun () ->
      t.models <- list_models driver codec;
      t.on_change ());
    `Stop_daemon);
  t
;;

let next_event t = Adapter.Driver.next_event t.driver
let settings_of t s = settings ~cwd:t.cwd ~sandbox:t.sandbox ~models:t.models s

let send t settings input =
  Adapter.Driver.await_idle t.driver;
  if not (Adapter.Driver.alive t.driver)
  then Or_error.error_string "Codex is not running"
  else if Adapter.Driver.in_turn t.driver
  then Or_error.error_string "a Codex turn is already running"
  else (
    renew_login t;
    if not (Adapter.Driver.alive t.driver)
    then Or_error.error_string "Codex is not running"
    else if Adapter.Driver.in_turn t.driver
    then Or_error.error_string "a Codex turn is already running"
    else (
      let%map.Or_error line =
        Codex_codec.start_turn t.codec (settings_of t settings) (inputs input)
      in
      Adapter.Driver.begin_turn t.driver;
      Adapter.Driver.write t.driver line))
;;

(* Waits for Codex's answer: a refused steer (the turn ended meanwhile,
   or cannot be steered) fails, so the runner keeps the message for the next
   turn rather than losing it. *)
let steer t input =
  let%bind.Or_error line = Codex_codec.steer t.codec (inputs input) in
  match%bind.Or_error
    Adapter.Driver.request t.driver line ~matches:(function
      | Steered _ | Failed { kind = Turn_steer; _ } -> true
      | _ -> false)
  with
  | Failed { message; _ } ->
    Or_error.error_string ("Codex did not take it: " ^ message)
  | _ -> Ok ()
;;

let interrupt t =
  List.iter (Codex_codec.cancel_pending t.codec) ~f:(dispatch t);
  (match Codex_codec.interrupt t.codec with
   | `Send line -> Adapter.Driver.write t.driver line
   | `Deferred | `Idle -> ());
  if Adapter.Driver.in_turn t.driver
  then Adapter.Driver.watch_interrupt t.driver
;;

let reply_approval t ~request_id reply =
  Option.iter
    (Codex_codec.reply_approval t.codec ~request_id reply)
    ~f:(Adapter.Driver.write t.driver)
;;

let answer t ~request_id reply =
  Option.iter
    (Codex_codec.answer_question t.codec ~request_id reply)
    ~f:(Adapter.Driver.write t.driver)
;;

let rollback = Adapter.Rollback.Revert
let fork_native = true

let revert t ~before_turn_id =
  let open Or_error.Let_syntax in
  let%bind line = Codex_codec.revert t.codec ~before_turn_id in
  let%bind reply =
    Adapter.Driver.request t.driver line ~matches:(function
      | Done Revert | Failed { kind = Revert; _ } -> true
      | _ -> false)
  in
  failed reply ~what:"to roll its thread back"
;;

let mark _ = None
let thread_id t = Codex_codec.thread_id t.codec
let cost_total _ = None
let in_turn t = Adapter.Driver.in_turn t.driver
let has_open_work _ = false
let alive t = Adapter.Driver.alive t.driver
let await_exit t = Adapter.Driver.await_exit t.driver
let login t = Option.map t.login ~f:Codex_login.credential
let renewal _ = None
let stop t = Adapter.Driver.stop t.driver
let models t = t.models
let limits t = Codex_codec.limits t.codec
let on_change t f = t.on_change <- f
let on_autonomous t f = Adapter.Driver.on_autonomous t.driver f
let on_title t f = Adapter.Driver.on_title t.driver f

let compact t settings ~instructions:_ =
  Adapter.Driver.await_idle t.driver;
  if not (Adapter.Driver.alive t.driver)
  then Or_error.error_string "Codex is not running"
  else if Adapter.Driver.in_turn t.driver
  then Or_error.error_string "a Codex turn is already running"
  else (
    renew_login t;
    if not (Adapter.Driver.alive t.driver)
    then Or_error.error_string "Codex is not running"
    else if Adapter.Driver.in_turn t.driver
    then Or_error.error_string "a Codex turn is already running"
    else (
      ignore (settings : Adapter.Settings.t);
      let%map.Or_error line = Codex_codec.compact t.codec in
      Adapter.Driver.begin_turn t.driver;
      Adapter.Driver.write t.driver line))
;;
