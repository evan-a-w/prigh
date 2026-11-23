open! Core
open! Import

type t =
  { codec : Codex_codec.t
  ; driver : Codex_codec.Response.t Adapter.Driver.t
  ; cwd : string
  ; mutable models : Model.t list
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

let launch (start : Adapter.Start.t) : Adapter.Launch.t =
  { argv = [ "codex"; "app-server" ]
  ; env_set = []
  ; env_unset = []
  ; cwd = start.cwd
  ; mcp = true
  }
;;

let settings ~cwd (s : Adapter.Settings.t) : Codex_codec.Settings.t =
  { cwd
  ; model = s.model
  ; thinking = s.thinking
  ; approval = s.approval
  ; plan = s.plan
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
    let default =
      match
        List.find infos ~f:(fun (m : Codex_codec.Model_info.t) -> m.is_default)
      with
      | Some m ->
        { (model_of m ~id) with
          id = External_model.default_id
        ; name = sprintf "default (%s)" m.display_name
        }
      | None -> External_model.default ~adapter:id
    in
    default :: List.map infos ~f:(model_of ~id)
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

let start
      ~sw
      ?interrupt_grace
      ?request_timeout
      ?injected:_
      transport
      (start : Adapter.Start.t)
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
  let t = { codec; driver; cwd = start.cwd; models = [] } in
  Adapter.Driver.on_orphan_request driver (refuse_orphan t);
  Adapter.Driver.run
    driver
    ~handle_line:(fun line ->
      List.iter (Codex_codec.handle_line codec line) ~f:(dispatch t))
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
  let fail_with_exit e = Error.tag e ~tag:"Codex did not start" in
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
  let%bind () = failed init ~what:"initialize" in
  let s = settings ~cwd:start.cwd (Adapter.Settings.of_start start) in
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
    t.models <- list_models driver codec;
    `Stop_daemon);
  t
;;

let next_event t = Adapter.Driver.next_event t.driver
let settings_of t s = settings ~cwd:t.cwd s

let send t settings input =
  Adapter.Driver.await_idle t.driver;
  if not (Adapter.Driver.alive t.driver)
  then Or_error.error_string "Codex is not running"
  else (
    let%map.Or_error line =
      Codex_codec.start_turn t.codec (settings_of t settings) (inputs input)
    in
    Adapter.Driver.begin_turn t.driver;
    Adapter.Driver.write t.driver line)
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
let alive t = Adapter.Driver.alive t.driver
let stop t = Adapter.Driver.stop t.driver
let models t = t.models

let compact t settings ~instructions:_ =
  Adapter.Driver.await_idle t.driver;
  if not (Adapter.Driver.alive t.driver)
  then Or_error.error_string "Codex is not running"
  else (
    ignore (settings : Adapter.Settings.t);
    let%map.Or_error line = Codex_codec.compact t.codec in
    Adapter.Driver.begin_turn t.driver;
    Adapter.Driver.write t.driver line)
;;
