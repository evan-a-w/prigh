open! Core
open! Import

type t =
  { codec : Codex_codec.t
  ; driver : Codex_codec.Response.t Adapter.Driver.t
  ; cwd : string
  }

let id = "codex"
let display_name = "Codex"

let capabilities : Capabilities.t =
  { fork = By_handoff
  ; rewind = Unsupported
  ; context_edit = false
  ; compact = Auto_only
  ; steer = In_run
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

let start ~sw ?interrupt_grace ?injected:_ transport (start : Adapter.Start.t) =
  let codec = Codex_codec.create () in
  let driver = Adapter.Driver.create ~sw ?interrupt_grace transport in
  let t = { codec; driver; cwd = start.cwd } in
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
      Codex_codec.fork_thread codec ~thread_id ~last_turn_id:None s options
    | Some thread_id -> Codex_codec.resume_thread codec ~thread_id s options
  in
  let%bind thread =
    Adapter.Driver.request driver line ~matches:(function
      | Thread _ -> true
      | Failed { kind = Thread_start | Thread_resume | Thread_fork; _ } -> true
      | _ -> false)
  in
  let%map () = failed thread ~what:"the thread" in
  t
;;

let next_event t = Adapter.Driver.next_event t.driver
let settings_of t s = settings ~cwd:t.cwd s

let send t settings input =
  if not (Adapter.Driver.alive t.driver)
  then Or_error.error_string "Codex is not running"
  else (
    let%map.Or_error line =
      Codex_codec.start_turn t.codec (settings_of t settings) (inputs input)
    in
    Adapter.Driver.begin_turn t.driver;
    Adapter.Driver.write t.driver line)
;;

let steer t input =
  let%map.Or_error line = Codex_codec.steer t.codec (inputs input) in
  Adapter.Driver.write t.driver line
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

let thread_id t = Codex_codec.thread_id t.codec
let in_turn t = Adapter.Driver.in_turn t.driver
let alive t = Adapter.Driver.alive t.driver
let stop t = Adapter.Driver.stop t.driver
