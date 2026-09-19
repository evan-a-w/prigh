open! Core
open! Import

module Queued = struct
  type t =
    { text : string
    ; attachments : string list
    }
  [@@deriving sexp_of]
end

module State = struct
  type t =
    { session_id : string
    ; session_path : string
    ; session_name : string option
    ; cwd : string
    ; git_branch : string option
    ; model : Model.t
    ; thinking : Thinking.t
    ; running : bool
    ; message_count : int
    ; usage : Usage.t
    ; cost_usd : float
    ; context_tokens : int
    }
  [@@deriving sexp_of]
end

module Session_stats = struct
  type t =
    { message_count : int
    ; turns : int
    ; tool_calls : (string * int) list
    ; usage : Usage.t
    ; cost_usd : float
    ; context_percent : float
    ; model_changes : int
    ; compactions : int
    ; duration_seconds : float
    }
  [@@deriving sexp_of]
end

module Event = struct
  type t =
    | Loop of Agent_event.t
    | State_changed of State.t
    | Compacted of { summary : string }
    | Notice of string
    | Config_changed of Config.t
    | Queue_update of
        { steer : int
        ; follow_up : int
        }
  [@@deriving sexp_of]
end

module Run = struct
  type t =
    { cancel : Cancellation.t
    ; finished : unit Promise.t
    }
end

type t =
  { env : Env.t
  ; sw : Switch.t
  ; provider : Provider.t
  ; tools : Tool.t list
  ; sessions_dir : string
  ; home : string
  ; mutable session : Session.t
  ; mutable cwd : string
  ; mutable git_branch : string option
  ; mutable model : Model.t
  ; mutable thinking : Thinking.t
  ; mutable run : Run.t option
  ; steer_queue : Queued.t Queue.t
  ; follow_up_queue : Queued.t Queue.t
  ; mutable subscribers : (Event.t -> unit) list
  ; mutable subagent_usage : Usage.t
  ; mutable subagent_cost_usd : float
  ; mutable config : Config.t
  ; pending_confirms : bool Promise.u String.Table.t
  ; mutable shell_seq : int
  }

let restore_settings t =
  match Session.model t.session with
  | Some (model_id, thinking) ->
    Option.iter (Model.find model_id) ~f:(fun m -> t.model <- m);
    t.thinking <- thinking
  | None -> ()
;;

let create
      ~env
      ~sw
      ~provider
      ~tools
      ~sessions_dir
      ~home
      ?session
      ?(model = Model.default)
      ?(thinking = Thinking.Off)
      ~cwd
      ()
  =
  let session =
    match session with
    | Some s -> s
    | None -> Session.create ~dir:sessions_dir ~cwd ()
  in
  let cwd = Session.cwd session in
  let t =
    { env
    ; sw
    ; provider
    ; tools
    ; sessions_dir
    ; home
    ; session
    ; cwd
    ; git_branch = Git_branch.find ~cwd
    ; model
    ; thinking
    ; run = None
    ; steer_queue = Queue.create ()
    ; follow_up_queue = Queue.create ()
    ; subscribers = []
    ; subagent_usage = Usage.zero
    ; subagent_cost_usd = 0.
    ; config =
        (match Config.load ~home with
         | Ok config -> config
         | Error _ -> Config.default)
    ; pending_confirms = String.Table.create ()
    ; shell_seq = 0
    }
  in
  restore_settings t;
  t
;;

let subscribe t ~f = t.subscribers <- f :: t.subscribers
let broadcast t event = List.iter (List.rev t.subscribers) ~f:(fun f -> f event)
let session t = t.session
let messages t = Session.messages t.session
let is_running t = Option.is_some t.run

let state t =
  let messages = messages t in
  let assistant_messages =
    List.filter_map messages ~f:(function
      | Message.Assistant a -> Some a
      | User _ | Tool_result _ -> None)
  in
  let assistant_usage =
    List.fold assistant_messages ~init:Usage.zero ~f:(fun acc a ->
      Usage.add acc a.usage)
  in
  let usage = Usage.add assistant_usage t.subagent_usage in
  let context_tokens =
    Option.value_map (List.last assistant_messages) ~default:0 ~f:(fun a ->
      a.usage.input)
  in
  { State.session_id = Session.id t.session
  ; session_path = Session.path t.session
  ; session_name = Session.name t.session
  ; cwd = t.cwd
  ; git_branch = t.git_branch
  ; model = t.model
  ; thinking = t.thinking
  ; running = is_running t
  ; message_count = List.length messages
  ; usage
  ; cost_usd = Model.cost_usd t.model assistant_usage +. t.subagent_cost_usd
  ; context_tokens
  }
;;

let state_changed t = broadcast t (State_changed (state t))

let queue_update t =
  broadcast
    t
    (Queue_update
       { steer = Queue.length t.steer_queue
       ; follow_up = Queue.length t.follow_up_queue
       })
;;

let config t = t.config

let set_config t config =
  Or_error.map (Config.save ~home:t.home config) ~f:(fun () ->
    t.config <- config;
    broadcast t (Config_changed config))
;;

let respond_confirm t ~call_id ~allow =
  match Hashtbl.find t.pending_confirms call_id with
  | None -> Or_error.errorf "no pending confirmation for tool call %S" call_id
  | Some resolver ->
    Hashtbl.remove t.pending_confirms call_id;
    Promise.resolve resolver allow;
    Ok ()
;;

let confirm_hook t cancel call ~summary =
  if not t.config.confirm_tools
  then true
  else (
    let promise, resolver = Promise.create () in
    Hashtbl.set t.pending_confirms ~key:call.Content.Tool_call.id ~data:resolver;
    broadcast
      t
      (Loop (Tool_confirm { call_id = call.id; name = call.name; summary }));
    let allow =
      match
        Cancellation.protect cancel ~f:(fun () -> Promise.await promise)
      with
      | None -> false
      | Some allow -> allow
    in
    Hashtbl.remove t.pending_confirms call.id;
    allow)
;;

let loop_config t =
  { Agent_loop.Config.model = t.model
  ; thinking = t.thinking
  ; system =
      Some
        (System_prompt.build
           ~cwd:t.cwd
           ~home:t.home
           ~tools:(Tools.specs t.tools)
           ())
  ; tools = t.tools
  ; max_turns = None
  ; max_tokens = None
  ; retries = Agent_loop.Config.default_retries
  }
;;

let with_attachments t text attachments =
  match attachments with
  | [] -> text
  | _ ->
    let block path =
      match Tool_read.read_for_context ~cwd:t.cwd path with
      | Error e ->
        sprintf "<file path=%S error=%S/>" path (Error.to_string_hum e)
      | Ok content ->
        let content =
          if String.is_suffix content ~suffix:"\n"
          then content
          else content ^ "\n"
        in
        sprintf "<file path=%S>\n%s</file>" path content
    in
    text ^ "\n\n" ^ String.concat (List.map attachments ~f:block) ~sep:"\n\n"
;;

let user_message t (q : Queued.t) =
  Message.user (with_attachments t q.text q.attachments)
;;

let rec start_run t prompts =
  t.git_branch <- Git_branch.find ~cwd:t.cwd;
  let cancel = Cancellation.create () in
  let finished, resolve = Promise.create () in
  t.run <- Some { cancel; finished };
  state_changed t;
  Fiber.fork ~sw:t.sw (fun () ->
    let session = t.session in
    let finish () =
      t.run <- None;
      Promise.resolve resolve ();
      state_changed t
    in
    (match
       Agent_loop.run
         ~env:t.env
         ~provider:t.provider
         ~config:(loop_config t)
         ~cwd:t.cwd
         ~cancel
         ~confirm:(confirm_hook t cancel)
         ~steer:(fun () ->
           let l = Queue.to_list t.steer_queue in
           if not (List.is_empty l)
           then (
             Queue.clear t.steer_queue;
             queue_update t);
           List.map l ~f:(user_message t))
         ~emit:(fun event ->
           (match event with
            | Subagent_end { usage; cost_usd; _ } ->
              t.subagent_usage <- Usage.add t.subagent_usage usage;
              t.subagent_cost_usd <- t.subagent_cost_usd +. cost_usd
            | _ -> ());
           (match event with
            | Message_end m ->
              ignore (Session.append_message session m : Session.Entry.t)
            | _ -> ());
           broadcast t (Loop event))
         ~context:(Session.messages session)
         ~prompts
         ()
     with
     | (_ : Message.t list) -> ()
     | exception exn ->
       broadcast t (Notice ("run failed: " ^ Exn.to_string exn)));
    (* Steering messages that arrived after the last turn boundary. *)
    if not (Queue.is_empty t.steer_queue)
    then (
      Queue.blit_transfer ~src:t.steer_queue ~dst:t.follow_up_queue ();
      queue_update t);
    auto_compact t;
    finish ();
    match Queue.dequeue t.follow_up_queue with
    | Some queued ->
      queue_update t;
      start_run t [ user_message t queued ]
    | None -> ())

and auto_compact t =
  let state = state t in
  if Compaction.should_compact t.model ~input_tokens:state.context_tokens
  then (
    match
      Compaction.compact
        ~env:t.env
        ~provider:t.provider
        ~model:t.model
        t.session
    with
    | Ok summary -> broadcast t (Compacted { summary })
    | Error e ->
      broadcast t (Notice ("auto-compaction failed: " ^ Error.to_string_hum e)))
;;

let prompt ?(attachments = []) t text =
  if is_running t
  then
    Or_error.error_string "a run is already in progress; use steer or follow_up"
  else (
    start_run t [ user_message t { text; attachments } ];
    Ok ())
;;

let enqueue t queue ?(attachments = []) text =
  let queued = { Queued.text; attachments } in
  if is_running t
  then (
    Queue.enqueue queue queued;
    queue_update t)
  else start_run t [ user_message t queued ]
;;

let steer ?attachments t text = enqueue t t.steer_queue ?attachments text

let follow_up ?attachments t text =
  enqueue t t.follow_up_queue ?attachments text
;;

(* Queued text is restored verbatim: attachments are only inlined when the
   message is actually sent. *)
let abort t =
  let texts q = List.map (Queue.to_list q) ~f:(fun (q : Queued.t) -> q.text) in
  let restored = texts t.steer_queue in
  let follow_ups = texts t.follow_up_queue in
  Queue.clear t.steer_queue;
  Queue.clear t.follow_up_queue;
  queue_update t;
  Option.iter t.run ~f:(fun run -> Cancellation.cancel run.cancel);
  restored @ follow_ups
;;

let rec wait_idle t =
  match t.run with
  | Some run ->
    Promise.await run.finished;
    wait_idle t
  | None -> ()
;;

(* Pops the most recently queued message: follow-ups take priority over steer
   messages, and within each queue the back (last enqueued) is removed. *)
let dequeue t =
  let pop_last queue =
    match List.rev (Queue.to_list queue) with
    | [] -> None
    | last :: rest ->
      Queue.clear queue;
      List.iter (List.rev rest) ~f:(fun item -> Queue.enqueue queue item);
      Some last
  in
  let popped =
    match pop_last t.follow_up_queue with
    | Some _ as queued -> queued
    | None -> pop_last t.steer_queue
  in
  Option.iter popped ~f:(fun _ -> queue_update t);
  popped
;;

let shell t ~command ~add_to_context =
  if is_running t
  then
    Or_error.error_string
      "cannot run a shell command while a run is in progress"
  else (
    let call_id = sprintf "shell-%d" t.shell_seq in
    t.shell_seq <- t.shell_seq + 1;
    let call : Content.Tool_call.t =
      { id = call_id
      ; name = "shell"
      ; arguments = Json.to_string (`Object [ "command", `String command ])
      }
    in
    let output = Buffer.create 1024 in
    let context =
      Tool.Context.create
        ~on_output:(fun chunk ->
          Buffer.add_string output chunk;
          broadcast t (Loop (Tool_output { call_id; chunk })))
        ~call_id
        ~tools:t.tools
        ~env:t.env
        ~cwd:t.cwd
        ()
    in
    broadcast t (Loop (Tool_start call));
    let result =
      Tool_bash.run
        context
        (`Object [ "command", `String command; "timeout", `Number "120" ])
    in
    broadcast
      t
      (Loop
         (Tool_end
            { call
            ; result =
                { Message.Tool_result.tool_call_id = call_id
                ; tool_name = "shell"
                ; text = result.text
                ; is_error = result.is_error
                }
            }));
    if add_to_context
    then (
      let truncated = Truncate.head (Buffer.contents output) in
      let output =
        if truncated.truncated
        then
          sprintf
            "[output truncated: showing the first part of %d lines / %d bytes]\n\
             %s"
            truncated.total_lines
            truncated.total_bytes
            truncated.text
        else truncated.text
      in
      let message = Message.user (sprintf "$ %s\n%s" command output) in
      broadcast t (Loop (Message_start message));
      ignore (Session.append_message t.session message : Session.Entry.t);
      broadcast t (Loop (Message_end message));
      state_changed t);
    Ok result)
;;

let set_model t model =
  t.model <- model;
  ignore
    (Session.set_model t.session ~model:(Model.key model) ~thinking:t.thinking
     : Session.Entry.t);
  state_changed t
;;

let set_thinking t thinking =
  t.thinking <- thinking;
  ignore
    (Session.set_model t.session ~model:(Model.key t.model) ~thinking
     : Session.Entry.t);
  state_changed t
;;

let compact t =
  if is_running t
  then Or_error.error_string "cannot compact while a run is in progress"
  else (
    let result =
      Compaction.compact
        ~env:t.env
        ~provider:t.provider
        ~model:t.model
        t.session
    in
    Result.iter result ~f:(fun summary ->
      broadcast t (Compacted { summary });
      state_changed t);
    result)
;;

let replace_session t session =
  ignore (abort t);
  wait_idle t;
  t.session <- session;
  t.cwd <- Session.cwd session;
  t.git_branch <- Git_branch.find ~cwd:t.cwd;
  t.subagent_usage <- Usage.zero;
  t.subagent_cost_usd <- 0.;
  restore_settings t;
  state_changed t
;;

let new_session t =
  replace_session t (Session.create ~dir:t.sessions_dir ~cwd:t.cwd ())
;;

let switch_session t ~path =
  Or_error.map (Session.load path) ~f:(fun session -> replace_session t session)
;;

let fork t ?at () =
  Or_error.map
    (Session.fork ?at t.session ~dir:t.sessions_dir)
    ~f:(fun session -> replace_session t session)
;;

let rewind t ~to_ =
  if is_running t
  then Or_error.error_string "cannot rewind while a run is in progress"
  else
    Or_error.map (Session.rewind t.session ~to_) ~f:(fun () -> state_changed t)
;;

let set_session_name t name =
  ignore (Session.set_name t.session ~name : Session.Entry.t);
  state_changed t
;;

let resolve_path t path =
  let path = Tool.expand_home path in
  if Filename.is_absolute path then path else Filename.concat t.cwd path
;;

let set_cwd t ~path =
  if is_running t
  then
    Or_error.error_string "cannot change directory while a run is in progress"
  else (
    let path = resolve_path t path in
    match Sys_unix.is_directory path with
    | `Yes ->
      let path = Filename_unix.realpath path in
      t.cwd <- path;
      t.git_branch <- Git_branch.find ~cwd:path;
      ignore (Session.set_cwd t.session ~cwd:path : Session.Entry.t);
      state_changed t;
      Ok ()
    | `No | `Unknown -> Or_error.errorf "not a directory: %s" path)
;;

let delete_session t ~path =
  if String.equal path (Session.path t.session)
  then Or_error.error_string "cannot delete the active session"
  else Or_error.try_with (fun () -> Core_unix.unlink path)
;;

let export t ~format ?path () =
  let path =
    match path with
    | Some p -> resolve_path t p
    | None ->
      let base = Filename.basename (Session.path t.session) in
      let stamp = List.hd_exn (String.split base ~on:'_') in
      Filename.concat
        (Filename.concat t.sessions_dir "exports")
        (sprintf
           "%s_%s.%s"
           stamp
           (Session.id t.session)
           (Session.Export_format.extension format))
  in
  Or_error.try_with (fun () ->
    Core_unix.mkdir_p (Filename.dirname path);
    (match format with
     | Session.Export_format.Markdown ->
       Out_channel.write_all path ~data:(Session.to_markdown t.session)
     | Jsonl ->
       Out_channel.write_all
         path
         ~data:(In_channel.read_all (Session.path t.session)));
    path)
;;

let import_session t ~path =
  Or_error.map (Session.import ~dir:t.sessions_dir path) ~f:(fun session ->
    replace_session t session;
    Session.path session)
;;

let session_stats t =
  let path = Session.active_path t.session in
  let messages = Session.messages t.session in
  let assistants =
    List.filter_map messages ~f:(function
      | Message.Assistant a -> Some a
      | User _ | Tool_result _ -> None)
  in
  let tool_calls = String.Table.create () in
  List.iter assistants ~f:(fun a ->
    List.iter (Message.Assistant.tool_calls a) ~f:(fun call ->
      Hashtbl.update tool_calls call.name ~f:(function
        | None -> 1
        | Some n -> n + 1)));
  let count is_model =
    List.count path ~f:(fun (e : Session.Entry.t) ->
      match is_model, e.payload with
      | true, Model _ -> true
      | false, Compaction _ -> true
      | _ -> false)
  in
  let state = state t in
  let context_percent =
    match state.context_tokens with
    | 0 -> 0.
    | tokens ->
      Float.of_int tokens /. Float.of_int t.model.context_window *. 100.
  in
  { Session_stats.message_count = List.length messages
  ; turns = List.length assistants
  ; tool_calls =
      Hashtbl.to_alist tool_calls
      |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  ; usage = state.usage
  ; cost_usd = state.cost_usd
  ; context_percent
  ; model_changes = count true
  ; compactions = count false
  ; duration_seconds = Session.duration_seconds t.session
  }
;;
