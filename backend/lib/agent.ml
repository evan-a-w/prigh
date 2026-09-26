open! Core
open! Import

module Queued = struct
  type t =
    { text : string
    ; attachments : string list
    }
  [@@deriving sexp_of]
end

module Host = struct
  type t =
    { id : string
    ; name : string
    ; cwd : string
    ; session_id : string option
    ; session_name : string option
    }
  [@@deriving sexp_of, equal]

  let backend_id = "backend"
end

module State = struct
  type t =
    { session_id : string
    ; session_path : string
    ; session_name : string option
    ; session_description : string option
    ; cwd : string
    ; git_branch : string option
    ; model : Model.t
    ; thinking : Thinking.t
    ; running : bool
    ; message_count : int
    ; usage : Usage.t
    ; cost_usd : float
    ; context_tokens : int
    ; active_host : string
    ; hosts : Host.t list
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
    | Tool_exec of
        { host : string
        ; exec_id : string
        ; call_id : string
        ; name : string
        ; arguments : Json.t
        ; cwd : string
        }
    | Tool_exec_cancel of
        { host : string
        ; exec_id : string
        }
  [@@deriving sexp_of]
end

module Pending_exec = struct
  type t =
    { host : string
    ; on_output : string -> unit
    ; resolver : Tool_result.t Promise.u
    }
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
  ; mutable describing : unit Promise.t option
  ; auto_describe : bool
  ; steer_queue : Queued.t Queue.t
  ; follow_up_queue : Queued.t Queue.t
  ; mutable subscribers : (Event.t -> unit) list
  ; mutable subagent_usage : Usage.t
  ; mutable subagent_cost_usd : float
  ; mutable config : Config.t
  ; pending_confirms : bool Promise.u String.Table.t
  ; mutable shell_seq : int
  ; mutable hosts : Host.t list
    (** every connected client able to run tools, set by the server *)
  ; host_cwds : string String.Table.t
    (** where this session last was on each host, overriding [Host.cwd] *)
  ; mutable backend_cwd : string (** the backend host's own cwd *)
  ; mutable active_host : string
  ; mutable host_pinned : bool (** chosen explicitly via [set_active_host] *)
  ; pending_execs : Pending_exec.t String.Table.t
  ; mutable exec_seq : int
  ; mutable environment_notes : string list
    (** cwd/host changes not yet told to the model, oldest first *)
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
      ?(auto_describe = false)
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
    ; describing = None
    ; auto_describe
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
    ; hosts = []
    ; host_cwds = String.Table.create ()
    ; backend_cwd = cwd
    ; active_host = Host.backend_id
    ; host_pinned = false
    ; pending_execs = String.Table.create ()
    ; exec_seq = 0
    ; environment_notes = []
    }
  in
  restore_settings t;
  t
;;

let backend_host t =
  { Host.id = Host.backend_id
  ; name = Core_unix.gethostname ()
  ; cwd = t.backend_cwd
  ; session_id = None
  ; session_name = None
  }
;;

let hosts t =
  backend_host t
  :: List.map t.hosts ~f:(fun h ->
    match Hashtbl.find t.host_cwds h.id with
    | Some cwd -> { h with cwd }
    | None -> h)
;;

let active_host t = t.active_host
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
  ; session_description = Session.description t.session
  ; cwd = t.cwd
  ; git_branch = t.git_branch
  ; model = t.model
  ; thinking = t.thinking
  ; running = is_running t
  ; message_count = List.length messages
  ; usage
  ; cost_usd = Model.cost_usd t.model assistant_usage +. t.subagent_cost_usd
  ; context_tokens
  ; active_host = t.active_host
  ; hosts = hosts t
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

(* ---- tool hosts ------------------------------------------------------- *)

let find_host t id = List.find (hosts t) ~f:(fun h -> String.equal h.id id)
let active_host_connected t = Option.is_some (find_host t t.active_host)

let fail_execs t ~host ~text =
  let failed =
    Hashtbl.filter t.pending_execs ~f:(fun (e : Pending_exec.t) ->
      String.equal e.host host)
  in
  Hashtbl.iteri failed ~f:(fun ~key ~data:(e : Pending_exec.t) ->
    Hashtbl.remove t.pending_execs key;
    Promise.resolve e.resolver (Tool.Result.error text))
;;

(* Before the first run the system prompt is still to be built from the
   current environment, so there is nothing to report. *)
let add_environment_note t note =
  if Option.is_some (Session.system_prompt t.session)
  then t.environment_notes <- t.environment_notes @ [ note ]
;;

let set_host_cwd t (host : Host.t) ~cwd =
  if not (String.equal t.cwd cwd)
  then (
    t.cwd <- cwd;
    t.git_branch <- Git_branch.find ~cwd;
    ignore (Session.set_cwd t.session ~cwd : Session.Entry.t);
    add_environment_note t (System_prompt.cwd_changed_note ~cwd));
  if String.equal host.id Host.backend_id
  then t.backend_cwd <- cwd
  else Hashtbl.set t.host_cwds ~key:host.id ~data:cwd
;;

let activate_host t (host : Host.t) ~cwd =
  if not (String.equal t.active_host host.id)
  then add_environment_note t (System_prompt.host_changed_note ~host:host.name);
  t.active_host <- host.id;
  set_host_cwd t host ~cwd;
  broadcast t (Notice (sprintf "tools now run on %s in %s" host.name cwd));
  state_changed t
;;

let set_hosts t hosts =
  if not (List.equal Host.equal t.hosts hosts)
  then (
    let gone =
      List.filter t.hosts ~f:(fun h ->
        not (List.exists hosts ~f:(fun h' -> String.equal h.id h'.id)))
    in
    t.hosts <- hosts;
    List.iter gone ~f:(fun h ->
      Hashtbl.remove t.host_cwds h.id;
      fail_execs t ~host:h.id ~text:"[tool host disconnected]");
    state_changed t)
;;

(* Tools default to the frontend: a host attaching takes over unless the user
   pinned a host that is still connected. *)
let prefer_host t id =
  match find_host t id with
  | None -> ()
  | Some host ->
    let take_over =
      (not (active_host_connected t))
      || ((not t.host_pinned) && String.equal t.active_host Host.backend_id)
    in
    if take_over then activate_host t host ~cwd:host.cwd
;;

let tool_exec_output t ~exec_id ~chunk =
  match Hashtbl.find t.pending_execs exec_id with
  | None -> Or_error.errorf "no tool execution %S" exec_id
  | Some e ->
    e.on_output chunk;
    Ok ()
;;

let tool_exec_result t ~exec_id ~text ~is_error =
  match Hashtbl.find t.pending_execs exec_id with
  | None -> Or_error.errorf "no tool execution %S" exec_id
  | Some e ->
    Hashtbl.remove t.pending_execs exec_id;
    Promise.resolve e.resolver { Tool_result.text; is_error };
    Ok ()
;;

(* Runs [name] on [host]: in-process when that is the backend, otherwise
   through a [Tool_exec] round trip with the client. *)
let host_exec_on
      t
      (host : Host.t)
      ~cancel
      ~on_output
      ~call_id
      ~cwd
      ~name
      ~arguments
  =
  if String.equal host.id Host.backend_id
  then Host_ops.execute ~env:t.env ~cancel ~on_output ~cwd ~name ~arguments
  else (
    let exec_id =
      sprintf "%s/%s-%d" (Session.id t.session) call_id t.exec_seq
    in
    t.exec_seq <- t.exec_seq + 1;
    let promise, resolver = Promise.create () in
    Hashtbl.set
      t.pending_execs
      ~key:exec_id
      ~data:{ Pending_exec.host = host.id; on_output; resolver };
    broadcast
      t
      (Tool_exec { host = host.id; exec_id; call_id; name; arguments; cwd });
    match Cancellation.protect cancel ~f:(fun () -> Promise.await promise) with
    | Some result -> result
    | None ->
      Hashtbl.remove t.pending_execs exec_id;
      broadcast t (Tool_exec_cancel { host = host.id; exec_id });
      Tool.Result.error "[cancelled]")
;;

let host_exec t ~cancel ~on_output ~call_id ~cwd ~name ~arguments =
  match find_host t t.active_host with
  | None ->
    Tool.Result.error
      (sprintf
         "tool host %S is not connected; use set_active_host to pick another"
         t.active_host)
  | Some host ->
    host_exec_on t host ~cancel ~on_output ~call_id ~cwd ~name ~arguments
;;

let resolve_dir_on t (host : Host.t) path =
  match
    host_exec_on
      t
      host
      ~cancel:Cancellation.never
      ~on_output:ignore
      ~call_id:"cd"
      ~cwd:host.cwd
      ~name:Host_ops.resolve_dir_op
      ~arguments:(`Object [ "path", `String path ])
  with
  | { is_error = true; text } -> Or_error.errorf "%s: %s" host.name text
  | { is_error = false; text } -> Ok text
;;

(* The session cwd is a property of the host, so switching hosts means
   choosing a directory there; without [cwd] the host's own is used. *)
let set_active_host t id ~cwd =
  match find_host t id with
  | None -> Or_error.errorf "unknown tool host %S" id
  | Some host ->
    let cwd =
      match cwd with
      | None -> Ok host.cwd
      | Some path -> resolve_dir_on t host (Tool.expand_home path)
    in
    Or_error.map cwd ~f:(fun cwd ->
      activate_host t host ~cwd;
      t.host_pinned <- true)
;;

let executor t : Tool.executor =
  fun context tool arguments ->
  if (not tool.spec.on_host) || String.equal t.active_host Host.backend_id
  then Tool.execute tool context arguments
  else
    host_exec
      t
      ~cancel:context.cancel
      ~on_output:context.on_output
      ~call_id:context.call_id
      ~cwd:context.cwd
      ~name:(Tool.name tool)
      ~arguments
;;

(* AGENTS.md/CLAUDE.md come from the active host's filesystem. *)
let instructions t ~cwd =
  Host_ops.instructions_of_result
    (host_exec
       t
       ~cancel:Cancellation.never
       ~on_output:ignore
       ~call_id:"instructions"
       ~cwd
       ~name:Host_ops.instructions_op
       ~arguments:(`Object [ "home", `String t.home ]))
;;

(* Built once per conversation and recorded in the session, so the prompt
   prefix stays cacheable; later cwd/host changes reach the model as notes on
   the next user message instead. *)
let system_prompt t =
  match Session.system_prompt t.session with
  | Some text -> text
  | None ->
    let text =
      System_prompt.build
        ~instructions:(instructions t ~cwd:t.cwd)
        ~cwd:t.cwd
        ~home:t.home
        ~tools:(Tools.specs t.tools)
        ()
    in
    ignore (Session.set_system_prompt t.session ~text : Session.Entry.t);
    text
;;

let loop_config t =
  { Agent_loop.Config.model = t.model
  ; thinking = t.thinking
  ; system = Some (system_prompt t)
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
      let result =
        host_exec
          t
          ~cancel:Cancellation.never
          ~on_output:ignore
          ~call_id:"attachment"
          ~cwd:t.cwd
          ~name:Host_ops.read_file_op
          ~arguments:(`Object [ "path", `String path ])
      in
      match result with
      | { is_error = true; text } ->
        sprintf "<file path=%S error=%S/>" path text
      | { is_error = false; text = content } ->
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
  let text = with_attachments t q.text q.attachments in
  let text =
    match t.environment_notes with
    | [] -> text
    | notes ->
      t.environment_notes <- [];
      String.concat ~sep:"\n" notes ^ "\n\n" ^ text
  in
  Message.user text
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
         ~execute:(executor t)
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
    auto_describe t;
    match Queue.dequeue t.follow_up_queue with
    | Some queued ->
      queue_update t;
      start_run t [ user_message t queued ]
    | None -> ())

(* Runs after the turn is over so the user is not kept waiting; [wait_idle]
   still covers it. *)
and auto_describe t =
  let session = t.session in
  if
    t.auto_describe
    && Option.is_none t.describing
    && Session_description.wanted session
  then (
    let finished, resolve = Promise.create () in
    t.describing <- Some finished;
    Fiber.fork ~sw:t.sw (fun () ->
      (match
         Session_description.describe
           ~provider:t.provider
           ~model:t.model
           session
       with
       | Ok _ -> if phys_equal session t.session then state_changed t
       | Error e ->
         broadcast
           t
           (Notice ("session description failed: " ^ Error.to_string_hum e)));
      t.describing <- None;
      Promise.resolve resolve ()))

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
  match t.run, t.describing with
  | Some run, _ ->
    Promise.await run.finished;
    wait_idle t
  | None, Some describing ->
    Promise.await describing;
    wait_idle t
  | None, None -> ()
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
    broadcast t (Loop (Tool_start call));
    let result =
      host_exec
        t
        ~cancel:Cancellation.never
        ~on_output:(fun chunk ->
          Buffer.add_string output chunk;
          broadcast t (Loop (Tool_output { call_id; chunk })))
        ~call_id
        ~cwd:t.cwd
        ~name:"bash"
        ~arguments:
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
  t.environment_notes <- [];
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
    match find_host t t.active_host with
    | None ->
      Or_error.errorf
        "tool host %S is not connected; use set_active_host to pick another"
        t.active_host
    | Some host ->
      Or_error.map
        (resolve_dir_on t host (resolve_path t path))
        ~f:(fun cwd ->
          set_host_cwd t host ~cwd;
          state_changed t))
;;

let list_paths t ~prefix =
  match
    host_exec
      t
      ~cancel:Cancellation.never
      ~on_output:ignore
      ~call_id:"list_paths"
      ~cwd:t.cwd
      ~name:Host_ops.list_paths_op
      ~arguments:(`Object [ "prefix", `String prefix ])
  with
  | { is_error = true; text } -> Or_error.error_string text
  | { is_error = false; text } -> Json.parse text
;;

let list_dirs ?host t ~prefix =
  let host =
    match host with
    | None -> Ok (find_host t t.active_host)
    | Some id ->
      (match find_host t id with
       | Some host -> Ok (Some host)
       | None -> Or_error.errorf "unknown tool host %S" id)
  in
  Or_error.bind host ~f:(function
    | None -> Or_error.error_string "tool host is not connected"
    | Some host ->
      let cwd =
        if String.equal host.id t.active_host then t.cwd else host.cwd
      in
      (match
         host_exec_on
           t
           host
           ~cancel:Cancellation.never
           ~on_output:ignore
           ~call_id:"list_dirs"
           ~cwd
           ~name:Host_ops.list_dirs_op
           ~arguments:(`Object [ "prefix", `String prefix ])
       with
       | { is_error = true; text } -> Or_error.error_string text
       | { is_error = false; text } -> Json.parse text))
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
