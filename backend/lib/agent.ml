open! Core
open! Import

module State = struct
  type t =
    { session_id : string
    ; session_path : string
    ; cwd : string
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

module Event = struct
  type t =
    | Loop of Agent_event.t
    | State_changed of State.t
    | Compacted of { summary : string }
    | Notice of string
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
  ; cwd : string
  ; mutable session : Session.t
  ; mutable model : Model.t
  ; mutable thinking : Thinking.t
  ; mutable run : Run.t option
  ; steer_queue : Message.t Queue.t
  ; follow_up_queue : string Queue.t
  ; mutable subscribers : (Event.t -> unit) list
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
    | None -> Session.create ~dir:sessions_dir ~cwd
  in
  let t =
    { env
    ; sw
    ; provider
    ; tools
    ; sessions_dir
    ; home
    ; cwd
    ; session
    ; model
    ; thinking
    ; run = None
    ; steer_queue = Queue.create ()
    ; follow_up_queue = Queue.create ()
    ; subscribers = []
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
  let usage =
    List.fold assistant_messages ~init:Usage.zero ~f:(fun acc a ->
      Usage.add acc a.usage)
  in
  let context_tokens =
    Option.value_map (List.last assistant_messages) ~default:0 ~f:(fun a ->
      a.usage.input)
  in
  { State.session_id = Session.id t.session
  ; session_path = Session.path t.session
  ; cwd = t.cwd
  ; model = t.model
  ; thinking = t.thinking
  ; running = is_running t
  ; message_count = List.length messages
  ; usage
  ; cost_usd = Model.cost_usd t.model usage
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

let config t =
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

let rec start_run t prompts =
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
         ~config:(config t)
         ~cwd:t.cwd
         ~cancel
         ~steer:(fun () ->
           let l = Queue.to_list t.steer_queue in
           if not (List.is_empty l)
           then (
             Queue.clear t.steer_queue;
             queue_update t);
           l)
         ~emit:(fun event ->
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
      Queue.iter t.steer_queue ~f:(fun m ->
        Queue.enqueue
          t.follow_up_queue
          (match m with
           | User u -> u.text
           | _ -> ""));
      Queue.clear t.steer_queue;
      queue_update t);
    auto_compact t;
    finish ();
    match Queue.dequeue t.follow_up_queue with
    | Some text ->
      queue_update t;
      start_run t [ Message.user text ]
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

let prompt t text =
  if is_running t
  then
    Or_error.error_string "a run is already in progress; use steer or follow_up"
  else (
    start_run t [ Message.user text ];
    Ok ())
;;

let steer t text =
  if is_running t
  then (
    Queue.enqueue t.steer_queue (Message.user text);
    queue_update t)
  else start_run t [ Message.user text ]
;;

let follow_up t text =
  if is_running t
  then (
    Queue.enqueue t.follow_up_queue text;
    queue_update t)
  else start_run t [ Message.user text ]
;;

let abort t =
  let restored =
    List.filter_map (Queue.to_list t.steer_queue) ~f:(function
      | User u -> Some u.text
      | Assistant _ | Tool_result _ -> None)
  in
  let follow_ups = Queue.to_list t.follow_up_queue in
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
  restore_settings t;
  state_changed t
;;

let new_session t =
  replace_session t (Session.create ~dir:t.sessions_dir ~cwd:t.cwd)
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
