open! Core
open! Import

type live = Live : (module Adapter.S with type t = 'a) * 'a -> live

type t =
  { sw : Switch.t
  ; adapter : Adapter.t
  ; spawn : Adapter.Launch.t -> Line_transport.t Or_error.t
  ; start : cwd:string -> Adapter.Start.t
  ; interrupt_grace : Time_float.Span.t option
  ; request_timeout : Time_float.Span.t option
  ; steer_poll : Time_float.Span.t
  ; host : unit -> string
  ; mutable live : live option
  ; mutable live_host : string option
  ; mutable thread_id : string option
  ; mutable resumed : string option
  ; mutable fresh : bool
  ; mutable handoff_from : Engine_choice.t option
  ; mutable last_place : (string * string) option
  ; mutable turns : int
  ; mutable thread_model : string option (* the model the CLI said it runs *)
  ; on_thread : string -> unit
  ; mutable services : Engine_intf.Services.t option (* the running turn's *)
  ; mutable cancel : Cancellation.t
  ; mutable models : Model.t list (* the harness's, as it last said *)
  ; mutable context : int option * int option
  }

let create
      ~sw
      ~adapter
      ~spawn
      ?(start = fun ~cwd -> Adapter.Start.default ~cwd)
      ?interrupt_grace
      ?request_timeout
      ?(steer_poll = Time_float.Span.of_sec 0.2)
      ?(on_thread = ignore)
      ?(host = fun () -> "local")
      ?handoff_from
      ()
  =
  { sw
  ; adapter
  ; spawn
  ; start
  ; interrupt_grace
  ; request_timeout
  ; steer_poll
  ; host
  ; live = None
  ; live_host = None
  ; thread_id = None
  ; resumed = None
  ; fresh = false
  ; handoff_from
  ; last_place = None
  ; turns = 0
  ; thread_model = None
  ; on_thread
  ; services = None
  ; cancel = Cancellation.never
  ; models = []
  ; context = None, None
  }
;;

let thread_id t = t.thread_id

let close t =
  Option.iter t.live ~f:(fun (Live ((module A), a)) -> A.stop a);
  t.live <- None
;;

let context (s : Engine_intf.Services.t) ~cancel ~call_id ?agent_id () =
  Tool.Context.create
    ~cancel
    ~execute:s.execute
    ~call_id
    ?agent_id
    ~tools:s.tools
    ~emit:s.emit
    ~background:s.background
    ~halt:s.halt
    ~approve:s.approve
    ~ask_user:s.ask_user
    ~ui:s.ui
    ~env:s.env
    ~cwd:s.cwd
    ()
;;

let mcp_calls = ref 0

(* prigh's tools called over MCP: approved and run as the harness engine's
   calls are. *)
let injected t (s : Engine_intf.Services.t) =
  let (module A : Adapter.S) = t.adapter in
  let native_questions = A.native_questions in
  Injected_tools.create
    ~tools:(Injected_tools.select ~native_questions s.tools)
    ~execute:(fun tool args ->
      match t.services with
      | None -> Tool_result.error "no turn is running"
      | Some s ->
        incr mcp_calls;
        let ctx =
          context s ~cancel:t.cancel ~call_id:(sprintf "mcp-%d" !mcp_calls) ()
        in
        (match s.approve ctx tool args with
         | Some refused -> refused
         | None when Cancellation.is_cancelled ctx.cancel ->
           (* Approved after its turn ended: nobody waits for it now. *)
           Tool_result.error "[cancelled]: its turn ended"
         | None -> s.execute ctx tool args))
;;

let settings (input : Engine_intf.Turn.Input.t) : Adapter.Settings.t =
  { model = input.model
  ; thinking = input.thinking
  ; approval = input.approval
  ; plan = input.plan
  }
;;

(* The thread [a] is on, once known: recorded when it is not the one it was
   asked to continue (that one is recorded already). A thread other than
   that one is new, so it is handed the whole session. *)
let note_thread t (type a) (module A : Adapter.S with type t = a) (a : a) =
  match A.thread_id a with
  | Some id when not (Option.equal String.equal (Some id) t.thread_id) ->
    t.thread_id <- Some id;
    if not (Option.equal String.equal (Some id) t.resumed)
    then (
      if Option.is_some t.resumed then t.fresh <- true;
      t.on_thread id)
  | Some _ | None -> ()
;;

let start_process t (s : Engine_intf.Services.t) (start : Adapter.Start.t) =
  let (module A) = t.adapter in
  match t.spawn (A.launch start) with
  | Error e -> Error (`Spawn e)
  | Ok transport ->
    let injected = injected t s in
    (* A harness with no MCP in its protocol reaches prigh's tools through
       the bridge on its host. *)
    let start =
      match Line_transport.mcp transport with
      | None -> start
      | Some mcp ->
        mcp.serve injected;
        { start with
          mcp_servers =
            start.mcp_servers
            @ [ ( Injected_tools.server_name
                , Injected_tools.Bridge.server_json mcp.bridge )
              ]
        }
    in
    (match
       A.start
         ~sw:t.sw
         ?interrupt_grace:t.interrupt_grace
         ?request_timeout:t.request_timeout
         ~injected
         transport
         start
     with
     | exception exn ->
       (* Cancelled while it started (Esc): it would run for nobody. *)
       Line_transport.stop transport;
       raise exn
     | Ok a ->
       t.resumed <- start.resume;
       (* A new thread has seen nothing: whatever an older one saw. *)
       if Option.is_none start.resume then t.fresh <- true;
       note_thread t (module A) a;
       Ok (Live ((module A), a))
     | Error e ->
       Line_transport.stop transport;
       Error (`Start e))
;;

(* The running cost total [thread] reported after its last recorded turn. *)
let cost_total_of session thread =
  List.rev (Session.external_turns session)
  |> List.find_map ~f:(fun (turn : Session.External_turn.t) ->
    if String.equal turn.thread_id thread then turn.cost_total else None)
;;

let live t (s : Engine_intf.Services.t) (input : Engine_intf.Turn.Input.t) =
  let host = t.host () in
  (* The session moved to another tool host: its harness runs there now,
     on a thread of its own there. *)
  (match t.live with
   | Some (Live ((module A), a))
     when not (Option.equal String.equal t.live_host (Some host)) ->
     A.stop a;
     t.live <- None;
     t.thread_id <- None
   | Some _ | None -> ());
  match t.live with
  | Some (Live ((module A), a)) when A.alive a -> Ok (Live ((module A), a))
  | _ ->
    let (module A) = t.adapter in
    let base = t.start ~cwd:s.cwd in
    let resume = Option.first_some t.thread_id base.resume in
    let start =
      { base with
        cost_total =
          Option.first_some
            (Option.bind resume ~f:(cost_total_of s.session))
            base.cost_total
      ; model = input.model
      ; thinking = input.thinking
      ; approval = input.approval
      ; plan = input.plan
      ; resume
      ; fork = base.fork && Option.is_none t.thread_id
      }
    in
    let started =
      match start_process t s start, resume with
      | Error (`Start e), Some thread ->
        (* Its thread cannot be continued (gone, or in use elsewhere): a new
           one, handed the whole session. *)
        s.notice
          (sprintf
             "%s could not continue its thread %s (%s): starting a new one, \
              with the session handed over"
             A.display_name
             thread
             (Error.to_string_hum e));
        t.thread_id <- None;
        t.fresh <- true;
        start_process
          t
          s
          { start with resume = None; fork = false; cost_total = None }
      | started, _ -> started
    in
    (match started with
     | Ok live ->
       t.live <- Some live;
       t.live_host <- Some host;
       Ok live
     | Error (`Spawn e | `Start e) -> Error e)
;;

(* What the harness has not seen of the session, handed to it before the
   turn's prompt: everything when it starts a thread on a session with
   history (after another adapter, on another host, or when its thread
   could not be continued), else what came after its last turn. Recorded in
   the session. *)
let handoff t (s : Engine_intf.Services.t) ~pending =
  let (module A) = t.adapter in
  match Adapter_id.of_string A.id with
  | Error _ -> None
  | Ok to_ when not (Handoff.needed to_) -> None
  | Ok to_ ->
    let from =
      match t.handoff_from with
      | Some _ as from -> from
      | None ->
        (match Session.threads s.session with
         | thread :: _ when not (String.equal thread.adapter A.id) ->
           Result.ok (Adapter_id.of_string thread.adapter)
           |> Option.map ~f:(fun adapter ->
             { Engine_choice.engine = Composer; adapter = Some adapter })
         | _ :: _ | [] -> None)
    in
    let from_host, from_cwd =
      match t.last_place with
      | Some (host, cwd) -> Some host, Some cwd
      | None -> None, None
    in
    let host = t.host () in
    let handoff =
      Handoff.build
        ~session:s.session
        ~from
        ~fresh:t.fresh
        ~pending
        ?from_host
        ?from_cwd
        ~to_
        ~host
        ~cwd:s.cwd
        ()
    in
    t.fresh <- false;
    t.handoff_from <- None;
    t.last_place <- Some (host, s.cwd);
    Option.iter handoff ~f:(fun (handoff : Handoff.t) ->
      ignore
        (Session.record_handoff
           s.session
           ~from_adapter:
             (Option.map from ~f:(fun (from : Engine_choice.t) ->
                match from.adapter with
                | Some adapter -> Adapter_id.to_string adapter
                | None -> Engine_id.to_string from.engine))
           ~to_adapter:A.id
           ~text:handoff.text
         : Session.Entry.t));
    handoff
;;

let input_of (messages : Message.t list) : Adapter.Input.t =
  let users =
    List.filter_map messages ~f:(function
      | User u -> Some u
      | Assistant _ | Tool_result _ -> None)
  in
  { text = String.concat ~sep:"\n\n" (List.map users ~f:(fun u -> u.text))
  ; images = List.concat_map users ~f:(fun u -> u.images)
  }
;;

(* A call the approvals know nothing of: asked about in every mode but
   [auto]. *)
let stand_in name : Tool.t =
  { spec =
      { name
      ; description = ""
      ; parameters = `Object []
      ; parallel_safe = false
      ; destructive = Always
      ; on_host = true
      ; freeform = None
      ; native = None
      }
  ; run = (fun _ _ -> Tool_result.error "run by the external harness")
  }
;;

let reply_of_refusal (r : Tool_result.t) : Adapter_event.Approval_reply.t =
  match r.text with
  | "[cancelled]" -> Cancel
  | "[denied by user]" -> Deny None
  | text ->
    (match
       String.chop_prefix text ~prefix:"[denied by user] Their reason: "
     with
     | Some reason -> Deny (Some reason)
     | None -> Deny (Some text))
;;

let kind_name : Adapter_event.Tool_kind.t -> string = function
  | Command -> "command"
  | File_change -> "file_change"
  | Read -> "read"
  | Search -> "search"
  | Web_search -> "web_search"
  | Mcp { server; tool } -> Mcp_tools.tool_name ~server ~tool
  | Todo -> "todo"
  | Subagent -> "subagent"
  | Other name -> name
;;

(* The harness's plan, put to the user: plan mode's refusals would deny
   leaving it otherwise. *)
let approve_plan (s : Engine_intf.Services.t) ~display_name ~request_id ~summary
  : Adapter_event.Approval_reply.t
  =
  let yes = "Start working" in
  match
    s.ask_user
      ~call_id:request_id
      ~timeout_seconds:None
      [ { question = sprintf "%s's plan:\n\n%s" display_name summary
        ; header = Some "Plan"
        ; choices =
            [ { label = yes; description = None }
            ; { label = "Keep planning"; description = None }
            ]
        ; multiple = false
        }
      ]
  with
  | Answered [ [ answer ] ] when String.equal answer yes -> Allow
  | Answered [ [ "Keep planning" ] ] -> Deny None
  | Answered answers ->
    Deny (Some (String.concat ~sep:"\n" (List.concat answers)))
  | Declined -> Deny None
  | Cancelled | Timed_out _ | Unavailable _ -> Cancel
;;

let rec run_turn
          t
          (s : Engine_intf.Services.t)
          ~cancel
          (input : Engine_intf.Turn.Input.t)
  =
  let (module A) = t.adapter in
  (* Cancelled when the run is, and when the turn ends however it does:
     what prigh's tools were asked over MCP for it is withdrawn then. *)
  let turn_cancel = Cancellation.create () in
  Fiber.fork_daemon ~sw:t.sw (fun () ->
    Fiber.first
      (fun () -> Cancellation.await cancel)
      (fun () -> Cancellation.await turn_cancel);
    Cancellation.cancel turn_cancel;
    `Stop_daemon);
  t.services <- Some s;
  t.cancel <- turn_cancel;
  Exn.protect
    ~finally:(fun () -> Cancellation.cancel turn_cancel)
    ~f:(fun () ->
      match run_turn' t s ~cancel input with
      | added -> added
      | exception exn ->
        (* Its events may still come, for a turn that is no longer
           anyone's: the next turn starts a new process instead. *)
        t.services <- None;
        close t;
        raise exn)

and run_turn'
      t
      (s : Engine_intf.Services.t)
      ~cancel
      (input : Engine_intf.Turn.Input.t)
  =
  let (module A) = t.adapter in
  let sent = ref false in
  let last_turn = ref None in
  let cost_total = ref None in
  let recorder =
    Composer_recorder.create
      ~adapter:A.id
      ~model:(Option.first_some input.model t.thread_model)
      ~emit:s.emit
      ~notice:s.notice
      ~prompts:input.prompts
  in
  let fail message =
    Composer_recorder.feed
      recorder
      (Turn_end { turn_id = None; stop = Failed { kind = Other; message } })
  in
  (match Cancellation.protect cancel ~f:(fun () -> live t s input) with
   | None ->
     Composer_recorder.feed
       recorder
       (Turn_end { turn_id = None; stop = Interrupted })
   | Some (Error e) ->
     fail
       (sprintf "%s could not start: %s" A.display_name (Error.to_string_hum e))
   | Some (Ok (Live ((module L), a))) ->
     let settings = settings input in
     let leftover = Queue.create () in
     let handoff =
       ref
         (Option.map
            (handoff t s ~pending:(List.length input.prompts))
            ~f:(fun (h : Handoff.t) -> h.text))
     in
     let with_handoff (input : Adapter.Input.t) =
       match !handoff with
       | None -> input
       | Some text ->
         handoff := None;
         { input with text = text ^ "\n\nThe user's message:\n\n" ^ input.text }
     in
     let rec turn ~first messages =
       let result =
         match input.kind with
         | Compact { instructions } when first ->
           L.compact a settings ~instructions
         | Compact _ | Prompt ->
           L.send a settings (with_handoff (input_of messages))
       in
       match result with
       | Error e -> fail (Error.to_string_hum e)
       | Ok () ->
         sent := true;
         cost_total := Some (fun () -> L.cost_total a);
         let requests = String.Table.create () in
         let stop = ref None in
         (* Each request waits in a fiber of its own, until answered,
            withdrawn, the run cancelled or the turn ended: its dialog
            closes then ({!Agent.confirm} and [ask_user] clean up when
            cancelled). Not daemons: the turn waits for them to close. *)
         let handle_request ~sw request_id f =
           let c = Cancellation.create () in
           Hashtbl.set requests ~key:request_id ~data:c;
           Fiber.fork ~sw (fun () ->
             Fiber.first
               (fun () -> f c)
               (fun () ->
                  Fiber.first
                    (fun () -> Cancellation.await c)
                    (fun () -> Cancellation.await cancel));
             Hashtbl.remove requests request_id)
         in
         let rec events ~sw =
           match L.next_event a with
           | Turn_end { stop = st; turn_id } as e ->
             Composer_recorder.feed recorder e;
             Option.iter turn_id ~f:(fun id -> last_turn := Some id);
             stop := Some st
           | Approval_request { request_id; tool_id; kind; summary; input; _ }
             ->
             handle_request ~sw request_id (fun c ->
               let reply =
                 match kind with
                 | Other "ExitPlanMode" ->
                   approve_plan
                     s
                     ~display_name:A.display_name
                     ~request_id
                     ~summary
                 | _ ->
                   let canonical, parent =
                     match
                       Option.bind tool_id ~f:(fun tool_id ->
                         Composer_recorder.call_started recorder ~tool_id)
                     with
                     | Some (canonical, parent) -> canonical, parent
                     | None ->
                       ( Canonical_tool.of_start
                           ~kind
                           ~native_name:(kind_name kind)
                           ~input
                       , None )
                   in
                   let tool =
                     Option.value_or_thunk
                       (Tools.find canonical.name)
                       ~default:(fun () -> stand_in canonical.name)
                   in
                   let ctx =
                     context
                       s
                       ~cancel:c
                       ~call_id:(Option.value tool_id ~default:request_id)
                       ?agent_id:parent
                       ()
                   in
                   (match s.approve ctx tool canonical.arguments with
                    | None -> Allow
                    | Some refused -> reply_of_refusal refused)
               in
               if not (Cancellation.is_cancelled c)
               then (
                 Option.iter tool_id ~f:(fun tool_id ->
                   Composer_recorder.note_approval recorder ~tool_id reply);
                 L.reply_approval a ~request_id reply));
             events ~sw
           | User_question { request_id; questions } ->
             handle_request ~sw request_id (fun c ->
               let reply =
                 s.ask_user ~call_id:request_id ~timeout_seconds:None questions
               in
               if not (Cancellation.is_cancelled c)
               then L.answer a ~request_id reply);
             events ~sw
           | Request_withdrawn { request_id } ->
             Option.iter
               (Hashtbl.find requests request_id)
               ~f:Cancellation.cancel;
             events ~sw
           | e ->
             (match e with
              | Thread_started { model = Some model; _ } ->
                t.thread_model <- Some model
              | _ -> ());
             Composer_recorder.feed recorder e;
             events ~sw
         in
         let interrupt_on_cancel () =
           Cancellation.await cancel;
           L.interrupt a;
           Fiber.await_cancel ()
         in
         let steer () =
           let rec loop () =
             Eio_unix.sleep (Time_float.Span.to_sec t.steer_poll);
             (match s.steer () with
              | [] -> ()
              | messages ->
                (match L.steer a (input_of messages) with
                 | Ok () ->
                   List.iter
                     messages
                     ~f:(Composer_recorder.user_message recorder)
                 | Error _ -> Queue.enqueue_all leftover messages));
             loop ()
           in
           loop ()
         in
         Switch.run (fun sw ->
           Fiber.any [ (fun () -> events ~sw); interrupt_on_cancel; steer ];
           Hashtbl.iter requests ~f:Cancellation.cancel);
         note_thread t (module L) a;
         (match !stop with
          | Some Done when not (Cancellation.is_cancelled cancel) ->
            let next = Queue.to_list leftover @ s.steer () in
            Queue.clear leftover;
            if (not (List.is_empty next)) && Option.is_none (s.halt ())
            then (
              List.iter next ~f:(Composer_recorder.user_message recorder);
              turn ~first:false next)
          | _ ->
            (* Not sent: kept in the session, for the user to send again. *)
            Queue.iter leftover ~f:(Composer_recorder.user_message recorder);
            Queue.clear leftover)
     in
     turn ~first:true input.prompts);
  (match t.live with
   | Some (Live ((module L), a)) ->
     (match L.models a with
      | [] -> ()
      | models -> t.models <- models)
   | None -> ());
  (let tokens, window = Composer_recorder.context recorder in
   let old_tokens, old_window = t.context in
   t.context
   <- Option.first_some tokens old_tokens, Option.first_some window old_window);
  let cost = Composer_recorder.cost_usd recorder in
  if Float.( > ) cost 0. then s.add_spend ~usage:Usage.zero ~cost_usd:cost;
  t.services <- None;
  let added = Composer_recorder.finish recorder in
  (* What the harness saw, for a later handoff to give it only what came
     after. *)
  (match t.thread_id, Session.head s.session with
   | Some thread_id, Some through when !sent ->
     t.turns <- t.turns + 1;
     ignore
       (Session.record_external_turn
          s.session
          { thread_id
          ; turn_id =
              Option.value !last_turn ~default:(sprintf "turn-%d" t.turns)
          ; through
          ; cost_total = Option.bind !cost_total ~f:(fun f -> f ())
          }
        : Session.Entry.t)
   | _ -> ());
  List.drop added (List.length input.prompts)
;;

module R = struct
  type nonrec t = t

  let engine = Engine_id.Composer

  let capabilities t =
    match t.live with
    | Some (Live ((module L), a)) -> L.current_capabilities a
    | None ->
      let (module A : Adapter.S) = t.adapter in
      A.capabilities
  ;;

  let models t =
    let (module A : Adapter.S) = t.adapter in
    (match t.live with
     | Some (Live ((module L), a)) ->
       (match L.models a with
        | [] -> ()
        | models -> t.models <- models)
     | None -> ());
    Some
      (match t.models with
       | [] -> [ External_model.default ~adapter:A.id ]
       | models -> models)
  ;;

  let context t = t.context
  let run_turn = run_turn
end

let runner t = Runner.create ~close:(fun () -> close t) (module R) t
