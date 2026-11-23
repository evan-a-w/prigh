open! Core
open! Import

type live = Live : (module Adapter.S with type t = 'a) * 'a -> live

type t =
  { sw : Switch.t
  ; adapter : Adapter.t
  ; spawn : Adapter.Launch.t -> Line_transport.t Or_error.t
  ; start : cwd:string -> Adapter.Start.t
  ; interrupt_grace : Time_float.Span.t option
  ; steer_poll : Time_float.Span.t
  ; mutable live : live option
  ; mutable thread_id : string option
  ; on_thread : string -> unit
  ; mutable services : Engine_intf.Services.t option (* the running turn's *)
  ; mutable cancel : Cancellation.t
  }

let create
      ~sw
      ~adapter
      ~spawn
      ?(start = fun ~cwd -> Adapter.Start.default ~cwd)
      ?interrupt_grace
      ?(steer_poll = Time_float.Span.of_sec 0.2)
      ?(on_thread = ignore)
      ()
  =
  { sw
  ; adapter
  ; spawn
  ; start
  ; interrupt_grace
  ; steer_poll
  ; live = None
  ; thread_id = None
  ; on_thread
  ; services = None
  ; cancel = Cancellation.never
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
  Injected_tools.create
    ~tools:(Injected_tools.select ~native_questions:true s.tools)
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
         | None -> s.execute ctx tool args))
;;

let settings t ~cwd (input : Engine_intf.Turn.Input.t) : Adapter.Settings.t =
  { model = (t.start ~cwd).model
  ; thinking = input.thinking
  ; approval = input.approval
  ; plan = input.plan
  }
;;

let live t (s : Engine_intf.Services.t) (input : Engine_intf.Turn.Input.t) =
  match t.live with
  | Some (Live ((module A), a)) when A.alive a -> Ok (Live ((module A), a))
  | _ ->
    let (module A) = t.adapter in
    let base = t.start ~cwd:s.cwd in
    let resume = Option.first_some t.thread_id base.resume in
    let start =
      { base with
        thinking = input.thinking
      ; approval = input.approval
      ; plan = input.plan
      ; resume
      ; fork = base.fork && Option.is_none t.thread_id
      }
    in
    let%bind.Or_error transport = t.spawn (A.launch start) in
    (match
       A.start
         ~sw:t.sw
         ?interrupt_grace:t.interrupt_grace
         ~injected:(injected t s)
         transport
         start
     with
     | Ok a ->
       let live = Live ((module A), a) in
       t.live <- Some live;
       Ok live
     | Error _ as e ->
       Line_transport.stop transport;
       e)
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
    (match String.chop_prefix text ~prefix:"[denied by user] Their reason: " with
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

let run_turn
      t
      (s : Engine_intf.Services.t)
      ~cancel
      (input : Engine_intf.Turn.Input.t)
  =
  let (module A) = t.adapter in
  t.services <- Some s;
  t.cancel <- cancel;
  let recorder =
    Composer_recorder.create
      ~adapter:A.id
      ~model:(t.start ~cwd:s.cwd).model
      ~emit:s.emit
      ~notice:s.notice
      ~prompts:input.prompts
  in
  let fail message =
    Composer_recorder.feed
      recorder
      (Turn_end { turn_id = None; stop = Failed { kind = Other; message } })
  in
  (match live t s input with
   | Error e ->
     fail (sprintf "%s could not start: %s" A.display_name (Error.to_string_hum e))
   | Ok (Live ((module L), a)) ->
     let settings = settings t ~cwd:s.cwd input in
     let leftover = Queue.create () in
     let rec turn messages =
       match L.send a settings (input_of messages) with
       | Error e -> fail (Error.to_string_hum e)
       | Ok () ->
         let requests = String.Table.create () in
         let stop = ref None in
         let handle_request ~sw request_id f =
           let c = Cancellation.create () in
           Hashtbl.set requests ~key:request_id ~data:c;
           Fiber.fork_daemon ~sw (fun () ->
             f c;
             Hashtbl.remove requests request_id;
             `Stop_daemon)
         in
         let rec events ~sw =
           match L.next_event a with
           | Turn_end { stop = st; _ } as e ->
             Composer_recorder.feed recorder e;
             stop := Some st
           | Approval_request { request_id; tool_id; kind; summary; input; _ } ->
             handle_request ~sw request_id (fun c ->
               let reply =
                 match kind with
                 | Other "ExitPlanMode" ->
                   approve_plan s ~display_name:A.display_name ~request_id ~summary
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
             Option.iter (Hashtbl.find requests request_id) ~f:Cancellation.cancel;
             events ~sw
           | e ->
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
                   List.iter messages ~f:(Composer_recorder.user_message recorder)
                 | Error _ -> Queue.enqueue_all leftover messages));
             loop ()
           in
           loop ()
         in
         Switch.run (fun sw ->
           Fiber.any [ (fun () -> events ~sw); interrupt_on_cancel; steer ];
           Hashtbl.iter requests ~f:Cancellation.cancel);
         (match L.thread_id a with
          | Some id when not (Option.equal String.equal (Some id) t.thread_id) ->
            t.thread_id <- Some id;
            t.on_thread id
          | Some _ | None -> ());
         (match !stop with
          | Some Done when not (Cancellation.is_cancelled cancel) ->
            let next = Queue.to_list leftover @ s.steer () in
            Queue.clear leftover;
            if (not (List.is_empty next)) && Option.is_none (s.halt ())
            then (
              List.iter next ~f:(Composer_recorder.user_message recorder);
              turn next)
          | _ ->
            (* Not sent: kept in the session, for the user to send again. *)
            Queue.iter leftover ~f:(Composer_recorder.user_message recorder);
            Queue.clear leftover)
     in
     turn input.prompts);
  let cost = Composer_recorder.cost_usd recorder in
  if Float.( > ) cost 0. then s.add_spend ~usage:Usage.zero ~cost_usd:cost;
  t.services <- None;
  let added = Composer_recorder.finish recorder in
  List.drop added (List.length input.prompts)
;;

module R = struct
  type nonrec t = t

  let engine = Engine_id.Composer

  let capabilities t =
    let (module A : Adapter.S) = t.adapter in
    A.capabilities
  ;;

  let run_turn = run_turn
end

let runner t = Runner.create ~close:(fun () -> close t) (module R) t
