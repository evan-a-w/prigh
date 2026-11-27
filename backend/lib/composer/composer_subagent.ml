open! Core
open! Import

let error_assistant ~adapter ~model message =
  Message.Assistant
    { content = []
    ; model = External_model.name ~adapter:(Adapter_id.to_string adapter) model
    ; usage = Usage.zero
    ; stop_reason = Error message
    ; account = None
    }
;;

let aborted_assistant ~adapter ~model =
  Message.Assistant
    { content = []
    ; model = External_model.name ~adapter:(Adapter_id.to_string adapter) model
    ; usage = Usage.zero
    ; stop_reason = Aborted
    ; account = None
    }
;;

let create
      ~engines
      ~current
      ~harness_model
      ~external_models
      ~parent_messages
      ~approval_mode
      ~plan_mode
      ~add_spend
      ~notice
      ~spawn
      ?spawn_start
      ?(on_start = fun ~host:_ ~adapter:_ _ _ -> ())
      ?(login = fun ~adapter:_ -> None)
      ?(no_sandbox = fun ~host:_ ~host_name:_ ~cwd:_ _ -> None)
      ?(catalog = fun ~host:_ _ -> None)
      ?request_timeout
      ?interrupt_grace
      ?autonomous_quiet
      ?(open_work_timeout = Time_float.Span.of_sec 1800.)
      ()
  : Tool_subagent.External.t
  =
  let steer_poll_sec =
    Time_float.Span.to_sec
      (Option.value steer_poll ~default:(Time_float.Span.of_sec 0.2))
  in
  let open_work_timeout_sec = Time_float.Span.to_sec open_work_timeout in
  let run
        ~adapter:adapter_id
        ~host_id
        ~host_name
        ~cwd
        ~model
        ~thinking
        ~approval
        ~plan
        ~developer_instructions
        ~resume
        ~handoff_from
        ~session
        ~on_thread
        ~depth
        ~agent_id
        ~home
        ~tools
        ~(context : Tool.Context.t)
        ~emit
        ~steer
        ~await_steer
        ~wake
        ~cancel
        ~prompts
    : Tool_subagent.External.run_result
    =
    match Composer_adapters.find ?autonomous_quiet engines adapter_id with
    | Error e ->
      { Tool_subagent.External.messages =
          [ error_assistant ~adapter:adapter_id ~model (Error.to_string_hum e) ]
      ; cost_usd = 0.
      }
    | Ok adapter ->
      let adapter_name = Adapter_id.to_string adapter_id in
      let run_cost = ref 0. in
      let recorded_prompts = ref [] in
      let record_prompts msgs =
        List.iter msgs ~f:(fun m ->
          recorded_prompts := m :: !recorded_prompts;
          ignore (Session.append_message session m : Session.Entry.t))
      in
      let recorded m = List.mem !recorded_prompts m ~equal:phys_equal in
      record_prompts prompts;
      Switch.run
      @@ fun sw ->
      let background =
        Background_tasks.create
          ~env:context.env
          ~sw
          ~first_subagent_id:1
          ~first_job_id:1
          ()
      in
      Background_tasks.connect background ~emit ~on_change:ignore;
      let autonomous_pending = ref false in
      let wake_cond = Eio.Condition.create () in
      let start ~cwd =
        { (Adapter.Start.default ~cwd) with
          resume
        ; developer_instructions
        ; login = login ~adapter:adapter_name
        ; no_sandbox = no_sandbox ~host:host_id ~host_name ~cwd adapter_id
        }
      in
      let runner =
        Composer_runner.create
          ~sw
          ~adapter
          ~spawn:(fun launch ->
            spawn ~sw ~host:host_id ~host_name ~adapter:adapter_id launch)
          ?spawn_start:
            (Option.map spawn_start ~f:(fun f start launch ->
               f ~sw ~host:host_id ~host_name ~adapter:adapter_id start launch))
          ~start
          ?interrupt_grace
          ?request_timeout
          ~on_thread:(fun tid ->
            ignore
              (Session.record_thread
                 session
                 { adapter = adapter_name
                 ; host = host_id
                 ; thread_id = tid
                 ; cwd
                 ; fork_from = None
                 }
               : Session.Entry.t);
            on_thread tid)
          ~on_start:(fun s res ->
            on_start ~host:host_id ~adapter:adapter_id s res)
          ~host:(fun () -> host_id)
          ?host_id:context.host
          ~home
          ~depth
          ~agent_id
          ?handoff_from
          ~catalog:(fun () -> catalog ~host:host_id adapter_id)
          ~on_autonomous:(fun () ->
            autonomous_pending := true;
            Eio.Condition.broadcast wake_cond)
          ()
      in
      let services : Engine_intf.Services.t =
        { env = context.env
        ; cwd
        ; session
        ; emit =
            (fun event ->
              (match event with
               | Message_end m when not (recorded m) ->
                 ignore (Session.append_message session m : Session.Entry.t)
               | _ -> ());
              emit event)
        ; notice
        ; approve = context.approve
        ; ask_user =
            (fun ~call_id:_ ~timeout_seconds:_ _ ->
              User_question.Reply.Unavailable
                "subagents cannot ask the user directly: make a reasonable \
                 choice or put the question in your report")
        ; ui = context.ui
        ; tools
        ; execute = context.execute
        ; background
        ; steer
        ; await_steer
        ; halt = context.halt
        ; add_spend =
            (fun ~usage ~cost_usd ->
              run_cost := !run_cost +. cost_usd;
              add_spend ~usage ~cost_usd)
        ; set_title = ignore
        }
      in
      let run_one ~kind prompts =
        let input : Engine_intf.Turn.Input.t =
          { context = Session.messages session
          ; prompts
          ; chat = context.chat
          ; thinking
          ; approval
          ; plan
          ; model
          ; kind
          }
        in
        Runner.run_turn (Composer_runner.runner runner) services ~cancel input
      in
      let await_open_work () =
        let await_autonomous () =
          let rec loop () =
            if !autonomous_pending
            then `Autonomous
            else (
              Eio.Condition.await_no_mutex wake_cond;
              loop ())
          in
          loop ()
        in
        let await_steer () =
          let rec loop () =
            match steer () with
            | _ :: _ as msgs -> `Steer msgs
            | [] ->
              Eio_unix.sleep steer_poll_sec;
              loop ()
          in
          loop ()
        in
        let await_exit () =
          Composer_runner.await_exit runner;
          `Exited
        in
        let await_cancel () =
          Cancellation.await cancel;
          `Cancelled
        in
        let await_timeout () =
          Eio_unix.sleep open_work_timeout_sec;
          `Timeout
        in
        Fiber.any
          [ await_autonomous
          ; await_steer
          ; await_exit
          ; await_cancel
          ; await_timeout
          ]
      in
      let messages =
        Exn.protect
          ~finally:(fun () ->
            Composer_runner.close runner;
            Background_tasks.cancel_all ~discard:true background;
            Background_tasks.wait_all background)
          ~f:(fun () ->
            let initial = run_one ~kind:Prompt prompts in
            let rec continue_loop acc =
              if Cancellation.is_cancelled cancel
              then
                if Composer_runner.has_open_work runner || !autonomous_pending
                then acc @ [ aborted_assistant ~adapter:adapter_id ~model ]
                else acc
              else if Option.is_some (context.halt ())
              then acc
              else if !autonomous_pending
              then (
                autonomous_pending := false;
                let more = run_one ~kind:Autonomous [] in
                continue_loop (acc @ more))
              else if Composer_runner.has_open_work runner
              then (
                match await_open_work () with
                | `Autonomous -> continue_loop acc
                | `Steer msgs ->
                  record_prompts msgs;
                  let more = run_one ~kind:Prompt msgs in
                  continue_loop (acc @ more)
                | `Exited ->
                  if !autonomous_pending then continue_loop acc else acc
                | `Cancelled ->
                  acc @ [ aborted_assistant ~adapter:adapter_id ~model ]
                | `Timeout ->
                  notice
                    (sprintf
                       "%s subagent timed out waiting for background work to \
                        finish"
                       adapter_name);
                  acc)
              else (
                match wake () with
                | [] -> acc
                | msgs ->
                  record_prompts msgs;
                  let more = run_one ~kind:Prompt msgs in
                  continue_loop (acc @ more))
            in
            continue_loop initial)
      in
      { Tool_subagent.External.messages; cost_usd = !run_cost }
  in
  { Tool_subagent.External.deployed = engines
  ; current
  ; harness_model
  ; external_models
  ; parent_messages
  ; approval_mode
  ; plan_mode
  ; add_spend
  ; run
  }
;;
