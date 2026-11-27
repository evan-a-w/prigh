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
      ?(on_start = fun ~host:_ ~adapter:_ _ _ -> ())
      ?(login = fun ~adapter:_ -> None)
      ?(no_sandbox = fun ~host:_ ~host_name:_ ~cwd:_ _ -> None)
      ?(catalog = fun ~host:_ _ -> None)
      ?request_timeout
      ?interrupt_grace
      ()
  : Tool_subagent.External.t
  =
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
    match Composer_adapters.find engines adapter_id with
    | Error e ->
      { Tool_subagent.External.messages =
          [ error_assistant ~adapter:adapter_id ~model (Error.to_string_hum e) ]
      ; cost_usd = 0.
      }
    | Ok adapter ->
      let adapter_name = Adapter_id.to_string adapter_id in
      let run_cost = ref 0. in
      List.iter prompts ~f:(fun m ->
        ignore (Session.append_message session m : Session.Entry.t));
      let recorded m = List.mem prompts m ~equal:phys_equal in
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
          ~wake
          ?handoff_from
          ~catalog:(fun () -> catalog ~host:host_id adapter_id)
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
      let input : Engine_intf.Turn.Input.t =
        { context = Session.messages session
        ; prompts
        ; chat = context.chat
        ; thinking
        ; approval
        ; plan
        ; model
        ; kind = Prompt
        }
      in
      let messages =
        Exn.protect
          ~finally:(fun () ->
            Composer_runner.close runner;
            Background_tasks.cancel_all ~discard:true background;
            Background_tasks.wait_all background)
          ~f:(fun () ->
            Runner.run_turn
              (Composer_runner.runner runner)
              services
              ~cancel
              input)
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
