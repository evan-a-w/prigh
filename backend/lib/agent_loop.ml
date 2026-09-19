open! Core
open! Import

module Config = struct
  type t =
    { model : Model.t
    ; thinking : Thinking.t
    ; system : string option
    ; tools : Tool.t list
    ; max_turns : int option
    ; max_tokens : int option
    ; retries : int
    }

  let default_retries = 3
end

let is_retryable_error message =
  List.exists
    [ "HTTP 429"
    ; "HTTP 500"
    ; "HTTP 502"
    ; "HTTP 503"
    ; "HTTP 504"
    ; "connection failed"
    ; "timed out"
    ]
    ~f:(fun prefix -> String.is_prefix message ~prefix)
;;

let cancelled_result (call : Content.Tool_call.t) =
  { Message.Tool_result.tool_call_id = call.id
  ; tool_name = call.name
  ; text = "[cancelled]"
  ; is_error = true
  }
;;

let execute_tool
      ~env
      ~(config : Config.t)
      ~cwd
      ~cancel
      ~emit
      ~depth
      ~(agent_id : string option)
      (call : Content.Tool_call.t)
  =
  let result : Tool.Result.t =
    match
      List.find config.tools ~f:(fun t -> String.equal (Tool.name t) call.name)
    with
    | None -> Tool.Result.error (sprintf "unknown tool %S" call.name)
    | Some tool ->
      (match Content.Tool_call.parse_arguments call with
       | Error e ->
         Tool.Result.error ("invalid arguments: " ^ Error.to_string_hum e)
       | Ok args ->
         let context =
           Tool.Context.create
             ~cancel
             ~on_output:(fun chunk ->
               emit (Agent_event.Tool_output { call_id = call.id; chunk }))
             ~emit
             ~depth
             ?agent_id
             ~call_id:call.id
             ~tools:config.tools
             ~env
             ~cwd
             ()
         in
         Tool.execute tool context args)
  in
  { Message.Tool_result.tool_call_id = call.id
  ; tool_name = call.name
  ; text = result.text
  ; is_error = result.is_error
  }
;;

let run
      ~env
      ~(provider : Provider.t)
      ~(config : Config.t)
      ~cwd
      ?(cancel = Cancellation.never)
      ?(depth = 0)
      ?agent_id
      ?(steer = fun () -> [])
      ?(emit = ignore)
      ?retry_delay
      ~context
      ~prompts
      ()
  =
  let messages = ref (List.rev_append (List.rev context) prompts) in
  let added = ref (List.rev prompts) in
  let append m =
    messages := !messages @ [ m ];
    added := m :: !added;
    emit (Agent_event.Message_start m);
    emit (Message_end m)
  in
  emit Agent_start;
  List.iter prompts ~f:(fun m ->
    emit (Message_start m);
    emit (Message_end m));
  let retry_delay =
    match retry_delay with
    | Some f -> f
    | None ->
      fun ~attempt ->
        Eio.Time.sleep
          (Eio.Stdenv.clock env)
          (Float.of_int (1 lsl (attempt - 1)))
  in
  let turns = ref 0 in
  let continue_ = ref true in
  while !continue_ do
    incr turns;
    emit Turn_start;
    let request =
      { Provider.Request.model = config.model
      ; system = config.system
      ; messages = !messages
      ; tools = Tools.specs config.tools
      ; thinking = config.thinking
      ; max_tokens = config.max_tokens
      }
    in
    let stream () =
      let builder = Assistant_builder.create ~model:config.model.id in
      emit (Message_start (Assistant (Assistant_builder.snapshot builder)));
      provider.stream request ~cancel ~on_event:(fun delta ->
        Assistant_builder.apply builder delta;
        emit
          (Message_update
             { partial = Assistant_builder.snapshot builder; delta }))
    in
    let rec attempt n =
      let assistant = stream () in
      match assistant.stop_reason with
      | Error message
        when n <= config.retries
             && is_retryable_error message
             && not (Cancellation.is_cancelled cancel) ->
        retry_delay ~attempt:n;
        attempt (n + 1)
      | _ -> assistant
    in
    let assistant = attempt 1 in
    messages := !messages @ [ Assistant assistant ];
    added := Assistant assistant :: !added;
    emit (Message_end (Assistant assistant));
    let calls = Message.Assistant.tool_calls assistant in
    let execute call =
      if Cancellation.is_cancelled cancel
      then cancelled_result call
      else (
        emit (Tool_start call);
        let result =
          execute_tool ~env ~config ~cwd ~cancel ~emit ~depth ~agent_id call
        in
        emit (Tool_end { call; result });
        result)
    in
    let parallel =
      List.length calls > 1
      && List.for_all calls ~f:(fun call ->
        match
          List.find config.tools ~f:(fun t ->
            String.equal (Tool.name t) call.name)
        with
        | Some tool -> tool.spec.parallel_safe
        | None -> false)
    in
    let tool_results =
      match assistant.stop_reason with
      | Aborted | Error _ ->
        (* Keep the context valid: every tool call needs a result. *)
        List.map calls ~f:(fun call ->
          let result = cancelled_result call in
          append (Tool_result result);
          result)
      | End_turn | Tool_use | Length ->
        if parallel
        then (
          let results = Fiber.List.map execute calls in
          List.map results ~f:(fun result ->
            append (Tool_result result);
            result))
        else
          List.map calls ~f:(fun call ->
            let result = execute call in
            append (Tool_result result);
            result)
    in
    emit (Turn_end { assistant; tool_results });
    let stopped =
      match assistant.stop_reason with
      | Aborted | Error _ | Length -> true
      | End_turn | Tool_use -> List.is_empty calls
    in
    let over_limit =
      match config.max_turns with
      | Some n -> !turns >= n
      | None -> false
    in
    if stopped || over_limit || Cancellation.is_cancelled cancel
    then continue_ := false
    else List.iter (steer ()) ~f:append
  done;
  let added = List.rev !added in
  emit (Agent_end added);
  added
;;
