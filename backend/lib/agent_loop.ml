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
    ; cache_key : string option
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
  && not (Usage_limit.unavailable message)
;;

let cancelled_result (call : Content.Tool_call.t) =
  { Message.Tool_result.tool_call_id = call.id
  ; tool_name = call.name
  ; text = "[cancelled]"
  ; is_error = true
  ; images = []
  }
;;

let tool_summary ~name (args : Json.t) =
  match args with
  | `Object fields ->
    let find name = List.Assoc.find fields ~equal:String.equal name in
    (match find "command", find "path", find "input" with
     | Some (`String s), _, _ -> s
     | _, Some (`String s), _ -> s
     | _, _, Some (`String patch) when String.equal name "apply_patch" ->
       String.concat ~sep:", " (Apply_patch.paths patch)
     | _, _, Some (`String input) when String.equal name "job_input" ->
       (match find "id" with
        | Some (`String id) -> sprintf "%s: %S" id input
        | _ -> sprintf "%S" input)
     | _ -> "")
  | _ -> ""
;;

let execute_tool
      ~env
      ~(config : Config.t)
      ~cwd
      ~cancel
      ~emit
      ~depth
      ~(agent_id : string option)
      ~halt
      ?approve
      ?ask_user
      ?execute
      ?background
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
             ?execute
             ?approve
             ?ask_user
             ~on_output:(fun chunk ->
               emit (Agent_event.Tool_output { call_id = call.id; chunk }))
             ~emit
             ~depth
             ?agent_id
             ~call_id:call.id
             ?cache_key:config.cache_key
             ~halt
             ~tools:config.tools
             ?background
             ~env
             ~cwd
             ()
         in
         (match context.approve context tool args with
          | Some refusal -> refusal
          | None -> Tool.execute_via context tool args))
  in
  { Message.Tool_result.tool_call_id = call.id
  ; tool_name = call.name
  ; text = result.text
  ; is_error = result.is_error
  ; images = result.images
  }
;;

let overflow_attempts = 2

(* A repeated overflow keeps less of the conversation's end. *)
let overflow_keep ~attempt =
  if attempt <= 1
  then Compaction.default_keep_recent_tokens
  else Compaction.default_keep_recent_tokens / 4
;;

let unrecoverable_overflow message ~reason =
  Stop_reason.Error
    (sprintf
       "%s (%s; /compact with instructions, /model a model with a larger \
        context window, or /new)"
       message
       reason)
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
      ?approve
      ?ask_user
      ?execute
      ?background
      ?retry_delay
      ?compact
      ?(halt = fun () -> None)
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
  let compact =
    match compact with
    | Some f -> f
    | None ->
      fun ~keep_recent_tokens ~cancel messages ->
        Compaction.compact_messages
          ~provider
          ~model:config.model
          ~keep_recent_tokens
          ~cancel
          messages
  in
  (* Tokens of the latest response's request and reply, while they still
     describe the context (until it is compacted). *)
  let last_tokens = ref None in
  let compact_context ~keep_recent_tokens =
    Or_error.map
      (compact ~keep_recent_tokens ~cancel !messages)
      ~f:(fun (context, (outcome : Compaction.Outcome.t)) ->
        messages := context;
        last_tokens := None;
        emit
          (Compacted
             { summary = outcome.summary
             ; usage = outcome.usage
             ; cost_usd = Model.cost_usd config.model outcome.usage
             }))
  in
  let compaction_failed = ref false in
  let compact_if_large () =
    match !last_tokens with
    | Some (tokens, since) when not !compaction_failed ->
      let tokens =
        tokens + Compaction.estimate_tokens (List.drop !messages since)
      in
      if Compaction.should_compact config.model ~input_tokens:tokens
      then (
        match
          compact_context
            ~keep_recent_tokens:Compaction.default_keep_recent_tokens
        with
        | Ok () -> ()
        | Error _ -> compaction_failed := true)
    | Some _ | None -> ()
  in
  let turns = ref 0 in
  let continue_ = ref true in
  while !continue_ do
    if !turns > 0 then compact_if_large ();
    match halt () with
    | Some _ -> continue_ := false
    | None ->
      incr turns;
      emit Turn_start;
      let stream () =
        let request =
          { Provider.Request.model = config.model
          ; system = config.system
          ; messages = !messages
          ; tools = Tools.specs config.tools
          ; thinking = config.thinking
          ; max_tokens = config.max_tokens
          ; cache_key = config.cache_key
          }
        in
        let builder = Assistant_builder.create ~model:config.model.id in
        emit (Message_start (Assistant (Assistant_builder.snapshot builder)));
        provider.stream request ~cancel ~on_event:(fun delta ->
          Assistant_builder.apply builder delta;
          emit
            (Message_update
               { partial = Assistant_builder.snapshot builder; delta }))
      in
      (* An overflowing request is compacted and sent again, up to
         [overflow_attempts] times; the failed reply is dropped. *)
      let rec attempt n ~overflows =
        let assistant = stream () in
        match assistant.stop_reason with
        | Error message
          when Context_overflow.matches message
               && not (Cancellation.is_cancelled cancel) ->
          if overflows >= overflow_attempts
          then
            { assistant with
              stop_reason =
                unrecoverable_overflow
                  message
                  ~reason:
                    (sprintf
                       "still too long after compacting %d times"
                       overflows)
            }
          else (
            match
              compact_context
                ~keep_recent_tokens:(overflow_keep ~attempt:(overflows + 1))
            with
            | Ok () -> attempt n ~overflows:(overflows + 1)
            | Error e ->
              { assistant with
                stop_reason =
                  unrecoverable_overflow
                    message
                    ~reason:
                      ("compacting to make it fit failed: "
                       ^ Error.to_string_hum e)
              })
        | Error message
          when n <= config.retries
               && is_retryable_error message
               && not (Cancellation.is_cancelled cancel) ->
          retry_delay ~attempt:n;
          attempt (n + 1) ~overflows
        | _ -> assistant
      in
      let assistant = attempt 1 ~overflows:0 in
      messages := !messages @ [ Assistant assistant ];
      added := Assistant assistant :: !added;
      emit (Message_end (Assistant assistant));
      (match assistant.stop_reason with
       | Error _ | Aborted -> ()
       | End_turn | Tool_use | Length ->
         last_tokens
         := Some
              ( assistant.usage.input + assistant.usage.output
              , List.length !messages ));
      let calls = Message.Assistant.tool_calls assistant in
      let execute call =
        if Cancellation.is_cancelled cancel
        then cancelled_result call
        else (
          emit (Tool_start call);
          let result =
            execute_tool
              ~env
              ~config
              ~cwd
              ~cancel
              ~emit
              ~depth
              ~agent_id
              ~halt
              ?approve
              ?ask_user
              ?execute
              ?background
              call
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
