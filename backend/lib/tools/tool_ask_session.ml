open! Core
open! Import

let name = "ask_session"
let answer_cut = 6000

let spec =
  { Tool_spec.name
  ; parallel_safe = true
  ; on_host = false
  ; freeform = None
  ; destructive = Never
  ; description =
      "Ask another saved session a question, only when the user refers to it \
       or asks, or the task plainly depends on what that session knows: not \
       routinely. One model call answers from that session's conversation, \
       without tools and without changing it, and only its answer comes back. \
       Ask one specific, self-contained question: the other session knows \
       nothing of this one."
  ; parameters =
      Tool_args.schema
        ~required:[ "session"; "question" ]
        [ ( "session"
          , `String
          , "The session's id or a unique prefix of it (the sessions tool's \
             search finds it)" )
        ; "question", `String, "The question"
        ]
  }
;;

let fallback_system =
  "You are prigh, a coding agent. This is the conversation of one of your \
   sessions."
;;

let ask
      ~provider
      ~resolve_model
      ~current_model
      ~record_spend
      (context : Tool.Context.t)
      session
      ~question
  =
  let model =
    match Session.model session with
    | None -> current_model ()
    | Some (key, (_ : Thinking.t)) ->
      (match resolve_model key with
       | Ok model -> model
       | Error _ -> current_model ())
  in
  let request =
    Btw.request
      ~asker:Session
      ~cache_key:(Session.id session)
      ~model
      ~system:
        (Option.value (Session.system_prompt session) ~default:fallback_system)
      ~messages:(Session.messages session)
      ~question
      ()
  in
  let reply : Message.Assistant.t =
    provider.Provider.stream request ~cancel:context.cancel ~on_event:(function
      | Text_delta text -> context.on_output text
      | Thinking_delta _
      | Thinking_signature _
      | Tool_call_start _
      | Tool_call_delta _
      | Server_tool_use _
      | Server_tool_result _ -> ())
  in
  record_spend ~usage:reply.usage ~cost_usd:(Model.cost_usd model reply.usage);
  match reply.stop_reason with
  | Error e ->
    Tool.Result.error (sprintf "%s could not answer: %s" model.name e)
  | Aborted -> Tool.Result.error "cancelled"
  | End_turn | Tool_use | Length ->
    (match String.strip (Message.Assistant.text reply) with
     | "" -> Tool.Result.error "the session gave no answer"
     | answer ->
       let cut =
         Truncate.head ~max_bytes:answer_cut ~max_lines:Int.max_value answer
       in
       Tool.Result.ok
         (if cut.truncated
          then cut.text ^ "\n[the answer was cut here: ask for less]"
          else answer))
;;

let create
      ~sessions_dir
      ~current
      ~provider
      ~resolve_model
      ~current_model
      ~record_spend
      ()
  =
  let run (context : Tool.Context.t) args =
    let key = Tool_args.string args "session" in
    let question = String.strip (Tool_args.string args "question") in
    if String.is_empty question
    then Tool.Result.error "question is empty: say what you want to know"
    else (
      match Tool_sessions.find ~dir:(sessions_dir ()) key with
      | Error e -> Tool.Result.error (Error.to_string_hum e)
      | Ok summary when [%equal: string option] (current ()) (Some summary.id)
        ->
        Tool.Result.error
          "that is this session: its conversation is your own context, so \
           answer from it"
      | Ok summary ->
        (match Session.load ~yield:Fiber.yield summary.path with
         | Error e -> Tool.Result.error (Error.to_string_hum e)
         | Ok session when List.is_empty (Session.messages session) ->
           Tool.Result.error "that session has no messages to answer from"
         | Ok session ->
           ask
             ~provider
             ~resolve_model
             ~current_model
             ~record_spend
             context
             session
             ~question))
  in
  { Tool.spec; run }
;;
