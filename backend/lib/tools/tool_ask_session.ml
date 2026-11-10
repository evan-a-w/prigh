open! Core
open! Import

let name = "ask_session"

let spec =
  { Tool_spec.name
  ; parallel_safe = true
  ; on_host = false
  ; freeform = None
  ; destructive = Never
  ; description =
      "Ask another session a question, interrupting its work and costing it a \
       run: only when the user asks or refers to that session, or the task \
       plainly depends on what only that session can tell or do; never \
       routinely. To look something up in a session, use sessions instead \
       (read-only, cheap). The session's own agent gets the question (at its \
       next step if it is working), can use its tools, and answers when it \
       can. This returns at once: the answer arrives later as a message \
       starting with \"[session ...\", so keep working or end your turn \
       meanwhile. Ask one specific, self-contained question: that session \
       knows nothing of this one."
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

let create ~sessions_dir ~current ~questions () =
  let run (_ : Tool.Context.t) args =
    let key = Tool_args.string args "session" in
    let question = String.strip (Tool_args.string args "question") in
    if String.is_empty question
    then Tool.Result.error "question is empty: say what you want to know"
    else (
      match Tool_sessions.find ~dir:(sessions_dir ()) key, current () with
      | Error e, _ -> Tool.Result.error (Error.to_string_hum e)
      | Ok _, None -> Tool.Result.error "this session cannot be answered yet"
      | Ok summary, Some (asker : Session_questions.Party.t)
        when String.equal asker.id summary.id ->
        Tool.Result.error
          "that is this session: its conversation is your own context, so \
           answer from it"
      | Ok summary, Some asker ->
        let target =
          { Session_questions.Party.id = summary.id; name = summary.name }
        in
        (match Session_questions.ask questions ~asker ~target ~question with
         | Error e ->
           Tool.Result.error
             (sprintf "%s was not asked: %s" summary.id (Error.to_string_hum e))
         | Ok asked ->
           Tool.Result.ok
             (sprintf
                "asked %s%s as %s; its answer arrives later as a message: keep \
                 working on what does not depend on it, or end your turn"
                summary.id
                (Option.value_map summary.name ~default:"" ~f:(sprintf " (%s)"))
                asked.id)))
  in
  { Tool.spec; run }
;;
