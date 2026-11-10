open! Core
open! Import

let name = "answer_session"

let spec =
  { Tool_spec.name
  ; parallel_safe = true
  ; on_host = false
  ; freeform = None
  ; native = None
  ; destructive = Never
  ; description =
      "Answer a question another session asked this one, only after a message \
       \"[session ... asks qN]\": the answer goes to that session, not to the \
       user."
  ; parameters =
      Tool_args.schema
        ~required:[ "id"; "answer" ]
        [ "id", `String, "The question's id, as its message gave it (q3)"
        ; ( "answer"
          , `String
          , "The answer, self-contained and concise: that session sees nothing \
             else of this one" )
        ]
  }
;;

let create ~current ~questions () =
  let run (_ : Tool.Context.t) args =
    let id = String.strip (Tool_args.string args "id") in
    let answer = String.strip (Tool_args.string args "answer") in
    if String.is_empty answer
    then Tool.Result.error "answer is empty: say what you found"
    else (
      match current () with
      | None -> Tool.Result.error "no session has asked this one anything"
      | Some (session : Session_questions.Party.t) ->
        (match
           Session_questions.answer questions ~session:session.id ~id answer
         with
         | Ok () -> Tool.Result.ok "sent"
         | Error e -> Tool.Result.error (Error.to_string_hum e)))
  in
  { Tool.spec; run }
;;
