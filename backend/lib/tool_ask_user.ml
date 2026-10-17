open! Core
open! Import

let name = "ask_user"

let choice_schema =
  `Object
    [ "type", `String "object"
    ; ( "properties"
      , `Object
          [ ( "label"
            , `Object
                [ "type", `String "string"
                ; "description", `String "The choice as shown (1-5 words)"
                ] )
          ; ( "description"
            , `Object
                [ "type", `String "string"
                ; ( "description"
                  , `String "Optional: one short sentence on what it implies" )
                ] )
          ] )
    ; "required", `Array [ `String "label" ]
    ; "additionalProperties", `False
    ]
;;

let question_schema =
  `Object
    [ "type", `String "object"
    ; ( "properties"
      , `Object
          [ ( "question"
            , `Object
                [ "type", `String "string"
                ; "description", `String "The question, in one sentence"
                ] )
          ; ( "header"
            , `Object
                [ "type", `String "string"
                ; "description", `String "Optional short title (a few words)"
                ] )
          ; ( "options"
            , `Object
                [ "type", `String "array"
                ; "items", choice_schema
                ; ( "description"
                  , `String
                      "Optional choices, recommended first. The user can \
                       always answer in their own words instead, so do not add \
                       an \"Other\" choice." )
                ] )
          ; ( "multiple"
            , `Object
                [ "type", `String "boolean"
                ; ( "description"
                  , `String
                      "Whether several options may be picked (default false)" )
                ] )
          ] )
    ; "required", `Array [ `String "question" ]
    ; "additionalProperties", `False
    ]
;;

let spec =
  { Tool_spec.name
  ; parallel_safe = false
  ; on_host = false
  ; freeform = None
  ; destructive = false
  ; description =
      sprintf
        "Asks the user questions and waits for the answers. Use it when you \
         need a decision or information only the user has (a preference, a \
         choice between approaches, missing requirements), not for things you \
         can find out yourself. Ask 1-%d short questions at once, with choices \
         where they help. If the user does not answer in time (the user's \
         setting, 5 minutes unless they changed it), you get a reply saying \
         so: then carry on with your best judgement."
        User_question.max_questions
  ; parameters =
      Tool_args.schema
        ~required:[ "questions" ]
        [ "questions", `Array question_schema, "The questions to ask"
        ; ( "timeout_seconds"
          , `Integer
          , sprintf
              "Optional: wait less than the user's setting before carrying on \
               without the answers (%d-%d seconds). It can only shorten the \
               wait: never past the user's setting or %d seconds, and only the \
               user can turn the timeout off."
              User_question.min_timeout_seconds
              User_question.max_timeout_seconds
              User_question.max_timeout_seconds )
        ]
  }
;;

let timeout_seconds args =
  match Tool_args.int_opt args "timeout_seconds" with
  | Some n when n <= 0 ->
    Error
      (sprintf
         "timeout_seconds must be a number of seconds from %d to %d; only the \
          user can turn the timeout off (leave timeout_seconds out to use \
          their setting)"
         User_question.min_timeout_seconds
         User_question.max_timeout_seconds)
  | requested -> Ok requested
;;

let run (context : Tool.Context.t) args =
  match User_question.of_arguments args, timeout_seconds args with
  | Error e, _ -> Tool.Result.error (Error.to_string_hum e)
  | _, Error e -> Tool.Result.error e
  | Ok questions, Ok timeout_seconds ->
    (match
       context.ask_user ~call_id:context.call_id ~timeout_seconds questions
     with
     | Answered answers ->
       Tool.Result.ok (User_question.result_text questions answers)
     | Declined ->
       Tool.Result.error
         "[the user declined to answer; carry on with your best judgement or \
          ask differently]"
     | Cancelled -> Tool.Result.error "[cancelled]"
     | Timed_out after ->
       Tool.Result.error
         (sprintf
            "[the user did not answer within %s; carry on with your best \
             judgement and say what you assumed]"
            (User_question.duration_hum after))
     | Unavailable why -> Tool.Result.error (sprintf "[cannot ask: %s]" why))
;;

let tool = { Tool.spec; run }
