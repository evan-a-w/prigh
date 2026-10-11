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
         where they help."
        User_question.max_questions
  ; parameters =
      Tool_args.schema
        ~required:[ "questions" ]
        [ "questions", `Array question_schema, "The questions to ask" ]
  }
;;

let run (context : Tool.Context.t) args =
  match User_question.of_arguments args with
  | Error e -> Tool.Result.error (Error.to_string_hum e)
  | Ok questions ->
    (match context.ask_user ~call_id:context.call_id questions with
     | Answered answers ->
       Tool.Result.ok (User_question.result_text questions answers)
     | Declined ->
       Tool.Result.error
         "[the user declined to answer; carry on with your best judgement or \
          ask differently]"
     | Cancelled -> Tool.Result.error "[cancelled]"
     | Unavailable why -> Tool.Result.error (sprintf "[cannot ask: %s]" why))
;;

let tool = { Tool.spec; run }
