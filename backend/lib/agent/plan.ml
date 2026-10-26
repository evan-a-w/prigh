open! Core
open! Import

module Status = struct
  type t =
    | Pending
    | In_progress
    | Completed
  [@@deriving sexp_of, equal, enumerate]

  let to_string = function
    | Pending -> "pending"
    | In_progress -> "in_progress"
    | Completed -> "completed"
  ;;

  let of_string s = List.find all ~f:(fun t -> String.equal (to_string t) s)
end

module Step = struct
  type t =
    { step : string
    ; status : Status.t
    }
  [@@deriving sexp_of, equal]
end

type t =
  { explanation : string option
  ; steps : Step.t list
  }
[@@deriving sexp_of, equal]

let tool_name = "update_plan"

let of_arguments args =
  Or_error.try_with_join (fun () ->
    let explanation =
      Option.filter (Tool_args.string_opt args "explanation") ~f:(fun s ->
        not (String.is_empty (String.strip s)))
    in
    let items =
      match Tool_args.list_opt args "plan" with
      | Some items -> items
      | None -> raise (Tool_args.Invalid "missing required argument \"plan\"")
    in
    let open Or_error.Let_syntax in
    let%map steps =
      Or_error.all
        (List.mapi items ~f:(fun i item ->
           let step =
             Option.map (Tool_args.string_opt item "step") ~f:String.strip
           in
           let status = Tool_args.string_opt item "status" in
           match step, status with
           | None, _ | Some "", _ ->
             Or_error.errorf "plan[%d] has no step text" i
           | Some step, Some s ->
             (match Status.of_string s with
              | Some status -> Ok { Step.step; status }
              | None ->
                Or_error.errorf
                  "plan[%d].status is %S; use pending, in_progress or completed"
                  i
                  s)
           | Some _, None -> Or_error.errorf "plan[%d] has no status" i))
    in
    match steps with
    | [] -> None
    | steps -> Some { explanation; steps })
;;

let to_json t =
  `Object
    [ ( "explanation"
      , Option.value_map t.explanation ~default:`Null ~f:(fun s -> `String s) )
    ; ( "steps"
      , `Array
          (List.map t.steps ~f:(fun (s : Step.t) ->
             `Object
               [ "step", `String s.step
               ; "status", `String (Status.to_string s.status)
               ])) )
    ]
;;

let latest messages =
  let ok_results =
    List.filter_map messages ~f:(function
      | Message.Tool_result r when not r.is_error -> Some r.tool_call_id
      | _ -> None)
    |> String.Set.of_list
  in
  List.fold messages ~init:None ~f:(fun acc message ->
    match (message : Message.t) with
    | Assistant a ->
      List.fold (Message.Assistant.tool_calls a) ~init:acc ~f:(fun acc call ->
        if String.equal call.name tool_name && Set.mem ok_results call.id
        then (
          match
            Or_error.bind
              (Content.Tool_call.parse_arguments call)
              ~f:of_arguments
          with
          | Ok plan -> plan
          | Error _ -> acc)
        else acc)
    | User _ | Tool_result _ -> acc)
;;

let spec =
  { Tool_spec.name = tool_name
  ; parallel_safe = false
  ; on_host = false
  ; freeform = None
  ; destructive = Never
  ; description =
      "Updates the task plan, which the user sees. Use it for work with \
       several steps: give every step (short, a few words to a sentence) with \
       its status, and update it as you go, marking steps completed and the \
       next one in_progress. Several steps can be in_progress at once when \
       they run in parallel. Send the whole plan each time; an empty plan \
       clears it. Skip it for simple tasks."
  ; parameters =
      Tool_args.schema
        ~required:[ "plan" ]
        [ ( "explanation"
          , `String
          , "Optional explanation for this plan update (e.g. why it changed)" )
        ; ( "plan"
          , `Array
              (`Object
                  [ "type", `String "object"
                  ; ( "properties"
                    , `Object
                        [ ( "step"
                          , `Object
                              [ "type", `String "string"
                              ; "description", `String "Task step text"
                              ] )
                        ; ( "status"
                          , `Object
                              [ "type", `String "string"
                              ; ( "enum"
                                , `Array
                                    (List.map Status.all ~f:(fun s ->
                                       `String (Status.to_string s))) )
                              ; "description", `String "Step status"
                              ] )
                        ] )
                  ; "required", `Array [ `String "step"; `String "status" ]
                  ; "additionalProperties", `False
                  ])
          , "The list of steps" )
        ]
  }
;;

let run (_ : Tool.Context.t) args =
  match of_arguments args with
  | Error e -> Tool.Result.error (Error.to_string_hum e)
  | Ok None -> Tool.Result.ok "Plan cleared"
  | Ok (Some plan) ->
    Tool.Result.ok
      (sprintf
         "Plan updated (%d of %d steps completed)"
         (List.count plan.steps ~f:(fun s -> Status.equal s.status Completed))
         (List.length plan.steps))
;;

let tool = { Tool.spec; run }
