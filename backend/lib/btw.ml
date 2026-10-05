open! Core
open! Import

let still_running (call : Content.Tool_call.t) =
  Message.Tool_result
    { tool_call_id = call.id
    ; tool_name = call.name
    ; text = "[still running: no result yet]"
    ; is_error = false
    }
;;

let sanitize messages =
  let rec go acc = function
    | [] -> List.rev acc
    | Message.Tool_result _ :: rest -> go acc rest
    | (User _ as m) :: rest -> go (m :: acc) rest
    | Assistant a :: rest when List.is_empty a.content -> go acc rest
    | (Assistant a as m) :: rest ->
      let results, rest =
        List.split_while rest ~f:(function
          | Message.Tool_result _ -> true
          | User _ | Assistant _ -> false)
      in
      let paired =
        List.map (Message.Assistant.tool_calls a) ~f:(fun call ->
          List.find results ~f:(function
            | Message.Tool_result r -> String.equal r.tool_call_id call.id
            | User _ | Assistant _ -> false)
          |> Option.value ~default:(still_running call))
      in
      go (List.rev_append paired (m :: acc)) rest
  in
  go [] messages
;;

let question_message question =
  Message.user
    ("<btw>The user asks a side question (/btw) while you may be in the middle \
      of a task. Answer it briefly from the conversation so far. You cannot \
      use tools in this reply, and neither the question nor your answer will \
      be added to the conversation.</btw>\n\n"
     ^ question)
;;

let request ~model ~system ~messages ~question =
  { Provider.Request.model
  ; system = Some system
  ; messages = sanitize messages @ [ question_message question ]
  ; tools = []
  ; thinking = Off
  ; max_tokens = None
  }
;;
