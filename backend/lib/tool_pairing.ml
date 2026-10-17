open! Core
open! Import

let interrupted (call : Content.Tool_call.t) =
  { Message.Tool_result.tool_call_id = call.id
  ; tool_name = call.name
  ; text =
      "[interrupted: prigh stopped before this tool call returned (e.g. the \
       backend restarted), so whether it ran, and what it did, is unknown: \
       check before relying on it]"
  ; is_error = true
  ; images = []
  }
;;

let rec repair = function
  | [] -> []
  | Message.Assistant a :: rest ->
    let calls = Message.Assistant.tool_calls a in
    let turn, later =
      List.split_while rest ~f:(function
        | Message.Assistant _ -> false
        | User _ | Tool_result _ -> true)
    in
    let results =
      List.filter_map turn ~f:(function
        | Message.Tool_result r -> Some r
        | User _ | Assistant _ -> None)
    in
    let answers =
      List.map calls ~f:(fun (call : Content.Tool_call.t) ->
        match
          List.find results ~f:(fun r -> String.equal r.tool_call_id call.id)
        with
        | Some r -> Message.Tool_result r
        | None -> Message.Tool_result (interrupted call))
    in
    let others =
      List.filter turn ~f:(function
        | Message.Tool_result _ -> false
        | User _ | Assistant _ -> true)
    in
    (Message.Assistant a :: answers) @ others @ repair later
  | Message.Tool_result _ :: rest -> repair rest
  | (Message.User _ as m) :: rest -> m :: repair rest
;;

let unanswered messages =
  let results, before =
    List.split_while (List.rev messages) ~f:(function
      | Message.Tool_result _ -> true
      | User _ | Assistant _ -> false)
  in
  match before with
  | Message.Assistant a :: _ ->
    List.filter (Message.Assistant.tool_calls a) ~f:(fun call ->
      not
        (List.exists results ~f:(function
           | Message.Tool_result r -> String.equal r.tool_call_id call.id
           | User _ | Assistant _ -> false)))
  | _ -> []
;;
