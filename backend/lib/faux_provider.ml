open! Core
open! Import

let json_string json name =
  match Json.member name json with
  | None | Some `Null -> Ok None
  | Some (`String s) -> Ok (Some s)
  | Some _ -> Or_error.errorf "%s must be a string" name
;;

let required_string json name =
  Or_error.bind (json_string json name) ~f:(function
    | Some s -> Ok s
    | None -> Or_error.errorf "%s is required" name)
;;

let json_int json name =
  match Json.member name json with
  | None | Some `Null -> Ok None
  | Some json ->
    (match Json.int json with
     | Some i -> Ok (Some i)
     | None -> Or_error.errorf "%s must be an integer" name)
;;

let json_usage json =
  let field name =
    match Json.member name json with
    | None | Some `Null -> Ok 0
    | Some json ->
      (match Json.int json with
       | Some i -> Ok i
       | None -> Or_error.errorf "usage.%s must be an integer" name)
  in
  Or_error.bind (field "input") ~f:(fun input ->
    Or_error.bind (field "output") ~f:(fun output ->
      Or_error.map (field "cache_read") ~f:(fun cache_read ->
        { Usage.input; output; cache_read })))
;;

let json_usage_opt json =
  match Json.member "usage" json with
  | None | Some `Null -> Ok Usage.zero
  | Some usage -> json_usage usage
;;

let json_tool_calls json =
  match Json.member "tool_calls" json with
  | None | Some `Null -> Ok []
  | Some (`Array items) ->
    Or_error.all
      (List.mapi items ~f:(fun index item ->
         Or_error.bind (required_string item "id") ~f:(fun id ->
           Or_error.map (required_string item "name") ~f:(fun name ->
             let arguments =
               match Json.member "arguments" item with
               | Some (`String s) -> s
               | Some json -> Json.to_string json
               | None -> "{}"
             in
             index, id, name, arguments))))
  | Some _ -> Or_error.error_string "tool_calls must be an array"
;;

let json_stop_reason json ~has_tool_calls =
  if has_tool_calls
  then Ok Stop_reason.Tool_use
  else (
    match Json.member "stop_reason" json with
    | None | Some `Null -> Ok Stop_reason.End_turn
    | Some (`String "end_turn") -> Ok End_turn
    | Some (`String "length") -> Ok Length
    | Some (`String "error") ->
      Or_error.map (required_string json "error") ~f:(fun error ->
        Stop_reason.Error error)
    | Some (`String s) -> Or_error.errorf "unknown stop_reason %S" s
    | Some _ -> Or_error.error_string "stop_reason must be a string")
;;

let split_chunks text chunks =
  if chunks <= 1
  then [ text ]
  else (
    let n = String.length text in
    let base = n / chunks in
    let extra = n mod chunks in
    let rec go i pos acc =
      if i = chunks
      then List.rev acc
      else (
        let len = base + if i < extra then 1 else 0 in
        go (i + 1) (pos + len) (String.sub text ~pos ~len :: acc))
    in
    go 0 0 [])
;;

module Reply = struct
  type t =
    { events : Assistant_event.t list
    ; stop_reason : Stop_reason.t
    ; usage : Usage.t
    }
  [@@deriving sexp_of]

  let text ?(stop_reason = Stop_reason.End_turn) s =
    { events = [ Text_delta s ]
    ; stop_reason
    ; usage = { input = 10; output = 5; cache_read = 0 }
    }
  ;;

  let tool_calls ?text calls =
    let events =
      Option.value_map text ~default:[] ~f:(fun t ->
        [ Assistant_event.Text_delta t ])
      @ List.concat_mapi calls ~f:(fun index (id, name, arguments) ->
        [ Assistant_event.Tool_call_start { index; id; name }
        ; Tool_call_delta { index; arguments }
        ])
    in
    { events
    ; stop_reason = Tool_use
    ; usage = { input = 20; output = 8; cache_read = 5 }
    }
  ;;

  let tool_call ?text ~id ~name ~arguments () =
    tool_calls ?text [ id, name, arguments ]
  ;;

  let of_json json =
    Or_error.bind (json_string json "text") ~f:(fun text ->
      Or_error.bind (json_string json "thinking") ~f:(fun thinking ->
        Or_error.bind (json_int json "chunks") ~f:(fun chunks ->
          Or_error.bind (json_tool_calls json) ~f:(fun calls ->
            Or_error.bind
              (json_stop_reason
                 json
                 ~has_tool_calls:(not (List.is_empty calls)))
              ~f:(fun stop_reason ->
                Or_error.map (json_usage_opt json) ~f:(fun usage ->
                  let chunks = Option.value chunks ~default:1 in
                  let thinking_events =
                    Option.value_map thinking ~default:[] ~f:(fun s ->
                      [ Assistant_event.Thinking_delta s ])
                  in
                  let text_events =
                    Option.value_map text ~default:[] ~f:(fun s ->
                      List.map (split_chunks s chunks) ~f:(fun c ->
                        Assistant_event.Text_delta c))
                  in
                  let call_events =
                    List.concat_map
                      calls
                      ~f:(fun (index, id, name, arguments) ->
                        [ Assistant_event.Tool_call_start { index; id; name }
                        ; Assistant_event.Tool_call_delta { index; arguments }
                        ])
                  in
                  { events = thinking_events @ text_events @ call_events
                  ; stop_reason
                  ; usage
                  }))))))
  ;;
end

let of_script_file path =
  Or_error.try_with (fun () -> In_channel.read_all path)
  |> Or_error.bind ~f:(fun contents ->
    Or_error.bind (Json.parse contents) ~f:(function
      | `Array items -> Or_error.all (List.map items ~f:Reply.of_json)
      | _ -> Or_error.error_string "faux script must be a JSON array of replies"))
;;

let create
      ?(on_request = ignore)
      ?(delay_between_events = Fiber.yield)
      ?(loop = false)
      replies
  =
  let original = replies in
  let remaining = ref replies in
  let next_reply () =
    (match !remaining with
     | [] -> if loop then remaining := original
     | _ -> ());
    match !remaining with
    | [] -> None
    | (reply : Reply.t) :: rest ->
      remaining := rest;
      Some reply
  in
  let stream (request : Provider.Request.t) ~cancel ~on_event =
    on_request request;
    let builder = Assistant_builder.create ~model:request.model.id in
    match next_reply () with
    | None ->
      Assistant_builder.finish
        builder
        ~stop_reason:(Error "faux provider: no scripted reply")
        ~usage:Usage.zero
    | Some reply ->
      let aborted = ref false in
      List.iter reply.events ~f:(fun event ->
        if not !aborted
        then (
          delay_between_events ();
          if Cancellation.is_cancelled cancel
          then aborted := true
          else (
            Assistant_builder.apply builder event;
            on_event event)));
      let stop_reason =
        if !aborted then Stop_reason.Aborted else reply.stop_reason
      in
      Assistant_builder.finish builder ~stop_reason ~usage:reply.usage
  in
  { Provider.name = "faux"; stream }
;;
