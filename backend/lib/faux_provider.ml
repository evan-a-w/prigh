open! Core
open! Import

module Reply = struct
  type t =
    { events : Assistant_event.t list
    ; stop_reason : Stop_reason.t
    ; usage : Usage.t
    }

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
end

let create ?(on_request = ignore) ?(delay_between_events = Fiber.yield) replies =
  let remaining = ref replies in
  let stream (request : Provider.Request.t) ~cancel ~on_event =
    on_request request;
    let builder = Assistant_builder.create ~model:request.model.id in
    match !remaining with
    | [] ->
      Assistant_builder.finish
        builder
        ~stop_reason:(Error "faux provider: no scripted reply")
        ~usage:Usage.zero
    | (reply : Reply.t) :: rest ->
      remaining := rest;
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
