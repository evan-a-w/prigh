open! Core
open! Import

let default_timeout = Time_float.Span.of_sec 60.

let no_tools_note =
  "Do not call any tools, run commands, or read/write files; reply with plain \
   text only."
;;

let system_with_no_tools system =
  let system = String.strip system in
  if String.is_empty system
  then no_tools_note
  else system ^ "\n\n" ^ no_tools_note
;;

let combine_prompt ~system ~prompt =
  let system = String.strip system in
  if String.is_empty system
  then sprintf "%s\n\n%s" no_tools_note prompt
  else
    sprintf
      "Follow these instructions strictly. %s\n\n\
       <instructions>\n\
       %s\n\
       </instructions>\n\n\
       %s"
      no_tools_note
      system
      prompt
;;

let fast_tiers =
  [ [ "flash-lite"; "flash_lite"; "flash lite" ]
  ; [ "flash" ]
  ; [ "haiku" ]
  ; [ "mini" ]
  ; [ "nano"; "lite"; "fast" ]
  ]
;;

let pick_fast_model (models : Model.t list) =
  let real = List.filter models ~f:(Fn.non External_model.is_placeholder) in
  let matches_word (m : Model.t) word =
    let id = String.lowercase m.id in
    let name = String.lowercase m.name in
    String.is_substring id ~substring:word
    || String.is_substring name ~substring:word
  in
  List.find_map fast_tiers ~f:(fun words ->
    List.find_map real ~f:(fun m ->
      Option.some_if (List.exists words ~f:(matches_word m)) m.id))
;;

let with_cancel_timeout ~cancel ~timeout f =
  if Cancellation.is_cancelled cancel
  then Or_error.error_string "cancelled"
  else if Float.(Time_float.Span.to_sec timeout <= 0.)
  then
    Or_error.errorf
      "completion timed out after %s"
      (Time_float.Span.to_string_hum timeout)
  else
    Fiber.first f (fun () ->
      match
        Fiber.first
          (fun () ->
             Cancellation.await cancel;
             `Cancelled)
          (fun () ->
             Eio_unix.sleep (Time_float.Span.to_sec timeout);
             `Timed_out)
      with
      | `Cancelled -> Or_error.error_string "cancelled"
      | `Timed_out ->
        Or_error.errorf
          "completion timed out after %s"
          (Time_float.Span.to_string_hum timeout))
;;

let drive
      ?(timeout = default_timeout)
      ?(on_delta = ignore)
      ~cancel
      ~next_event
      ~interrupt
      ~reply_approval
      ~answer
      ~model
      ()
  =
  if Cancellation.is_cancelled cancel
  then (
    interrupt ();
    Or_error.error_string "cancelled")
  else (
    let buf = Buffer.create 128 in
    let streamed = String.Hash_set.create () in
    let usage = ref Usage.zero in
    let cost_usd = ref 0. in
    let rec loop () =
      match next_event () with
      | Adapter_event.Text_delta { item; text } ->
        Hash_set.add streamed item;
        Buffer.add_string buf text;
        on_delta text;
        loop ()
      | Text_done { item; text } ->
        if not (Hash_set.mem streamed item)
        then (
          Buffer.add_string buf text;
          on_delta text);
        loop ()
      | Approval_request { request_id; _ } ->
        reply_approval
          ~request_id
          (Adapter_event.Approval_reply.Deny
             (Some "tools are disabled for this completion"));
        loop ()
      | User_question { request_id; _ } ->
        answer ~request_id User_question.Reply.Declined;
        loop ()
      | Usage { usage = u; cost_usd = c; _ } ->
        usage := Usage.add !usage u;
        Option.iter c ~f:(fun c -> cost_usd := !cost_usd +. c);
        loop ()
      | Turn_end { stop = Done; _ } ->
        Ok
          { Adapter.Completion.text = Buffer.contents buf
          ; usage = !usage
          ; cost_usd = !cost_usd
          ; model
          }
      | Turn_end { stop = Interrupted; _ } -> Or_error.error_string "cancelled"
      | Turn_end { stop = Failed { message; _ }; _ } ->
        Or_error.error_string message
      | Thread_started _
      | Turn_started _
      | Thinking_delta _
      | Thinking_done _
      | Tool_start _
      | Tool_update _
      | Tool_end _
      | Plan _
      | Turn_diff _
      | Request_withdrawn _
      | Compacted _
      | Retrying _
      | Session_title _
      | Notice _ -> loop ()
    in
    let watcher () =
      let reason =
        Fiber.first
          (fun () ->
             Cancellation.await cancel;
             `Cancelled)
          (fun () ->
             Eio_unix.sleep (Time_float.Span.to_sec timeout);
             `Timed_out)
      in
      interrupt ();
      match reason with
      | `Cancelled -> Or_error.error_string "cancelled"
      | `Timed_out ->
        Or_error.errorf
          "completion timed out after %s"
          (Time_float.Span.to_string_hum timeout)
    in
    Fiber.first loop watcher)
;;

let run
      ~adapter:(module A : Adapter.S)
      ~spawn
      ?login
      ?no_sandbox
      ?(catalog_models = [])
      ?(timeout = default_timeout)
      ?request_timeout
      ?on_delta
      ~cancel
      ~cwd
      ~model
      ~system
      ~prompt
      ()
  =
  if Cancellation.is_cancelled cancel
  then Or_error.error_string "cancelled"
  else (
    let launch_model =
      match model with
      | Some _ as m -> m
      | None -> pick_fast_model catalog_models
    in
    let start : Adapter.Start.t =
      { (Adapter.Start.default ~cwd) with
        model = launch_model
      ; thinking = Off
      ; approval = Ask
      ; plan = false
      ; login
      ; no_sandbox
      }
    in
    let launch = A.launch_complete start in
    let spawned =
      with_cancel_timeout ~cancel ~timeout (fun () -> spawn launch)
    in
    match spawned with
    | Error _ as e -> e
    | Ok transport ->
      Switch.run (fun sw ->
        Exn.protect
          ~finally:(fun () -> Line_transport.stop transport)
          ~f:(fun () ->
            A.complete
              ~sw
              ~timeout
              ?request_timeout
              ?on_delta
              ~cancel
              transport
              start
              { Adapter.Complete_input.system; prompt; model; catalog_models })))
;;
