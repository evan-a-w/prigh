open! Core
open! Prigh
module Reply = Faux_provider.Reply

(* Each request takes [connect] seconds to its first byte and [per_delta]
   per streamed delta. *)
let timed ?(connect = 0.5) ?(per_delta = 0.1) clock (provider : Provider.t) =
  { provider with
    stream =
      (fun request ~cancel ~on_event ->
        Virtual_clock.sleep clock connect;
        provider.stream request ~cancel ~on_event:(fun delta ->
          Virtual_clock.sleep clock per_delta;
          on_event delta))
  }
;;

let work_tool clock =
  { Tool.spec =
      { Tool_spec.name = "work"
      ; description = "works for SECONDS"
      ; parameters = Tool_args.schema [ "seconds", `Integer, "how long" ]
      ; parallel_safe = true
      ; destructive = Never
      ; on_host = false
      ; freeform = None
      }
  ; run =
      (fun context args ->
        let seconds =
          Option.value (Tool_args.int_opt args "seconds") ~default:1
        in
        match
          Cancellation.protect context.cancel ~f:(fun () ->
            Virtual_clock.sleep clock (Float.of_int seconds))
        with
        | Some () -> Tool.Result.ok (sprintf "worked %ds" seconds)
        | None -> Tool.Result.error "[cancelled]")
  }
;;

let work ?(id = "w1") seconds =
  Reply.tool_call ~id ~name:"work" ~arguments:(sprintf {|{"seconds":%d}|} seconds) ()
;;

let shown =
  [ "stop"
  ; "error"
  ; "result_bytes"
  ; "where"
  ; "attempt"
  ; "delay"
  ; "tokens_before"
  ; "tokens_after"
  ; "tokens_saved"
  ; "background"
  ; "forked"
  ; "continued"
  ; "outcome"
  ; "kind"
  ]
;;

let print_outline ?(attrs = shown) spans =
  print_string
    (Trace_report.outline ~attrs:(List.mem attrs ~equal:String.equal) spans)
;;

let print_tracks spans =
  List.map spans ~f:(fun (s : Trace.Span.t) -> s.track)
  |> List.sort_and_group ~compare:String.compare
  |> List.iter ~f:(fun group ->
    printf "track %S: %d spans\n" (List.hd_exn group) (List.length group))
;;

