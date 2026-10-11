open! Core
open! Import

let background_of (context : Tool.Context.t) =
  match context.background with
  | Some background -> background
  | None -> raise (Tool_args.Invalid "background jobs are not available here")
;;

let ok_or_invalid = function
  | Ok x -> x
  | Error e -> raise (Tool_args.Invalid (Error.to_string_hum e))
;;

let spec ~name ~description parameters =
  { Tool_spec.name
  ; parallel_safe = true
  ; on_host = false
  ; freeform = None
  ; destructive = false
  ; description
  ; parameters
  }
;;

let now (context : Tool.Context.t) = Eio.Time.now (Eio.Stdenv.clock context.env)

let status_tool =
  let spec =
    spec
      ~name:"job_status"
      ~description:
        "List background shell jobs: id, state, elapsed time, command, output \
         size and last output line."
      (Tool_args.schema [])
  in
  let run context _args =
    Tool.Result.ok
      (Background_tasks.status_text (background_of context) ~kind:Job)
  in
  { Tool.spec; run }
;;

let output_text task ~now ~lines ~offset =
  let all = Output_tail.lines (Background_tasks.Task.output task) in
  let total = List.length all in
  let stop = Int.max 0 (total - offset) in
  let start = Int.max 0 (stop - lines) in
  let shown = List.sub all ~pos:start ~len:(stop - start) in
  let state =
    match Background_tasks.Task.outcome task with
    | None -> "running"
    | Some outcome -> outcome.status
  in
  let elapsed =
    Option.value (Background_tasks.Task.finished_at task) ~default:now
    -. Background_tasks.Task.started_at task
  in
  let header =
    sprintf
      "[job %s %s after %.0fs, %d bytes; %s] %s"
      (Background_tasks.Task.id task)
      state
      elapsed
      (Output_tail.total_bytes (Background_tasks.Task.output task))
      (if total = 0
       then "no output"
       else if start = stop
       then sprintf "no lines in range; %d retained" total
       else sprintf "lines %d-%d of %d" (start + 1) stop total)
      (Background_tasks.short_task (Background_tasks.Task.label task))
  in
  let body = Truncate.tail (String.concat ~sep:"\n" shown) in
  if String.is_empty body.text then header else header ^ "\n" ^ body.text
;;

let output_tool =
  let spec =
    spec
      ~name:"job_output"
      ~description:
        "Show recent output of a background job without waiting for it (its \
         exit report is still delivered as usual)."
      (Tool_args.schema
         ~required:[ "id" ]
         [ "id", `String, "Job id"
         ; "lines", `Integer, "How many lines (default 50)"
         ; ( "offset"
           , `Integer
           , "Skip this many of the most recent lines, to page back (default 0)"
           )
         ])
  in
  let run (context : Tool.Context.t) args =
    let id = Tool_args.string args "id" in
    let lines = Option.value (Tool_args.int_opt args "lines") ~default:50 in
    let offset = Option.value (Tool_args.int_opt args "offset") ~default:0 in
    let task =
      ok_or_invalid (Background_tasks.find (background_of context) ~kind:Job id)
    in
    Tool.Result.ok
      (output_text
         task
         ~now:(now context)
         ~lines:(Int.max 0 lines)
         ~offset:(Int.max 0 offset))
  in
  { Tool.spec; run }
;;

let wait_tool =
  let spec =
    spec
      ~name:"job_wait"
      ~description:
        "Block until background jobs exit and return their reports (they are \
         then not delivered again). By default waits for all running or \
         undelivered jobs; pass ids to pick some, all=false to return as soon \
         as one exits, timeout to give up after some seconds."
      (Tool_args.schema
         [ ( "ids"
           , `Array (`Object [ "type", `String "string" ])
           , "Job ids (default: every running or undelivered one)" )
         ; ( "all"
           , `Boolean
           , "Wait for all of them (default) or only the first to exit" )
         ; "timeout", `Integer, "Give up after this many seconds"
         ])
  in
  let run (context : Tool.Context.t) args =
    let background = background_of context in
    let ids = Tool_args.string_list_opt args "ids" in
    let all = Option.value (Tool_args.bool_opt args "all") ~default:true in
    let timeout =
      Option.map (Tool_args.int_opt args "timeout") ~f:Float.of_int
    in
    let { Background_tasks.Wait_result.finished; running; timed_out } =
      ok_or_invalid
        (Background_tasks.wait
           background
           ~kind:Job
           ~ids
           ~all
           ~timeout
           ~cancel:context.cancel)
    in
    let reports = List.map finished ~f:Background_tasks.Task.report in
    let still =
      match running with
      | [] -> []
      | running ->
        [ sprintf
            "%sstill running: %s"
            (if timed_out then "timed out; " else "")
            (String.concat
               ~sep:", "
               (List.map running ~f:(fun task ->
                  sprintf
                    "%s (%s)"
                    (Background_tasks.Task.id task)
                    (Background_tasks.short_task
                       (Background_tasks.Task.label task)))))
        ]
    in
    match reports @ still with
    | [] -> Tool.Result.ok "no jobs to wait for"
    | parts -> Tool.Result.ok (String.concat ~sep:"\n\n" parts)
  in
  { Tool.spec; run }
;;

let kill_tool =
  let spec =
    spec
      ~name:"job_kill"
      ~description:
        "Kill a background job (its whole process group, on the host it runs \
         on) and return its final output tail."
      (Tool_args.schema ~required:[ "id" ] [ "id", `String, "Job id" ])
  in
  let run (context : Tool.Context.t) args =
    let id = Tool_args.string args "id" in
    let task =
      ok_or_invalid
        (Background_tasks.cancel_and_wait
           (background_of context)
           ~kind:Job
           id
           ~cancel:context.cancel)
    in
    Tool.Result.ok (Background_tasks.Task.report task)
  in
  { Tool.spec; run }
;;

let default_wait_ms = 1000
let max_wait_ms = 30_000

(* Output settles when nothing new arrived for this long. *)
let quiet_ms = 300

(* What the job printed after [offset] bytes: until it exits, [wait_ms]
   pass, or the output that started arriving goes quiet. *)
let collect_output (context : Tool.Context.t) task ~offset ~wait_ms =
  let clock = Eio.Stdenv.clock context.env in
  let start = Eio.Time.now clock in
  let output = Background_tasks.Task.output task in
  let rec go ~last_total ~last_change =
    let now = Eio.Time.now clock in
    let total = Output_tail.total_bytes output in
    let last_change = if total <> last_total then now else last_change in
    let elapsed_ms = (now -. start) *. 1000. in
    if
      (not (Background_tasks.Task.running task))
      || Float.(elapsed_ms >= of_int wait_ms)
      || (total > offset
          && Float.((now -. last_change) *. 1000. >= of_int quiet_ms))
      || Cancellation.is_cancelled context.cancel
    then ()
    else (
      Eio.Time.sleep clock 0.02;
      go ~last_total:total ~last_change)
  in
  go ~last_total:offset ~last_change:start;
  Output_tail.since output offset
;;

let input_tool =
  let spec =
    { (spec
         ~name:"job_input"
         ~description:
           "Write text to a running background job's stdin (a pipe, not a \
            terminal), optionally closing it afterwards, to drive interactive \
            programs (REPLs, prompts). Include the newline a line needs. \
            Returns the output the job printed in response: whatever arrives \
            until it goes quiet, the job exits or wait_ms pass. Programs that \
            buffer output when stdin is not a terminal may need a flag \
            (python3 -u -i, stdbuf -oL)."
         (Tool_args.schema
            ~required:[ "id"; "input" ]
            [ "id", `String, "Job id"
            ; "input", `String, "Text to write (may be empty to only close)"
            ; ( "close"
              , `Boolean
              , "Close stdin after writing (end of input), default false" )
            ; ( "wait_ms"
              , `Integer
              , sprintf
                  "How long to wait for output, in milliseconds (default %d, \
                   at most %d)"
                  default_wait_ms
                  max_wait_ms )
            ]))
      with
      parallel_safe = false
    ; destructive = true (* the job may run what it reads *)
    }
  in
  let run (context : Tool.Context.t) args =
    let id = Tool_args.string args "id" in
    let input = Tool_args.string args "input" in
    let close = Option.value (Tool_args.bool_opt args "close") ~default:false in
    let wait_ms =
      Int.clamp_exn
        (Option.value
           (Tool_args.int_opt args "wait_ms")
           ~default:default_wait_ms)
        ~min:0
        ~max:max_wait_ms
    in
    let task =
      ok_or_invalid (Background_tasks.find (background_of context) ~kind:Job id)
    in
    let refused reason =
      Tool.Result.error (sprintf "cannot write to job %s: %s" id reason)
    in
    match
      Background_tasks.Task.input task, Background_tasks.Task.outcome task
    with
    | _, Some outcome ->
      refused
        (sprintf
           "it has already exited (%s); start it again with bash and background"
           outcome.status)
    | None, None -> refused "it was started without a stdin pipe"
    | Some feed, None ->
      let offset =
        Output_tail.total_bytes (Background_tasks.Task.output task)
      in
      (match Stdin_feed.write feed input ~close with
       | Error e -> refused (Error.to_string_hum e)
       | Ok () ->
         let output = collect_output context task ~offset ~wait_ms in
         let state =
           match Background_tasks.Task.outcome task with
           | None -> ""
           | Some outcome -> sprintf "; it then %s" outcome.status
         in
         let header =
           sprintf
             "sent %d byte%s to job %s%s%s"
             (String.length input)
             (if String.length input = 1 then "" else "s")
             id
             (if close then " and closed its stdin" else "")
             state
         in
         let body =
           if String.is_empty output
           then "(no output yet; job_output shows what comes later)"
           else (Truncate.tail (Utf8.sanitize output)).text
         in
         Tool.Result.ok (header ^ "\n" ^ body))
  in
  { Tool.spec; run }
;;

let tools = [ status_tool; output_tool; wait_tool; input_tool; kill_tool ]
