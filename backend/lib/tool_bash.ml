open! Core
open! Import

let default_timeout = Time_ns.Span.of_min 10.

(* "No timeout" for background jobs, as a number every host understands. *)
let job_timeout_seconds = 30 * 24 * 3600

let description =
  "Run a shell command with bash in the working directory. Returns combined \
   stdout and stderr (interleaved) and the exit code if non-zero. Long output \
   is truncated to the last part. Do not use for reading or editing files; use \
   the dedicated tools."
;;

let command_parameters =
  [ "command", `String, "The command to run"
  ; ( "timeout"
    , `Integer
    , "Timeout in seconds (default 600, none in the background). The command \
       is killed when it expires." )
  ]
;;

let spec_with parameters =
  { Tool_spec.name = "bash"
  ; parallel_safe = false
  ; on_host = true
  ; destructive = true
  ; description
  ; parameters = Tool_args.schema ~required:[ "command" ] parameters
  }
;;

let spec =
  spec_with
    (command_parameters
     @ [ ( "background"
         , `Boolean
         , "Start it as a background job and return at once with the job id. \
            You are notified with the exit status and output tail when it \
            exits; job_output, job_wait and job_kill act on it meanwhile. Use \
            for anything that may take over a minute or keeps running (builds, \
            test suites, servers, watchers)." )
       ])
;;

let run_foreground (context : Tool.Context.t) args =
  let command = Tool_args.string args "command" in
  let timeout =
    match Tool_args.int_opt args "timeout" with
    | Some seconds -> Time_ns.Span.of_int_sec seconds
    | None -> default_timeout
  in
  let output = Buffer.create 1024 in
  let pending = ref "" in
  let on_data s =
    Buffer.add_string output s;
    let complete, rest = Utf8.split_incomplete_suffix (!pending ^ s) in
    pending := rest;
    if not (String.is_empty complete)
    then context.on_output (Utf8.sanitize complete)
  in
  let exit =
    Process.run
      ~env:context.env
      ~cwd:context.cwd
      ~timeout
      ~cancel:context.cancel
      ~on_stdout:on_data
      ~on_stderr:on_data
      ~prog:"bash"
      ~args:[ "-c"; command ]
      ()
  in
  if not (String.is_empty !pending)
  then context.on_output (Utf8.sanitize !pending);
  let truncated = Truncate.tail (Buffer.contents output) in
  let text =
    if truncated.truncated
    then
      sprintf
        "[output truncated: showing the last part of %d lines / %d bytes]\n%s"
        truncated.total_lines
        truncated.total_bytes
        truncated.text
    else truncated.text
  in
  let with_suffix suffix =
    if String.is_empty text || String.is_suffix text ~suffix:"\n"
    then text ^ suffix
    else text ^ "\n" ^ suffix
  in
  match exit with
  | Exited 0 -> Tool.Result.ok text
  | Exited n -> Tool.Result.error (with_suffix (sprintf "[exit code %d]" n))
  | Signaled s ->
    Tool.Result.error
      (with_suffix (sprintf "[killed by %s]" (Signal.to_string s)))
  | Timed_out ->
    let seconds = Time_ns.Span.to_sec timeout |> Float.iround_nearest_exn in
    Tool.Result.error (with_suffix (sprintf "[timed out after %ds]" seconds))
  | Cancelled -> Tool.Result.error (with_suffix "[cancelled]")
;;

let foreground_tool =
  { Tool.spec = spec_with command_parameters; run = run_foreground }
;;

let fields = function
  | `Object fields -> fields
  | _ -> []
;;

let starts_job (context : Tool.Context.t) args =
  context.depth = 0
  && Option.is_some context.background
  &&
  match List.Assoc.find (fields args) ~equal:String.equal "background" with
  | Some `True -> true
  | _ -> false
;;

let foreground_arguments args =
  let fields =
    List.filter (fields args) ~f:(fun (name, _) ->
      not (String.equal name "background"))
  in
  if List.Assoc.mem fields ~equal:String.equal "timeout"
  then `Object fields
  else
    `Object (fields @ [ "timeout", `Number (Int.to_string job_timeout_seconds) ])
;;

let report_lines = 40

(* The foreground result ends with a [[...]] trailer unless the command
   exited 0; anything else is a failure to run it at all. *)
let job_status (result : Tool_result.t) =
  if not result.is_error
  then "exited 0"
  else (
    let last =
      String.split_lines result.text
      |> List.rev
      |> List.find ~f:(fun l -> not (String.is_empty (String.strip l)))
      |> Option.value ~default:""
      |> String.strip
    in
    let inner =
      String.chop_prefix last ~prefix:"["
      |> Option.bind ~f:(String.chop_suffix ~suffix:"]")
    in
    match inner with
    | Some "cancelled" -> "killed"
    | Some inner ->
      (match String.chop_prefix inner ~prefix:"exit code " with
       | Some code -> "exited " ^ code
       | None ->
         if
           String.is_prefix inner ~prefix:"killed by "
           || String.is_prefix inner ~prefix:"timed out after "
         then inner
         else "failed: " ^ inner)
    | None -> "failed: " ^ Background_tasks.short_task last)
;;

let job_outcome (result : Tool_result.t) ~output =
  let lines =
    match Output_tail.lines output with
    | [] when result.is_error ->
      (* No streamed output: keep what the result has, minus its trailer. *)
      List.drop_last (String.split_lines result.text)
      |> Option.value ~default:[]
    | [] -> String.split_lines result.text
    | lines -> lines
  in
  let shown =
    Truncate.tail
      ~max_lines:report_lines
      ~max_bytes:16_384
      (String.concat ~sep:"\n" lines)
  in
  let body =
    if shown.truncated
    then
      sprintf
        "[last %d of %d lines; job_output shows more]\n%s"
        (Truncate.count_lines shown.text)
        (List.length lines)
        shown.text
    else shown.text
  in
  { Background_tasks.Outcome.status = job_status result
  ; body
  ; is_error = result.is_error
  }
;;

let start_job background ~command ~run =
  Background_tasks.spawn
    background
    ~kind:Job
    ~label:command
    ~run:(fun ~id ~cancel ~emit:_ ~on_output ->
      let tail = Output_tail.create ~capacity:65_536 () in
      let result =
        run ~id ~cancel ~on_output:(fun chunk ->
          Output_tail.add tail chunk;
          on_output chunk)
      in
      job_outcome result ~output:tail)
;;

let started_message ~id ~command =
  sprintf
    "started job %s: %s; its output is being recorded; you will be notified \
     when it exits. Use job_output to peek, job_wait to block, job_kill to \
     stop it."
    id
    (Background_tasks.short_task command)
;;

let run (context : Tool.Context.t) args =
  match context.background with
  | Some background when starts_job context args ->
    let command = Tool_args.string args "command" in
    ignore (Tool_args.int_opt args "timeout" : int option);
    let arguments = foreground_arguments args in
    let id =
      start_job background ~command ~run:(fun ~id ~cancel ~on_output ->
        Tool.execute_via
          { context with cancel; on_output; call_id = id; background = None }
          foreground_tool
          arguments)
    in
    Tool.Result.ok (started_message ~id ~command)
  | _ -> run_foreground context args
;;

let tool = { Tool.spec; run }
