open! Core
open! Import

let default_timeout = Time_ns.Span.of_min 10.

(* "No timeout" for background jobs, as a number every host understands. *)
let job_timeout_seconds = 30 * 24 * 3600

let description =
  "Run a shell command with bash in the working directory. Returns combined \
   stdout and stderr (interleaved) and the exit code if non-zero. Long output \
   keeps its beginning and end, and the full output is saved to a file you can \
   read. Do not use for reading or editing files; use the dedicated tools."
;;

let auto_description =
  description
  ^ " A command still running after a while (about 30 seconds by default) is \
     moved to the background as a job, without being restarted: the call \
     returns with its job id and output so far, and its report arrives when it \
     exits."
;;

let command_parameters =
  [ "command", `String, "The command to run"
  ; ( "timeout"
    , `Integer
    , "Timeout in seconds (default 600, none in the background). The command \
       is killed when it expires." )
  ]
;;

let spec_with ?(description = description) parameters =
  { Tool_spec.name = "bash"
  ; parallel_safe = false
  ; on_host = true
  ; freeform = None
  ; destructive = Always
  ; description
  ; parameters = Tool_args.schema ~required:[ "command" ] parameters
  }
;;

let stdin_parameter =
  ( "stdin"
  , `Boolean
  , "With background: keep its stdin open (a pipe, not a terminal) for input \
     you send with job_input. Otherwise it reads end of input at once, like a \
     foreground command." )
;;

let spec =
  spec_with
    ~description:auto_description
    (command_parameters
     @ [ ( "background"
         , `Boolean
         , "Start it as a background job and return at once with the job id. \
            You are notified with the exit status and output tail when it \
            exits, even after you end your turn; job_output, job_input and \
            job_kill act on it meanwhile. Use it for anything that may take \
            long or keeps running (builds, test suites, installs, servers, \
            watchers), then carry on with other work or end your turn." )
       ; stdin_parameter
       ])
;;

(* A subagent finishes only once its jobs have exited. *)
let subagent_spec =
  spec_with
    (command_parameters
     @ [ ( "background"
         , `Boolean
         , "Start it as a background job and return at once with the job id, \
            to run commands side by side or keep a server running while you \
            work. Its report arrives when it exits, even after you end your \
            turn; job_output, job_input and job_kill act on it meanwhile. You \
            finish only once your jobs have exited: kill the ones you no \
            longer need." )
       ; stdin_parameter
       ])
;;

let output_retention = Time_float.Span.of_day 7.

(* Outputs can hold secrets: they go in a directory of this user's only,
   which must not be a symlink or someone else's (another user could have
   made it first in a shared [/tmp]). *)
let output_dir ~tmp =
  let uid = Core_unix.getuid () in
  let dir = Filename.concat tmp (sprintf "prigh-output-%d" uid) in
  (try Core_unix.mkdir dir ~perm:0o700 with
   | Core_unix.Unix_error (EEXIST, _, _) -> ());
  match Core_unix.lstat dir with
  | { st_kind = S_DIR; st_uid; st_perm; _ } when st_uid = uid ->
    if st_perm land 0o077 <> 0 then Core_unix.chmod dir ~perm:0o700;
    dir
  | { st_kind = S_DIR; _ } ->
    failwithf "%s belongs to another user: remove it or set TMPDIR" dir ()
  | _ -> failwithf "%s is not a directory: remove it or set TMPDIR" dir ()
;;

(* Saves a command's full output where the model can read or grep it, and
   drops files older than [output_retention]. *)
let save_output_in ~tmp ~call_id output =
  let dir = output_dir ~tmp in
  let now = Time_float.now () in
  Array.iter (Sys_unix.readdir dir) ~f:(fun name ->
    let path = Filename.concat dir name in
    match Core_unix.lstat path with
    | { st_mtime; _ } ->
      if
        Time_float.Span.( > )
          (Time_float.diff
             now
             (Time_float.of_span_since_epoch (Time_float.Span.of_sec st_mtime)))
          output_retention
      then Sys_unix.remove path
    | exception _ -> ());
  let safe =
    String.map call_id ~f:(fun c ->
      if Char.is_alphanum c || Char.equal c '-' || Char.equal c '_'
      then c
      else '_')
  in
  let base =
    sprintf
      "%s-%s"
      (Time_float.to_filename_string now ~zone:Time_float.Zone.utc)
      (if String.is_empty safe then "bash" else safe)
  in
  let rec create n =
    let path =
      Filename.concat
        dir
        (if n = 0 then base ^ ".log" else sprintf "%s-%d.log" base n)
    in
    match
      Core_unix.openfile path ~mode:[ O_WRONLY; O_CREAT; O_EXCL ] ~perm:0o600
    with
    | fd -> path, fd
    | exception Core_unix.Unix_error (EEXIST, _, _) -> create (n + 1)
  in
  let path, fd = create 0 in
  let oc = Core_unix.out_channel_of_descr fd in
  Exn.protect
    ~f:(fun () -> Out_channel.output_string oc output)
    ~finally:(fun () -> Out_channel.close oc);
  path
;;

let save_output (context : Tool.Context.t) output =
  save_output_in ~tmp:Filename.temp_dir_name ~call_id:context.call_id output
;;

module For_testing = struct
  let save_output = save_output_in
end

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
      ?input:context.stdin
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
  let full = Buffer.contents output in
  let truncated = Truncate.head_and_tail full in
  let text =
    if truncated.truncated
    then (
      let saved =
        match save_output context full with
        | path ->
          sprintf
            "the full output is in %s (read it with offset/limit, or grep it)"
            path
        | exception exn ->
          sprintf "saving the full output failed: %s" (Exn.to_string exn)
      in
      sprintf
        "[output truncated: %d lines / %d bytes, showing the beginning and the \
         end; %s]\n\
         %s"
        truncated.total_lines
        truncated.total_bytes
        saved
        truncated.text)
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
  Option.is_some context.background
  &&
  match List.Assoc.find (fields args) ~equal:String.equal "background" with
  | Some `True -> true
  | _ -> false
;;

let auto_background (context : Tool.Context.t) args =
  if context.depth = 0 && not (starts_job context args)
  then Option.bind context.background ~f:Background_tasks.auto_background
  else None
;;

let runs_in_backend context args =
  starts_job context args || Option.is_some (auto_background context args)
;;

let foreground_arguments args =
  let fields =
    List.filter (fields args) ~f:(fun (name, _) ->
      not (List.mem [ "background"; "stdin" ] name ~equal:String.equal))
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

let start_job ?(keep_stdin = false) background ~command ~run =
  let stdin = Stdin_feed.create () in
  if not keep_stdin
  then (
    ignore (Stdin_feed.write stdin "" ~close:true : unit Or_error.t);
    Stdin_feed.refuse
      stdin
      "it was started without stdin: true, so its stdin is closed; start it \
       again with bash, background: true and stdin: true to send it input");
  Background_tasks.spawn
    background
    ~kind:Job
    ~label:command
    ~input:stdin
    ~run:(fun ~id ~cancel ~emit:_ ~on_output ->
      let tail = Output_tail.create ~capacity:65_536 () in
      let result =
        Exn.protect
          ~finally:(fun () -> Stdin_feed.finish stdin)
          ~f:(fun () ->
            run ~id ~cancel ~stdin ~on_output:(fun chunk ->
              Output_tail.add tail chunk;
              on_output chunk))
      in
      job_outcome result ~output:tail)
;;

let started_message ?(subagent = false) ~id ~command () =
  if subagent
  then
    sprintf
      "started job %s: %s; its output is being recorded; you will be notified \
       when it exits, even after you end your turn (you finish only once your \
       jobs have exited). Use job_output to peek, job_kill to stop it."
      id
      (Background_tasks.short_task command)
  else
    sprintf
      "started job %s: %s; its output is being recorded; you will be notified \
       when it exits. Use job_output to peek, job_kill to stop it."
      id
      (Background_tasks.short_task command)
;;

let moved_prefix = "moved to the background as job "
let moved_tail_lines = 10

let moved_message ~id ~after ~output =
  let lines = Output_tail.lines output in
  let shown = List.drop lines (List.length lines - moved_tail_lines) in
  let so_far =
    match shown with
    | [] -> "No output so far."
    | shown ->
      sprintf
        "Output so far%s:\n%s"
        (if List.length shown < List.length lines
         then
           sprintf
             " (last %d of %d lines)"
             (List.length shown)
             (List.length lines)
         else "")
        (Truncate.tail
           ~max_lines:moved_tail_lines
           ~max_bytes:4096
           (String.concat ~sep:"\n" shown))
          .text
  in
  sprintf
    "still running after %s, so it was %s%s (not restarted: it keeps running). \
     %s\n\
     Its report ([job %s exited ...] and its last lines) is delivered to you \
     when it exits: keep working on something else or end your turn. \
     job_output shows more output, job_kill stops it."
    (Background_tasks.seconds_text after)
    moved_prefix
    id
    so_far
    id
;;

let moved_job_id text =
  let open Option.Let_syntax in
  let%bind rest = String.chop_prefix text ~prefix:"still running after " in
  let%bind index = String.substr_index rest ~pattern:moved_prefix in
  let rest = String.drop_prefix rest (index + String.length moved_prefix) in
  let id = String.take_while rest ~f:Char.is_alphanum in
  Option.some_if (not (String.is_empty id)) id
;;

(* Without its own timeout a command is not killed after [default_timeout]
   once it has moved to the background, like a job. *)
let auto_arguments args ~after =
  if
    List.Assoc.mem (fields args) ~equal:String.equal "timeout"
    || Float.( >= ) after (Time_ns.Span.to_sec default_timeout)
  then args
  else
    `Object
      (fields args @ [ "timeout", `Number (Int.to_string job_timeout_seconds) ])
;;

let refused_stdin () =
  let feed = Stdin_feed.create () in
  ignore (Stdin_feed.write feed "" ~close:true : unit Or_error.t);
  Stdin_feed.refuse
    feed
    "it was moved to the background from a foreground call, whose stdin is \
     closed; start it again with bash, background: true and stdin: true to \
     send it input";
  feed
;;

let run_auto (context : Tool.Context.t) background args ~after =
  let command = Tool_args.string args "command" in
  ignore (Tool_args.int_opt args "timeout" : int option);
  let arguments = auto_arguments args ~after in
  match
    Background_tasks.run_foreground
      background
      ~kind:Job
      ~label:command
      ~input:(refused_stdin ())
      ~after
      ~cancel:context.cancel
      ~on_output:context.on_output
      ~run:(fun ~cancel ~on_output ->
        Tool.execute_via
          { context with cancel; on_output; background = None }
          foreground_tool
          arguments)
      ~outcome:(fun result ~output -> job_outcome result ~output)
  with
  | Finished result -> result
  | Moved task ->
    Tool.Result.ok
      (moved_message
         ~id:(Background_tasks.Task.id task)
         ~after
         ~output:(Background_tasks.Task.output task))
;;

let run (context : Tool.Context.t) args =
  match context.background with
  | Some background when Option.is_some (auto_background context args) ->
    run_auto
      context
      background
      args
      ~after:(Option.value_exn (auto_background context args))
  | Some background when starts_job context args ->
    let command = Tool_args.string args "command" in
    ignore (Tool_args.int_opt args "timeout" : int option);
    let arguments = foreground_arguments args in
    let keep_stdin =
      Option.value (Tool_args.bool_opt args "stdin") ~default:false
    in
    let id =
      start_job
        background
        ~keep_stdin
        ~command
        ~run:(fun ~id ~cancel ~stdin ~on_output ->
          Tool.execute_via
            { context with
              cancel
            ; on_output
            ; call_id = id
            ; background = None
            ; stdin = Some stdin
            }
            foreground_tool
            arguments)
    in
    Tool.Result.ok
      (started_message ~subagent:(context.depth > 0) ~id ~command ())
  | _ -> run_foreground context args
;;

let tool = { Tool.spec; run }
let subagent_tool = { Tool.spec = subagent_spec; run }
