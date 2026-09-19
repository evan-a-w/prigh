open! Core
open! Import

let default_timeout = Time_ns.Span.of_min 10.

let spec =
  { Tool_spec.name = "bash"
  ; parallel_safe = false
  ; description =
      "Run a shell command with bash in the working directory. Returns \
       combined stdout and stderr (interleaved) and the exit code if non-zero. \
       Long output is truncated to the last part. Do not use for reading or \
       editing files; use the dedicated tools."
  ; parameters =
      Tool_args.schema
        ~required:[ "command" ]
        [ "command", `String, "The command to run"
        ; ( "timeout"
          , `Integer
          , "Timeout in seconds (default 600). The command is killed when it \
             expires." )
        ]
  }
;;

let run (context : Tool.Context.t) args =
  let command = Tool_args.string args "command" in
  let timeout =
    match Tool_args.int_opt args "timeout" with
    | Some seconds -> Time_ns.Span.of_int_sec seconds
    | None -> default_timeout
  in
  let output = Buffer.create 1024 in
  let on_data s =
    Buffer.add_string output s;
    context.on_output s
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
    Tool.Result.error
      (with_suffix
         (sprintf "[timed out after %s]" (Time_ns.Span.to_string_hum timeout)))
  | Cancelled -> Tool.Result.error (with_suffix "[cancelled]")
;;

let tool = { Tool.spec; run }
