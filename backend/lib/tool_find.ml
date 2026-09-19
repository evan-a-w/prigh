open! Core
open! Import

let default_limit = 500

let spec =
  { Tool_spec.name = "find"
  ; parallel_safe = true
  ; description =
      "Find files by glob pattern (e.g. *.ml, src/**/*.ts). Respects \
       .gitignore. Returns paths relative to the searched directory."
  ; parameters =
      Tool_args.schema
        ~required:[ "pattern" ]
        [ "pattern", `String, "Glob pattern matched against the path"
        ; "path", `String, "Directory to search (default: working directory)"
        ; ( "limit"
          , `Integer
          , sprintf "Maximum number of results (default %d)" default_limit )
        ]
  }
;;

let run (context : Tool.Context.t) args =
  let pattern = Tool_args.string args "pattern" in
  let path =
    Tool.resolve_path
      context
      (Option.value (Tool_args.string_opt args "path") ~default:".")
  in
  let limit =
    Option.value (Tool_args.int_opt args "limit") ~default:default_limit
  in
  (* A bare name like [*.ml] should match at any depth. *)
  let glob =
    if String.is_prefix pattern ~prefix:"**/" || String.mem pattern '/'
    then pattern
    else "**/" ^ pattern
  in
  let output =
    Process.run_collect
      ~env:context.env
      ~cwd:path
      ~cancel:context.cancel
      ~prog:"rg"
      ~args:
        [ "--files"; "--no-require-git"; "--glob"; glob; "--color"; "never" ]
      ()
  in
  match output.exit with
  | Exited (0 | 1) ->
    let lines =
      List.sort (String.split_lines output.stdout) ~compare:String.compare
    in
    let total = List.length lines in
    if total = 0
    then Tool.Result.ok "No files found.\n"
    else (
      let shown = List.take lines limit in
      let note =
        if total > limit
        then sprintf "[%d of %d results shown]\n" limit total
        else ""
      in
      Tool.Result.ok (String.concat_lines shown ^ note))
  | Exited _ -> Tool.Result.error (String.strip output.stderr)
  | Signaled _ | Timed_out -> Tool.Result.error "find failed"
  | Cancelled -> Tool.Result.error "[cancelled]"
;;

let tool = { Tool.spec; run }
