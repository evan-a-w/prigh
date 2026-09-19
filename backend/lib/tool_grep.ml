open! Core
open! Import

let default_limit = 200

let spec =
  { Tool_spec.name = "grep"
  ; description =
      "Search file contents with a regular expression (ripgrep syntax). \
       Returns matching lines as path:line:text. Respects .gitignore."
  ; parameters =
      Tool_args.schema
        ~required:[ "pattern" ]
        [ "pattern", `String, "Regular expression to search for"
        ; ( "path"
          , `String
          , "File or directory to search (default: working directory)" )
        ; "glob", `String, "Only search files matching this glob, e.g. *.ml"
        ; "ignore_case", `Boolean, "Case-insensitive search"
        ; ( "limit"
          , `Integer
          , sprintf
              "Maximum number of matching lines (default %d)"
              default_limit )
        ]
  }
;;

let run (context : Tool.Context.t) args =
  let pattern = Tool_args.string args "pattern" in
  (* Passed relative to cwd (not resolved) so rg prints relative paths. *)
  let path =
    Option.value_map
      (Tool_args.string_opt args "path")
      ~default:"."
      ~f:Tool.expand_home
  in
  let limit =
    Option.value (Tool_args.int_opt args "limit") ~default:default_limit
  in
  let rg_args =
    List.concat
      [ [ "--line-number"
        ; "--no-heading"
        ; "--color"
        ; "never"
        ; "--with-filename"
        ; "--no-require-git"
        ; "--sort"
        ; "path"
        ]
      ; (if Option.value (Tool_args.bool_opt args "ignore_case") ~default:false
         then [ "--ignore-case" ]
         else [])
      ; Option.value_map
          (Tool_args.string_opt args "glob")
          ~default:[]
          ~f:(fun g -> [ "--glob"; g ])
      ; [ "--regexp"; pattern; "--"; path ]
      ]
  in
  let output =
    Process.run_collect
      ~env:context.env
      ~cwd:context.cwd
      ~cancel:context.cancel
      ~prog:"rg"
      ~args:rg_args
      ()
  in
  match output.exit with
  | Exited 0 ->
    let lines =
      List.map (String.split_lines output.stdout) ~f:(fun line ->
        String.chop_prefix_if_exists line ~prefix:"./")
    in
    let total = List.length lines in
    let shown = List.take lines limit in
    let truncated = Truncate.head (String.concat_lines shown) in
    let note =
      if total > limit || truncated.truncated
      then
        sprintf
          "[results truncated; showing first %d lines]\n"
          (Truncate.count_lines truncated.text)
      else ""
    in
    Tool.Result.ok (truncated.text ^ note)
  | Exited 1 -> Tool.Result.ok "No matches found.\n"
  | Exited _ -> Tool.Result.error (String.strip output.stderr)
  | Signaled _ | Timed_out -> Tool.Result.error "grep failed"
  | Cancelled -> Tool.Result.error "[cancelled]"
;;

let tool = { Tool.spec; run }
