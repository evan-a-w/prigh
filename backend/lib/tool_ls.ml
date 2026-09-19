open! Core
open! Import

let spec =
  { Tool_spec.name = "ls"
  ; parallel_safe = true
  ; description =
      "List a directory. Directories are shown with a trailing slash."
  ; parameters =
      Tool_args.schema
        [ "path", `String, "Directory to list (default: working directory)"
        ; "limit", `Integer, "Maximum number of entries (default 500)"
        ]
  }
;;

let run (context : Tool.Context.t) args =
  let path =
    Tool.resolve_path
      context
      (Option.value (Tool_args.string_opt args "path") ~default:".")
  in
  let limit = Option.value (Tool_args.int_opt args "limit") ~default:500 in
  match Sys_unix.is_directory path with
  | `No | `Unknown -> Tool.Result.error (sprintf "not a directory: %s" path)
  | `Yes ->
    let entries =
      Sys_unix.ls_dir path
      |> List.sort ~compare:String.compare
      |> List.map ~f:(fun name ->
        match Sys_unix.is_directory (Filename.concat path name) with
        | `Yes -> name ^ "/"
        | `No | `Unknown -> name)
    in
    let total = List.length entries in
    let shown = List.take entries limit in
    let text = String.concat_lines shown in
    let note =
      if total > limit
      then sprintf "[%d of %d entries shown]\n" limit total
      else ""
    in
    Tool.Result.ok (if total = 0 then "(empty directory)\n" else text ^ note)
;;

let tool = { Tool.spec; run }
