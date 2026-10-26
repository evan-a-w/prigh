open! Core
open! Import

let spec =
  { Tool_spec.name = "write"
  ; parallel_safe = false
  ; on_host = true
  ; freeform = None
  ; destructive = Always
  ; description =
      "Write a file, creating it (and parent directories) or overwriting it \
       entirely. An existing file must have been read first, and not changed \
       since. To change parts of an existing file, edit it instead."
  ; parameters =
      Tool_args.schema
        ~required:[ "path"; "content" ]
        [ "path", `String, "Path to the file"
        ; "content", `String, "Full content to write"
        ]
  }
;;

let rec mkdir_p dir =
  match Sys_unix.is_directory dir with
  | `Yes -> ()
  | `No | `Unknown ->
    mkdir_p (Filename.dirname dir);
    (try Core_unix.mkdir dir with
     | Core_unix.Unix_error (EEXIST, _, _) -> ())
;;

let run (context : Tool.Context.t) args =
  let path = Tool.resolve_path context (Tool_args.string args "path") in
  let content = Tool_args.string args "content" in
  let seen = Seen_files.of_args args in
  let existed = Sys_unix.file_exists_exn path in
  match
    if existed && Sys_unix.is_file_exn path
    then
      Seen_files.check_overwrite
        seen
        ~path
        ~current:(In_channel.read_all path)
        ~what:("overwrite " ^ path)
        ~retry:"write it again (or edit it to change parts of it)"
    else Ok ()
  with
  | Error message -> Tool.Result.error message
  | Ok () ->
    mkdir_p (Filename.dirname path);
    Out_channel.write_all path ~data:content;
    Option.iter seen ~f:(Seen_files.record ~path ~contents:content);
    let n = Truncate.count_lines content in
    Tool.Result.ok
      (sprintf
         "%s %d line%s to %s"
         (if existed then "overwrote" else "wrote")
         n
         (if n = 1 then "" else "s")
         path)
;;

let tool = { Tool.spec; run }
