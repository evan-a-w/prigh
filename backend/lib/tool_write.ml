open! Core
open! Import

let spec =
  { Tool_spec.name = "write"
  ; parallel_safe = false
  ; description =
      "Write a file, creating it (and parent directories) or overwriting it \
       entirely. Prefer edit for changing parts of an existing file."
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
  let existed = Sys_unix.file_exists_exn path in
  mkdir_p (Filename.dirname path);
  Out_channel.write_all path ~data:content;
  Tool.Result.ok
    (sprintf
       "%s %s (%d bytes)"
       (if existed then "Overwrote" else "Created")
       path
       (String.length content))
;;

let tool = { Tool.spec; run }
