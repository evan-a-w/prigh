open! Core
open! Import

let spec =
  { Tool_spec.name = "read"
  ; parallel_safe = true
  ; on_host = true
  ; destructive = false
  ; description =
      "Read a text file. Returns the content; large files are truncated and \
       can be read in pieces with offset and limit."
  ; parameters =
      Tool_args.schema
        ~required:[ "path" ]
        [ ( "path"
          , `String
          , "Path to the file (absolute or relative to the working directory)" )
        ; "offset", `Integer, "1-based line number to start from"
        ; "limit", `Integer, "Maximum number of lines to return"
        ]
  }
;;

let looks_binary s =
  let sample = String.prefix s 8192 in
  String.exists sample ~f:(fun c -> Char.equal c '\000')
;;

(* Shared by the read tool and prompt attachments: resolves [path] against
   [cwd], then reads and truncates it. Error strings include the resolved
   path. *)
let read_for_context ?(offset = 1) ?limit ~cwd path =
  let path = Tool.resolve ~cwd path in
  match Sys_unix.is_directory path with
  | `Yes -> Or_error.errorf "%s is a directory; use ls" path
  | `Unknown | `No ->
    (match Sys_unix.file_exists_exn path with
     | false -> Or_error.errorf "file not found: %s" path
     | true ->
       let content = In_channel.read_all path in
       if looks_binary content
       then Or_error.errorf "%s looks like a binary file" path
       else (
         let lines = String.split_lines content in
         let total = List.length lines in
         let selected = List.drop lines (offset - 1) in
         let selected =
           match limit with
           | Some n -> List.take selected n
           | None -> selected
         in
         let truncated = Truncate.head (String.concat_lines selected) in
         let shown = Truncate.count_lines truncated.text in
         let last_shown = offset - 1 + shown in
         let note =
           if truncated.truncated || last_shown < total
           then
             sprintf
               "\n[showing lines %d-%d of %d; use offset=%d to continue]"
               offset
               last_shown
               total
               (last_shown + 1)
           else ""
         in
         Ok (truncated.text ^ note)))
;;

let run (context : Tool.Context.t) args =
  let path = Tool_args.string args "path" in
  let offset = Option.value (Tool_args.int_opt args "offset") ~default:1 in
  let limit = Tool_args.int_opt args "limit" in
  if offset < 1 then raise (Tool_args.Invalid "offset must be >= 1");
  match read_for_context ~offset ?limit ~cwd:context.cwd path with
  | Ok text -> Tool.Result.ok text
  | Error e -> Tool.Result.error (Error.to_string_hum e)
;;

let tool = { Tool.spec; run }
