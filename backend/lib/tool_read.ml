open! Core
open! Import

let spec =
  { Tool_spec.name = "read"
  ; parallel_safe = true
  ; on_host = true
  ; destructive = false
  ; description =
      "Read a text or image file. Returns the content; large text files are \
       truncated and can be read in pieces with offset and limit. Images (PNG, \
       JPEG, GIF, WebP) are attached for you to see; large ones are \
       downscaled."
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

let read_image ~env ~cancel ~path content =
  match Image.load ~env ~cancel content with
  | Error e -> Tool.Result.error (sprintf "%s: %s" path (Error.to_string_hum e))
  | Ok loaded ->
    Tool.Result.ok
      ~images:[ loaded.image ]
      (String.concat
         ~sep:"\n"
         (sprintf "Read image file [%s]" (Image.describe loaded)
          :: Option.to_list loaded.note))
;;

(* Shared by the read tool and prompt attachments: resolves [path] against
   [cwd], then reads and truncates it, or attaches it when it is an image.
   Error strings include the resolved path. *)
let read_for_context
      ~env
      ?(cancel = Cancellation.never)
      ?(offset = 1)
      ?limit
      ~cwd
      path
  =
  let path = Tool.resolve ~cwd path in
  match Sys_unix.is_directory path with
  | `Yes -> Tool.Result.error (sprintf "%s is a directory; use ls" path)
  | `Unknown | `No ->
    (match Sys_unix.file_exists_exn path with
     | false -> Tool.Result.error (sprintf "file not found: %s" path)
     | true ->
       let content = In_channel.read_all path in
       if Option.is_some (Image.sniff content)
       then read_image ~env ~cancel ~path content
       else if looks_binary content
       then Tool.Result.error (sprintf "%s looks like a binary file" path)
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
         Tool.Result.ok (truncated.text ^ note)))
;;

let run (context : Tool.Context.t) args =
  let path = Tool_args.string args "path" in
  let offset = Option.value (Tool_args.int_opt args "offset") ~default:1 in
  let limit = Tool_args.int_opt args "limit" in
  if offset < 1 then raise (Tool_args.Invalid "offset must be >= 1");
  read_for_context
    ~env:context.env
    ~cancel:context.cancel
    ~offset
    ?limit
    ~cwd:context.cwd
    path
;;

let tool = { Tool.spec; run }
