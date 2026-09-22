open! Core
open! Import

(* Operations a tool host performs on behalf of a session: the [on_host]
   tools by name, plus two pseudo-tools the harness needs from the host's
   filesystem. *)

let resolve_dir_op = "$resolve_dir"
let read_file_op = "$read_file"

let resolve_dir ~cwd path =
  let path = Tool.resolve ~cwd path in
  match Sys_unix.is_directory path with
  | `Yes -> Tool.Result.ok (Filename_unix.realpath path)
  | `No | `Unknown -> Tool.Result.error (sprintf "not a directory: %s" path)
;;

let read_file ~cwd path =
  match Tool_read.read_for_context ~cwd path with
  | Ok content -> Tool.Result.ok content
  | Error e -> Tool.Result.error (Error.to_string_hum e)
;;

let host_tools () = List.filter Tools.all ~f:(fun t -> t.spec.on_host)

let execute ~env ~cancel ~on_output ~cwd ~name ~(arguments : Json.t) =
  let string_arg key =
    match arguments with
    | `Object fields ->
      (match List.Assoc.find fields ~equal:String.equal key with
       | Some (`String s) -> Ok s
       | _ -> Or_error.errorf "missing %S" key)
    | _ -> Or_error.errorf "missing %S" key
  in
  if String.equal name resolve_dir_op
  then (
    match string_arg "path" with
    | Ok path -> resolve_dir ~cwd path
    | Error e -> Tool.Result.error (Error.to_string_hum e))
  else if String.equal name read_file_op
  then (
    match string_arg "path" with
    | Ok path -> read_file ~cwd path
    | Error e -> Tool.Result.error (Error.to_string_hum e))
  else (
    match
      List.find (host_tools ()) ~f:(fun t -> String.equal (Tool.name t) name)
    with
    | None -> Tool.Result.error (sprintf "unknown host tool %S" name)
    | Some tool ->
      let context = Tool.Context.create ~cancel ~on_output ~env ~cwd () in
      Tool.execute tool context arguments)
;;
