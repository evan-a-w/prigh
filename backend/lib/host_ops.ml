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

let list_paths_op = "$list_paths"

let list_paths ~env ~cwd prefix =
  Tool.Result.ok
    (Json.to_string
       (`Array
           (List.map (Path_listing.list ~env ~root:cwd ~prefix) ~f:(fun p ->
              `String p))))
;;

let list_dirs_op = "$list_dirs"

(* Completions for a directory being typed: [prefix] is what the user has
   so far (absolute, [~/...] or relative to [cwd]); the results keep that
   notation, so accepting one just extends the text. *)
let list_dirs ~cwd prefix =
  let dir_part, base =
    match String.rsplit2 prefix ~on:'/' with
    | Some (dir, base) -> dir ^ "/", base
    | None -> "", prefix
  in
  let dir =
    match String.rstrip dir_part ~drop:(Char.equal '/') with
    | "" -> if String.is_empty dir_part then cwd else "/"
    | dir -> Tool.resolve ~cwd dir
  in
  let names =
    match Sys_unix.ls_dir dir with
    | names -> names
    | exception _ -> []
  in
  let show_hidden = String.is_prefix base ~prefix:"." in
  names
  |> List.filter ~f:(fun name ->
    (show_hidden || not (String.is_prefix name ~prefix:"."))
    && String.Caseless.is_prefix name ~prefix:base
    &&
    match Sys_unix.is_directory (Filename.concat dir name) with
    | `Yes -> true
    | `No | `Unknown -> false)
  |> List.sort ~compare:String.compare
  |> Fn.flip List.take 200
  |> List.map ~f:(fun name -> `String (dir_part ^ name ^ "/"))
  |> fun items -> Tool.Result.ok (Json.to_string (`Array items))
;;

let read_file ~cwd path =
  match Tool_read.read_for_context ~cwd path with
  | Ok content -> Tool.Result.ok content
  | Error e -> Tool.Result.error (Error.to_string_hum e)
;;

let instructions_op = "$instructions"

(* AGENTS.md/CLAUDE.md from the host's filesystem: the ancestors of [cwd]
   and the host's own ~/.prigh. A pseudo-tool so that it goes through the
   same executor (and hence the same host) as the real tools. [home] is only
   honoured when given (the backend's, for tests); the tool-host worker
   strips it so a remote host uses its own. *)
let instructions_tool =
  { Tool.spec =
      { Tool_spec.name = instructions_op
      ; description = "project instruction files"
      ; parameters = `Object []
      ; parallel_safe = true
      ; destructive = false
      ; on_host = true
      }
  ; run =
      (fun context args ->
        let home =
          match Tool_args.string_opt args "home" with
          | Some home -> home
          | None -> Option.value (Sys.getenv "HOME") ~default:"/"
        in
        let files = System_prompt.read_instructions ~cwd:context.cwd ~home in
        Tool.Result.ok
          (Json.to_string
             (`Array
                 (List.map files ~f:(fun (path, text) ->
                    `Object [ "path", `String path; "text", `String text ])))))
  }
;;

let instructions_of_result (result : Tool.Result.t) =
  if result.is_error
  then []
  else (
    match Json.parse result.text with
    | Ok (`Array items) ->
      List.filter_map items ~f:(fun item ->
        match Json.member "path" item, Json.member "text" item with
        | Some (`String path), Some (`String text) -> Some (path, text)
        | _ -> None)
    | _ -> [])
;;

let host_tools () =
  instructions_tool :: List.filter Tools.all ~f:(fun t -> t.spec.on_host)
;;

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
  else if String.equal name list_paths_op
  then (
    match string_arg "prefix" with
    | Ok prefix -> list_paths ~env ~cwd prefix
    | Error e -> Tool.Result.error (Error.to_string_hum e))
  else if String.equal name list_dirs_op
  then (
    match string_arg "prefix" with
    | Ok prefix -> list_dirs ~cwd prefix
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
