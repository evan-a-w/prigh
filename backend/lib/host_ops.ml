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

let read_file ~env ~cancel ~cwd path =
  Tool_read.read_for_context ~env ~cancel ~cwd path
;;

let instructions_op = "$instructions"

module Instructions = struct
  type t =
    { files : (string * string) list
    ; nix : bool
    ; skills : Skill.t list
    }
  [@@deriving sexp_of]
end

let on_path name =
  match Sys.getenv "PATH" with
  | None -> false
  | Some path ->
    List.exists (String.split path ~on:':') ~f:(fun dir ->
      (not (String.is_empty dir))
      &&
      let file = Filename.concat dir name in
      Result.is_ok (Core_unix.access file [ `Exec ])
      &&
      match Sys_unix.is_file file with
      | `Yes -> true
      | `No | `Unknown -> false)
;;

(* AGENTS.md/CLAUDE.md from the host's filesystem: the ancestors of [cwd]
   and the host's own ~/.prigh. A pseudo-tool so that it goes through the
   same executor (and hence the same host) as the real tools. [home] is only
   honoured when given (the backend's, for tests); the tool-host worker
   strips it so a remote host uses its own. With [with_nix], the reply is an
   object that also says whether [nix] is on the host's PATH; without it,
   the bare array that backends predating it expect. *)
let home_arg args =
  match Tool_args.string_opt args "home" with
  | Some home -> home
  | None -> Option.value (Sys.getenv "HOME") ~default:"/"
;;

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
        let home = home_arg args in
        let files =
          `Array
            (List.map
               (System_prompt.read_instructions ~cwd:context.cwd ~home)
               ~f:(fun (path, text) ->
                 `Object [ "path", `String path; "text", `String text ]))
        in
        Tool.Result.ok
          (Json.to_string
             (match Tool_args.bool_opt args "with_nix" with
              | Some true ->
                `Object
                  [ "files", files
                  ; ("nix", if on_path "nix" then `True else `False)
                  ; ( "skills"
                    , `Array
                        (List.map
                           (Skill.discover ~cwd:context.cwd ~home)
                           ~f:[%jsonaf_of: Skill.t]) )
                  ]
              | Some false | None -> files)))
  }
;;

let instructions_args ~home =
  `Object [ "home", `String home; "with_nix", `True ]
;;

(* Tool hosts predating [with_nix] answer with just the array of files. *)
let instructions_of_result (result : Tool.Result.t) : Instructions.t =
  let files = function
    | `Array items ->
      List.filter_map items ~f:(fun item ->
        match Json.member "path" item, Json.member "text" item with
        | Some (`String path), Some (`String text) -> Some (path, text)
        | _ -> None)
    | _ -> []
  in
  let none = { Instructions.files = []; nix = false; skills = [] } in
  if result.is_error
  then none
  else (
    match Json.parse result.text with
    | Ok (`Array _ as items) -> { none with files = files items }
    | Ok (`Object _ as json) ->
      { files = Option.value_map (Json.member "files" json) ~default:[] ~f:files
      ; nix =
          (match Json.member "nix" json with
           | Some `True -> true
           | _ -> false)
      ; skills =
          (match Json.member "skills" json with
           | Some (`Array items) ->
             List.filter_map items ~f:(fun item ->
               Option.try_with (fun () -> [%of_jsonaf: Skill.t] item))
           | _ -> [])
      }
    | _ -> none)
;;

let skill_op = "$skill"

(* The skill [name] as the host sees it now, with its body. *)
let skill ~cwd ~home name =
  let skills = Skill.discover ~cwd ~home in
  match List.find skills ~f:(fun s -> String.equal s.name name) with
  | None -> Tool.Result.error (Error.to_string_hum (Skill.unknown skills name))
  | Some skill ->
    (match Skill.parse ~path:skill.path (In_channel.read_all skill.path) with
     | exception exn -> Tool.Result.error (Exn.to_string exn)
     | Error e -> Tool.Result.error (Error.to_string_hum e)
     | Ok (skill, body) ->
       Tool.Result.ok
         (Json.to_string
            (`Object
                [ "skill", [%jsonaf_of: Skill.t] skill; "body", `String body ])))
;;

let skill_args ~home name =
  `Object [ "home", `String home; "name", `String name ]
;;

let skill_of_result (result : Tool.Result.t) =
  if result.is_error
  then Error (Error.of_string result.text)
  else
    Or_error.try_with (fun () ->
      let json = Json.parse result.text |> Or_error.ok_exn in
      ( [%of_jsonaf: Skill.t] (Option.value_exn (Json.member "skill" json))
      , match Json.member "body" json with
        | Some (`String body) -> body
        | _ -> "" ))
;;

let mcp_servers_op = "$mcp_servers"
let mcp_call_op = "$mcp_call"
let mcp_approve_op = "$mcp_approve"

let is_mcp_op name =
  List.mem
    [ mcp_servers_op; mcp_call_op; mcp_approve_op ]
    name
    ~equal:String.equal
;;

let mcp_servers_args ~home ~reconnect =
  `Object
    [ "home", `String home; ("reconnect", if reconnect then `True else `False) ]
;;

let mcp_call_args ~home ~source ~server ~tool arguments =
  `Object
    [ "home", `String home
    ; "source", `String source
    ; "server", `String server
    ; "tool", `String tool
    ; "arguments", arguments
    ]
;;

let mcp_approve_args ~home ~source ~server =
  `Object
    [ "home", `String home; "source", `String source; "server", `String server ]
;;

let listing_of_result (result : Tool.Result.t) =
  if result.is_error
  then Error (Error.of_string result.text)
  else Or_error.bind (Json.parse result.text) ~f:Mcp_tools.Listing.of_json
;;

let mcp ~mcp ~cancel ~cwd ~name ~(arguments : Json.t) =
  let home = home_arg arguments in
  let listing ~reconnect =
    Tool.Result.ok
      (Json.to_string
         (Mcp_tools.Listing.to_json
            (Mcp_tools.Listing.of_hub
               (Mcp_hub.servers mcp ~reconnect ~cwd ~home ()))))
  in
  let string key = Tool_args.string arguments key in
  if String.equal name mcp_servers_op
  then
    listing
      ~reconnect:
        (Option.value (Tool_args.bool_opt arguments "reconnect") ~default:false)
  else if String.equal name mcp_call_op
  then
    Mcp_hub.call
      mcp
      ~cancel
      ~source:(string "source")
      ~server:(string "server")
      ~home
      ~tool:(string "tool")
      ~arguments:
        (Option.value (Json.member "arguments" arguments) ~default:(`Object []))
  else (
    match
      Or_error.bind
        (Mcp_config.find ~home ~source:(string "source") (string "server"))
        ~f:(Mcp_config.approve ~home)
    with
    | Error e -> Tool.Result.error (Error.to_string_hum e)
    | Ok () -> listing ~reconnect:true)
;;

let host_tools () =
  instructions_tool :: List.filter Tools.all ~f:(fun t -> t.spec.on_host)
;;

let execute ~mcp:hub ~env ~cancel ~on_output ~cwd ~name ~(arguments : Json.t) =
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
    | Ok path -> read_file ~env ~cancel ~cwd path
    | Error e -> Tool.Result.error (Error.to_string_hum e))
  else if String.equal name list_paths_op
  then (
    match string_arg "prefix" with
    | Ok prefix -> list_paths ~env ~cwd prefix
    | Error e -> Tool.Result.error (Error.to_string_hum e))
  else if String.equal name skill_op
  then (
    match string_arg "name" with
    | Ok skill_name -> skill ~cwd ~home:(home_arg arguments) skill_name
    | Error e -> Tool.Result.error (Error.to_string_hum e))
  else if is_mcp_op name
  then (
    match hub with
    | None -> Tool.Result.error "MCP is not available on this tool host"
    | Some hub ->
      (try mcp ~mcp:hub ~cancel ~cwd ~name ~arguments with
       | Tool_args.Invalid message -> Tool.Result.error message))
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
