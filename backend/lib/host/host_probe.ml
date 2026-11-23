open! Core
open! Import

let op = "$probe_programs"
let command_timeout = Time_ns.Span.of_sec 5.
let max_output = 4096
let number n = `Number (Int.to_string n)
let strings l = `Array (List.map l ~f:(fun s -> `String s))
let opt f = Option.value_map ~default:`Null ~f

let field json key =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal key
  | _ -> None
;;

let string_field json key =
  match field json key with
  | Some (`String s) -> Some s
  | _ -> None
;;

let int_field json key =
  match field json key with
  | Some (`Number n) -> Int.of_string_opt n
  | _ -> None
;;

let list_field json key =
  match field json key with
  | Some (`Array l) -> l
  | _ -> []
;;

let string_list_field json key =
  List.filter_map (list_field json key) ~f:(function
    | `String s -> Some s
    | _ -> None)
;;

module Program = struct
  type t =
    { name : string
    ; version : string list
    ; status : string list
    }
  [@@deriving sexp_of]

  let to_json t =
    `Object
      [ "name", `String t.name
      ; "version", strings t.version
      ; "status", strings t.status
      ]
  ;;

  let of_json json =
    Option.map (string_field json "name") ~f:(fun name ->
      { name
      ; version = string_list_field json "version"
      ; status = string_list_field json "status"
      })
  ;;
end

module Request = struct
  type t =
    { programs : Program.t list
    ; paths : string list
    ; env : string list
    }
  [@@deriving sexp_of]

  let to_json t =
    `Object
      [ "programs", `Array (List.map t.programs ~f:Program.to_json)
      ; "paths", strings t.paths
      ; "env", strings t.env
      ]
  ;;

  let of_json json =
    { programs = List.filter_map (list_field json "programs") ~f:Program.of_json
    ; paths = string_list_field json "paths"
    ; env = string_list_field json "env"
    }
  ;;
end

module Command_output = struct
  type t =
    { exit : Process.Exit.t
    ; stdout : string
    ; stderr : string
    }
  [@@deriving sexp_of]

  let to_json t =
    `Object
      [ ( "exit"
        , match t.exit with
          | Exited code -> number code
          | Signaled _ -> `String "signaled"
          | Timed_out -> `String "timed_out"
          | Cancelled -> `String "cancelled" )
      ; "stdout", `String t.stdout
      ; "stderr", `String t.stderr
      ]
  ;;

  let of_json json =
    let exit : Process.Exit.t =
      match field json "exit" with
      | Some (`Number n) -> Exited (Int.of_string n)
      | Some (`String "timed_out") -> Timed_out
      | Some (`String "cancelled") -> Cancelled
      | _ -> Signaled Signal.kill
    in
    { exit
    ; stdout = Option.value (string_field json "stdout") ~default:""
    ; stderr = Option.value (string_field json "stderr") ~default:""
    }
  ;;
end

module Found = struct
  type t =
    { name : string
    ; path : string option
    ; login_shell_path : string option
    ; version : Command_output.t option
    ; status : Command_output.t option
    }
  [@@deriving sexp_of]

  let to_json t =
    `Object
      [ "name", `String t.name
      ; "path", opt (fun p -> `String p) t.path
      ; "login_shell_path", opt (fun p -> `String p) t.login_shell_path
      ; "version", opt Command_output.to_json t.version
      ; "status", opt Command_output.to_json t.status
      ]
  ;;

  let of_json json =
    let output key =
      match field json key with
      | Some (`Object _ as o) -> Some (Command_output.of_json o)
      | _ -> None
    in
    Option.map (string_field json "name") ~f:(fun name ->
      { name
      ; path = string_field json "path"
      ; login_shell_path = string_field json "login_shell_path"
      ; version = output "version"
      ; status = output "status"
      })
  ;;
end

module File = struct
  type t =
    { path : string
    ; exists : bool
    ; size : int option
    ; mode : int option
    }
  [@@deriving sexp_of]

  let to_json t =
    `Object
      [ "path", `String t.path
      ; ("exists", if t.exists then `True else `False)
      ; "size", opt number t.size
      ; "mode", opt number t.mode
      ]
  ;;

  let of_json json =
    Option.map (string_field json "path") ~f:(fun path ->
      { path
      ; exists =
          (match field json "exists" with
           | Some `True -> true
           | _ -> false)
      ; size = int_field json "size"
      ; mode = int_field json "mode"
      })
  ;;
end

module Reply = struct
  type t =
    { programs : Found.t list
    ; paths : File.t list
    ; env : (string * bool) list
    }
  [@@deriving sexp_of]

  let to_json t =
    `Object
      [ "programs", `Array (List.map t.programs ~f:Found.to_json)
      ; "paths", `Array (List.map t.paths ~f:File.to_json)
      ; ( "env"
        , `Object
            (List.map t.env ~f:(fun (k, set) -> k, if set then `True else `False))
        )
      ]
  ;;

  let of_result (result : Tool.Result.t) =
    if result.is_error
    then Or_error.error_string result.text
    else (
      match Json.parse result.text with
      | Error _ -> Or_error.error_string "bad reply from the tool host"
      | Ok json ->
        Ok
          { programs = List.filter_map (list_field json "programs") ~f:Found.of_json
          ; paths = List.filter_map (list_field json "paths") ~f:File.of_json
          ; env =
              (match field json "env" with
               | Some (`Object vars) ->
                 List.map vars ~f:(fun (k, v) ->
                   ( k
                   , match v with
                     | `True -> true
                     | _ -> false ))
               | _ -> [])
          })
  ;;
end

let clip s =
  let s =
    if String.length s > max_output
    then fst (Utf8.split_incomplete_suffix (String.prefix s max_output))
    else s
  in
  Utf8.sanitize s
;;

let command ~env ~cancel ~cwd ~path args =
  if List.is_empty args
  then None
  else (
    let output =
      Process.run_collect
        ~env
        ~cwd
        ~cancel
        ~timeout:command_timeout
        ~prog:path
        ~args
        ()
    in
    Some
      { Command_output.exit = output.exit
      ; stdout = clip output.stdout
      ; stderr = clip output.stderr
      })
;;

let program ~env ~cancel ~cwd ~environment (p : Program.t) =
  match Executable.resolve ~environment ~dir:cwd p.name with
  | Error _ ->
    { Found.name = p.name
    ; path = None
    ; login_shell_path = Executable.login_shell_path ~env p.name
    ; version = None
    ; status = None
    }
  | Ok path ->
    let version, status =
      Fiber.pair
        (fun () -> command ~env ~cancel ~cwd ~path p.version)
        (fun () -> command ~env ~cancel ~cwd ~path p.status)
    in
    { name = p.name; path = Some path; login_shell_path = None; version; status }
;;

let file path =
  let path = Tool.expand_home path in
  match Core_unix.stat path with
  | st ->
    { File.path
    ; exists = true
    ; size = Some (Int64.to_int_exn st.st_size)
    ; mode = Some (st.st_perm land 0o7777)
    }
  | exception _ -> { path; exists = false; size = None; mode = None }
;;

let execute ~env ~cancel ~cwd ~arguments () =
  let request = Request.of_json arguments in
  let environment = Executable.environment ~extra:[] in
  let programs =
    Fiber.List.map (program ~env ~cancel ~cwd ~environment) request.programs
  in
  let reply =
    { Reply.programs
    ; paths = List.map request.paths ~f:file
    ; env =
        List.map request.env ~f:(fun var ->
          ( var
          , match Sys.getenv var with
            | Some value -> not (String.is_empty value)
            | None -> false ))
    }
  in
  Tool.Result.ok (Json.to_string (Reply.to_json reply))
;;
