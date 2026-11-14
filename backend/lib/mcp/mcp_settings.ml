open! Core
open! Import

type t =
  { path : string
  ; mutex : Eio.Mutex.t
  ; mutable subscribers : (unit -> unit) list
  }

let path ~home = Filename.concat home ".prigh/mcp-servers.json"

let create ~home =
  { path = path ~home; mutex = Eio.Mutex.create (); subscribers = [] }
;;

let subscribe t ~f = t.subscribers <- f :: t.subscribers

let field json name =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let hide_values json =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, value) ->
         match key, value with
         | ("env" | "headers"), `Object pairs ->
           ( key
           , `Object (List.map pairs ~f:(fun (k, _) -> k, `String "<hidden>")) )
         | _ -> key, value))
  | json -> json
;;

module Entry = struct
  type t =
    { name : string
    ; enabled : bool
    ; config : Json.t
    }

  let sexp_of_t t =
    [%sexp
      { name : string = t.name
      ; enabled : bool = t.enabled
      ; config : string = Json.to_string (hide_values t.config)
      }]
  ;;

  let to_json t =
    `Object
      [ "name", `String t.name
      ; ("enabled", if t.enabled then `True else `False)
      ; "config", t.config
      ]
  ;;
end

let read t =
  match In_channel.read_all t.path with
  | exception Sys_error _ -> Ok []
  | text ->
    (match Json.parse text with
     | Error e ->
       Or_error.errorf
         "%s: invalid JSON (%s); fix or delete the file"
         t.path
         (Error.to_string_hum e)
     | Ok json ->
       let disabled =
         match field json "disabled" with
         | Some (`Array names) ->
           List.filter_map names ~f:(function
             | `String s -> Some s
             | _ -> None)
         | _ -> []
       in
       (match field json "mcpServers" with
        | None -> Ok []
        | Some (`Object servers) ->
          Ok
            (List.map servers ~f:(fun (name, config) ->
               { Entry.name
               ; enabled = not (List.mem disabled name ~equal:String.equal)
               ; config
               }))
        | Some _ ->
          Or_error.errorf
            "%s: mcpServers must be an object; fix or delete the file"
            t.path))
;;

let list t = read t

let enabled t =
  match read t with
  | Error _ -> `Object []
  | Ok entries ->
    `Object
      (List.filter_map entries ~f:(fun (e : Entry.t) ->
         Option.some_if e.enabled (e.name, e.config)))
;;

let write t entries =
  let json =
    `Object
      ([ ( "mcpServers"
         , `Object (List.map entries ~f:(fun (e : Entry.t) -> e.name, e.config))
         )
       ]
       @
       match
         List.filter_map entries ~f:(fun (e : Entry.t) ->
           Option.some_if (not e.enabled) (`String e.name))
       with
       | [] -> []
       | names -> [ "disabled", `Array names ])
  in
  Or_error.try_with (fun () ->
    Core_unix.mkdir_p (Filename.dirname t.path);
    let tmp = t.path ^ ".tmp" in
    Out_channel.with_file tmp ~perm:0o600 ~f:(fun oc ->
      Out_channel.output_string oc (Json.to_string_hum json ^ "\n"));
    Core_unix.chmod tmp ~perm:0o600;
    Core_unix.rename ~src:tmp ~dst:t.path)
;;

(* Reads, changes and writes the list under the lock, then tells the
   subscribers. *)
let change t ~f =
  let result =
    Eio.Mutex.use_rw ~protect:false t.mutex
    @@ fun () ->
    let open Or_error.Let_syntax in
    let%bind entries = read t in
    let%bind entries = f entries in
    let%map () = write t entries in
    entries
  in
  if Or_error.is_ok result
  then List.iter (List.rev t.subscribers) ~f:(fun f -> f ());
  result
;;

let check (name, config) =
  match Mcp_config.check_name name with
  | Error e -> Error e
  | Ok () ->
    (match Mcp_config.check config with
     | Ok () -> Ok ()
     | Error e -> Or_error.errorf "server %S: %s" name (Error.to_string_hum e))
;;

let parse text =
  let text = String.strip text in
  let parsed =
    match Json.parse text with
    | Ok json -> Ok json
    | Error e ->
      (* An entry copied without its object's braces. *)
      (match Json.parse ("{" ^ text ^ "}") with
       | Ok json -> Ok json
       | Error _ ->
         Or_error.errorf
           "this is not JSON (%s); paste an \"mcpServers\" object, e.g. \
            {\"mcpServers\": {\"NAME\": {\"command\": \"...\"}}}"
           (Error.to_string_hum e))
  in
  Or_error.bind parsed ~f:(fun json ->
    let servers =
      match field json "mcpServers" with
      | Some servers -> servers
      | None -> json
    in
    match servers with
    | `Object entries
      when List.exists entries ~f:(fun (key, _) ->
             List.mem [ "command"; "url"; "type" ] key ~equal:String.equal) ->
      Or_error.error_string
        "this is one server's definition without its name: write it as \
         {\"NAME\": {...}}"
    | `Object [] -> Or_error.error_string "there are no servers in it"
    | `Object entries ->
      let%map.Or_error () = List.map entries ~f:check |> Or_error.all_unit in
      entries
    | _ ->
      Or_error.error_string
        "paste an \"mcpServers\" object: {\"NAME\": {\"command\": ...}}")
;;

let names entries =
  match entries with
  | [] -> "there are none"
  | entries ->
    "they are: "
    ^ String.concat
        ~sep:", "
        (List.map entries ~f:(fun (e : Entry.t) -> e.name))
;;

let unknown entries name =
  Or_error.errorf
    "there is no MCP server %S in the settings (%s)"
    name
    (names entries)
;;

let taken name =
  Or_error.errorf
    "there is already an MCP server %S in the settings: choose another name, \
     or edit that one"
    name
;;

let add t servers =
  change t ~f:(fun entries ->
    let open Or_error.Let_syntax in
    let%bind () = List.map servers ~f:check |> Or_error.all_unit in
    let%map () =
      List.fold_result
        servers
        ~init:(List.map entries ~f:(fun e -> e.name))
        ~f:(fun names (name, _) ->
          if List.mem names name ~equal:String.equal
          then taken name
          else Ok (name :: names))
      |> Or_error.map ~f:ignore
    in
    entries
    @ List.map servers ~f:(fun (name, config) ->
      { Entry.name; enabled = true; config }))
;;

let with_entry entries name ~f =
  if List.exists entries ~f:(fun (e : Entry.t) -> String.equal e.name name)
  then
    List.map entries ~f:(fun (e : Entry.t) ->
      if String.equal e.name name then f e else Ok e)
    |> Or_error.all
  else unknown entries name
;;

let update t ~name ?rename config =
  let new_name = Option.value rename ~default:name in
  change t ~f:(fun entries ->
    let open Or_error.Let_syntax in
    let%bind () = check (new_name, config) in
    let%bind () =
      if
        (not (String.equal new_name name))
        && List.exists entries ~f:(fun e -> String.equal e.name new_name)
      then taken new_name
      else Ok ()
    in
    with_entry entries name ~f:(fun e -> Ok { e with name = new_name; config }))
;;

let remove t ~name =
  change t ~f:(fun entries ->
    if List.exists entries ~f:(fun e -> String.equal e.name name)
    then Ok (List.filter entries ~f:(fun e -> not (String.equal e.name name)))
    else unknown entries name)
;;

let set_enabled t ~name enabled =
  change t ~f:(fun entries ->
    with_entry entries name ~f:(fun e -> Ok { e with enabled }))
;;
