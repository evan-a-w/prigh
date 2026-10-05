(* Speaks newline-delimited JSON-RPC on stdin/stdout. Appends what it
   receives to $FAKE_MCP_LOG. [--exit-with-stderr] fails at startup;
   [--hang] never answers. *)

open! Core

let log line =
  match Sys.getenv "FAKE_MCP_LOG" with
  | None -> ()
  | Some path ->
    Out_channel.with_file path ~append:true ~f:(fun oc ->
      fprintf oc "%s\n" line)
;;

let send json =
  print_endline (Jsonaf.to_string json);
  Out_channel.flush stdout
;;

let respond id result =
  send (`Object [ "jsonrpc", `String "2.0"; "id", id; "result", result ])
;;

let respond_error id ~code message =
  send
    (`Object
        [ "jsonrpc", `String "2.0"
        ; "id", id
        ; ( "error"
          , `Object
              [ "code", `Number (Int.to_string code)
              ; "message", `String message
              ] )
        ])
;;

let text s = `Object [ "type", `String "text"; "text", `String s ]
let content items = `Object [ "content", `Array items ]

let string_member name json =
  Option.bind (Jsonaf.member name json) ~f:Jsonaf.string
;;

let tool ?(read_only = false) name description =
  `Object
    ([ "name", `String name
     ; "description", `String description
     ; ( "inputSchema"
       , `Object
           [ "type", `String "object"
           ; ( "properties"
             , `Object [ "text", `Object [ "type", `String "string" ] ] )
           ] )
     ]
     @
     if read_only
     then [ "annotations", `Object [ "readOnlyHint", `True ] ]
     else [])
;;

let tools_added = ref false

let tools_list id params =
  let cursor = Option.bind params ~f:(string_member "cursor") in
  log (sprintf "tools/list cursor=%s" (Option.value cursor ~default:"none"));
  match cursor with
  | None ->
    respond
      id
      (`Object
          [ ( "tools"
            , `Array
                [ tool ~read_only:true "echo" "Echoes its text"
                ; tool "image" "Returns an image"
                ; tool "error" "Fails"
                ] )
          ; "nextCursor", `String "page2"
          ])
  | Some _ ->
    respond
      id
      (`Object
          [ ( "tools"
            , `Array
                ([ tool "slow" "Never finishes"
                 ; tool "structured" "Returns structured content"
                 ; tool "ping_client" "Pings the client"
                 ; tool "change_tools" "Adds a tool"
                 ]
                 @ if !tools_added then [ tool "added" "Added later" ] else [])
            )
          ])
;;

(* Sends requests to the client and waits for their responses. *)
let ask_client requests =
  List.iter requests ~f:(fun (id, method_) ->
    send
      (`Object
          [ "jsonrpc", `String "2.0"
          ; "id", `String id
          ; "method", `String method_
          ; "params", `Object []
          ]));
  let rec wait answers =
    if List.length answers = List.length requests
    then List.rev answers
    else (
      match In_channel.input_line In_channel.stdin with
      | None -> exit 0
      | Some line ->
        let json = ok_exn (Jsonaf.parse line) in
        (match Jsonaf.member "method" json with
         | None -> wait (Jsonaf.to_string json :: answers)
         | Some _ -> wait answers))
  in
  wait []
;;

let tools_call id params =
  let name =
    Option.bind params ~f:(string_member "name") |> Option.value ~default:""
  in
  let arguments = Option.bind params ~f:(Jsonaf.member "arguments") in
  log (sprintf "tools/call %s" name);
  match name with
  | "echo" ->
    let s =
      Option.bind arguments ~f:(string_member "text")
      |> Option.value ~default:""
    in
    respond id (content [ text s; text "(echoed)" ])
  | "image" ->
    respond
      id
      (content
         [ text "here is a picture"
         ; `Object
             [ "type", `String "image"
             ; "data", `String "iVBORw0KGgo="
             ; "mimeType", `String "image/png"
             ]
         ; `Object
             [ "type", `String "resource"
             ; ( "resource"
               , `Object
                   [ "uri", `String "file:///notes.txt"
                   ; "text", `String "notes"
                   ] )
             ]
         ])
  | "error" ->
    respond
      id
      (`Object
          [ "content", `Array [ text "something broke" ]; "isError", `True ])
  | "structured" ->
    respond
      id
      (`Object
          [ "content", `Array []
          ; "structuredContent", `Object [ "answer", `Number "42" ]
          ])
  | "slow" -> ()
  | "ping_client" ->
    let answers = ask_client [ "s1", "ping"; "s2", "sampling/createMessage" ] in
    respond id (content (List.map answers ~f:text))
  | "change_tools" ->
    tools_added := true;
    send
      (`Object
          [ "jsonrpc", `String "2.0"
          ; "method", `String "notifications/tools/list_changed"
          ]);
    respond id (content [ text "changed" ])
  | "crash" ->
    eprintf "fake: crashing on purpose\n";
    exit 3
  | other -> respond_error id ~code:(-32602) (sprintf "Unknown tool: %s" other)
;;

let handle json =
  let id = Jsonaf.member "id" json in
  let params = Jsonaf.member "params" json in
  match string_member "method" json, id with
  | Some "initialize", Some id ->
    log
      (sprintf
         "initialize %s"
         (Jsonaf.to_string (Option.value params ~default:`Null)));
    respond
      id
      (`Object
          [ "protocolVersion", `String "2025-06-18"
          ; ( "capabilities"
            , `Object [ "tools", `Object [ "listChanged", `True ] ] )
          ; ( "serverInfo"
            , `Object [ "name", `String "fake"; "version", `String "1.0" ] )
          ])
  | Some "tools/list", Some id -> tools_list id params
  | Some "tools/call", Some id -> tools_call id params
  | Some "ping", Some id -> respond id (`Object [])
  | Some "notifications/cancelled", None ->
    log
      (sprintf
         "cancelled %s"
         (Jsonaf.to_string (Option.value params ~default:`Null)))
  | Some method_, None -> log method_
  | Some method_, Some id ->
    respond_error id ~code:(-32601) (sprintf "Method not found: %s" method_)
  | None, _ -> ()
;;

let () =
  let args = List.tl_exn (Array.to_list (Sys.get_argv ())) in
  log "start";
  if List.mem args "--exit-with-stderr" ~equal:String.equal
  then (
    eprintf "fake: could not read the config\nfake: FAKE_TOKEN is not set\n";
    exit 1);
  let hang = List.mem args "--hang" ~equal:String.equal in
  In_channel.iter_lines In_channel.stdin ~f:(fun line ->
    if not hang then handle (ok_exn (Jsonaf.parse line)));
  log "stdin closed"
;;
