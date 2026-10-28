open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* The real [prigh acp] over pipes, as an editor runs it: its own backend
   in the process, a scripted model, the editor answering fs requests.
   Every line on stdout is protocol JSON. *)

let exe = Filename.concat (Sys_unix.getcwd ()) "../bin/main.exe"
let field name json = Option.value (Json.member name json) ~default:`Null

let string_field name json =
  Option.value (Json.string (field name json)) ~default:""
;;

let%expect_test "prigh acp -faux-script over stdio" =
  with_sandbox
  @@ fun t ->
  write t "proj/a.txt" "on disk\n";
  write
    t
    "script.json"
    {|[{"thinking": "Look first.", "text": "Reading.", "tool_calls": [{"id": "c1", "name": "read", "arguments": {"path": "a.txt"}}]},
       {"tool_calls": [{"id": "c2", "name": "bash", "arguments": {"command": "echo from bash"}}]},
       {"text": "Done."}]|};
  let proj = Filename.concat t.dir "proj" in
  let p =
    Core_unix.create_process_env
      ~prog:exe
      ~args:
        [ "acp"
        ; "-faux-script"
        ; Filename.concat t.dir "script.json"
        ; "-name"
        ; "testhost"
        ]
      ~env:
        (`Replace
            [ "HOME", t.dir
            ; "PATH", Option.value (Sys.getenv "PATH") ~default:"/usr/bin:/bin"
            ])
      ()
  in
  let to_agent = Core_unix.out_channel_of_descr p.stdin in
  let from_agent = Core_unix.in_channel_of_descr p.stdout in
  let send line =
    Out_channel.output_string to_agent (line ^ "\n");
    Out_channel.flush to_agent
  in
  let show line =
    let shown =
      match Json.parse line with
      | Error _ -> "NOT JSON: " ^ line
      | Ok json ->
        let jsonrpc = string_field "jsonrpc" json in
        let what =
          match field "method" json, field "params" json with
          | `String "session/update", params ->
            let update = field "update" params in
            let kind = string_field "sessionUpdate" update in
            (match kind, string_field "status" update with
             | "tool_call_update", "" -> "" (* output so far: timing *)
             | "tool_call_update", status ->
               let text =
                 match field "content" update with
                 | `Array (block :: _) ->
                   string_field "text" (field "content" block)
                 | _ -> ""
               in
               sprintf "update %s %s %S" kind status text
             | _ -> "update " ^ kind)
          | `String method_, _ -> method_
          | _ ->
            (match Json.member "error" json with
             | Some error -> "error " ^ Json.to_string error
             | None -> "result " ^ Json.to_string (field "id" json))
        in
        if String.equal jsonrpc "2.0" then what else "NOT JSON-RPC: " ^ line
    in
    if not (String.is_empty shown) then print_endline (mask t shown)
  in
  (* Reads and shows lines (answering the agent's requests) until the
     response to [id]; returns it. *)
  let rec until_response id =
    match In_channel.input_line from_agent with
    | None ->
      print_endline "(stdout closed)";
      `Null
    | Some line ->
      show line;
      let json = Json.of_string line in
      (match Json.member "id" json, Json.member "method" json with
       | Some request_id, Some (`String "fs/read_text_file") ->
         send
           (Json.to_string
              (`Object
                  [ "jsonrpc", `String "2.0"
                  ; "id", request_id
                  ; "result", `Object [ "content", `String "from the editor\n" ]
                  ]));
         until_response id
       | Some (`Number n), None when Int.of_string n = id -> json
       | _ -> until_response id)
  in
  send
    {|{"jsonrpc": "2.0", "id": 0, "method": "initialize", "params": {"protocolVersion": 1, "clientCapabilities": {"fs": {"readTextFile": true}}}}|};
  ignore (until_response 0 : Json.t);
  send
    (sprintf
       {|{"jsonrpc": "2.0", "id": 1, "method": "session/new", "params": {"cwd": %S, "mcpServers": []}}|}
       proj);
  let session = string_field "sessionId" (field "result" (until_response 1)) in
  send
    (sprintf
       {|{"jsonrpc": "2.0", "id": 2, "method": "session/prompt", "params": {"sessionId": %S, "prompt": [{"type": "text", "text": "go"}]}}|}
       session);
  print_endline (Json.to_string (field "result" (until_response 2)));
  Out_channel.close to_agent;
  let rest = In_channel.input_all from_agent in
  let err = In_channel.input_all (Core_unix.in_channel_of_descr p.stderr) in
  print_endline (mask t (sprintf "after stdin closed: %S, stderr: %S" rest err));
  print_s [%sexp (Core_unix.waitpid p.pid : Core_unix.Exit_or_signal.t)];
  [%expect
    {|
    result 0
    result 1
    update available_commands_update
    update agent_thought_chunk
    update agent_message_chunk
    update tool_call
    fs/read_text_file
    update tool_call_update in_progress ""
    update tool_call_update completed "from the editor\n"
    update tool_call
    update tool_call_update in_progress ""
    update tool_call_update completed "```\nfrom bash\n```"
    update agent_message_chunk
    result 2
    {"stopReason":"end_turn"}
    after stdin closed: "", stderr: ""
    (Ok ())
    |}]
;;
