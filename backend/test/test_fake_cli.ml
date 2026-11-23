open! Core
open! Prigh

(* [prigh fake-cli]'s replay: our requests' ids carried into the responses,
   scripted commands, default answers, an exit. *)

let script lines =
  Fake_cli.Script.parse (String.concat ~sep:"\n" lines) |> Or_error.ok_exn
;;

let feed t line =
  print_endline ("> " ^ line);
  List.iter (Fake_cli.handle_line t line) ~f:(fun l -> print_endline ("< " ^ l))
;;

let%expect_test "a JSON-RPC peer: ids mapped, defaults, exit" =
  let s =
    script
      [ {|{"type":"command","args":["--version"],"stdout":"codex-cli 0.166.0\n"}|}
      ; {|{"type":"command","args":["login","status"],"exit":1,"stderr":"Not logged in\n"}|}
      ; {|{"type":"default_reply","method":"model/list","result":{"data":[]}}|}
      ; {|{"type":"expect_outbound","frame":{"id":1,"method":"initialize"}}|}
      ; {|{"type":"emit_inbound","frame":{"id":1,"result":{"userAgent":"codex/1"}}}|}
      ; {|{"type":"expect_outbound","frame":{"method":"initialized"}}|}
      ; {|{"type":"emit_inbound","frame":{"method":"note","params":{}}}|}
      ; {|{"type":"emit_inbound","frame":{"method":"ask","id":0,"params":{}}}|}
      ; {|{"type":"expect_outbound","frame":{"id":0}}|}
      ; {|{"type":"runtime_exit"}|}
      ; {|{"type":"exit","code":3,"stderr":"boom"}|}
      ]
  in
  print_s
    [%sexp
      (Fake_cli.command s [ "--version" ] : (int * string * string) option)];
  print_s
    [%sexp
      (Fake_cli.command s [ "login"; "status" ]
       : (int * string * string) option)];
  print_s
    [%sexp
      (Fake_cli.command s [ "app-server" ] : (int * string * string) option)];
  [%expect
    {|
    ((0 "codex-cli 0.166.0\n" ""))
    ((1 "" "Not logged in\n"))
    ()
    |}];
  let t = Fake_cli.create s in
  print_s [%sexp (Fake_cli.start t : string list)];
  feed t {|{"id":7,"method":"model/list","params":{}}|};
  feed t {|{"id":8,"method":"initialize","params":{}}|};
  feed t {|{"method":"initialized"}|};
  feed t {|{"id":0,"result":{"decision":"accept"}}|};
  print_s [%sexp (Fake_cli.exit_requested t : (int * string) option)];
  [%expect
    {|
    ()
    > {"id":7,"method":"model/list","params":{}}
    < {"id":7,"result":{"data":[]}}
    > {"id":8,"method":"initialize","params":{}}
    < {"id":8,"result":{"userAgent":"codex/1"}}
    > {"method":"initialized"}
    < {"method":"note","params":{}}
    < {"method":"ask","id":0,"params":{}}
    > {"id":0,"result":{"decision":"accept"}}
    ((3 boom))
    |}]
;;

let%expect_test "Claude's stream-json: control requests by subtype" =
  let s =
    script
      [ {|{"type":"expect_outbound","frame":{"type":"control_request","request_id":"r1","request":{"subtype":"initialize"}}}|}
      ; {|{"type":"emit_inbound","frame":{"type":"control_response","response":{"subtype":"success","request_id":"r1","response":{}}}}|}
      ; {|{"type":"expect_outbound","frame":{"type":"user"}}|}
      ; {|{"type":"emit_inbound","frame":{"type":"result","subtype":"success"}}|}
      ]
  in
  let t = Fake_cli.create s in
  feed
    t
    {|{"type":"control_request","request_id":"req_9","request":{"subtype":"interrupt"}}|};
  feed
    t
    {|{"type":"control_request","request_id":"req_1","request":{"subtype":"initialize"}}|};
  feed t {|{"type":"user","message":{"role":"user","content":"hi"}}|};
  [%expect
    {|
    > {"type":"control_request","request_id":"req_9","request":{"subtype":"interrupt"}}
    < {"type":"control_response","response":{"subtype":"success","request_id":"req_9","response":{}}}
    > {"type":"control_request","request_id":"req_1","request":{"subtype":"initialize"}}
    < {"type":"control_response","response":{"subtype":"success","request_id":"req_1","response":{}}}
    > {"type":"user","message":{"role":"user","content":"hi"}}
    < {"type":"result","subtype":"success"}
    |}]
;;
