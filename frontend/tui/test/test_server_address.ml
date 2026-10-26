open! Core
open! Expect_test_helpers_core
open Prigh_ui

(* What -connect and /connect accept: HOST:PORT, or a prigh-web URL. Kept
   in step with the backend's [Backend_address] (test_web_connect.ml). *)

let%expect_test "addresses: parsed, normalised, displayed" =
  List.iter
    [ "127.0.0.1:7777"
    ; ":7777"
    ; "[::1]:7777"
    ; "wss://ai.example.com/ws"
    ; "https://ai.example.com/"
    ; "https://ai.example.com"
    ; "HTTPS://ai.example.com/prigh/"
    ; "wss://ai.example.com/prigh/ws?x=1#y"
    ; "http://10.0.0.5:7790/"
    ; "ws://[::1]:7790/ws"
    ; "wss://user@ai.example.com:8443/ws"
    ; " wss://ai.example.com/ws "
    ]
    ~f:(fun text ->
      match Server_address.parse text with
      | Error e -> print_endline e
      | Ok t ->
        print_s
          [%sexp
            { given = (text : string)
            ; parsed = (t : Server_address.t)
            ; normalized = (Server_address.normalize text : string)
            ; display = (Server_address.display t : string)
            ; authority = (Server_address.authority t : string)
            ; path = (Server_address.websocket_path t : string)
            }]);
  [%expect
    {|
    ((given 127.0.0.1:7777)
     (parsed (
       (tls    false)
       (host   127.0.0.1)
       (port   7777)
       (prefix "")
       (lines  true)))
     (normalized 127.0.0.1:7777)
     (display    127.0.0.1:7777)
     (authority  127.0.0.1:7777)
     (path       /ws))
    ((given :7777)
     (parsed (
       (tls    false)
       (host   127.0.0.1)
       (port   7777)
       (prefix "")
       (lines  true)))
     (normalized :7777)
     (display    127.0.0.1:7777)
     (authority  127.0.0.1:7777)
     (path       /ws))
    ((given [::1]:7777)
     (parsed (
       (tls    false)
       (host   ::1)
       (port   7777)
       (prefix "")
       (lines  true)))
     (normalized [::1]:7777)
     (display    [::1]:7777)
     (authority  [::1]:7777)
     (path       /ws))
    ((given wss://ai.example.com/ws)
     (parsed (
       (tls    true)
       (host   ai.example.com)
       (port   443)
       (prefix "")
       (lines  false)))
     (normalized wss://ai.example.com/ws)
     (display    wss://ai.example.com)
     (authority  ai.example.com)
     (path       /ws))
    ((given https://ai.example.com/)
     (parsed (
       (tls    true)
       (host   ai.example.com)
       (port   443)
       (prefix "")
       (lines  false)))
     (normalized wss://ai.example.com/ws)
     (display    wss://ai.example.com)
     (authority  ai.example.com)
     (path       /ws))
    ((given https://ai.example.com)
     (parsed (
       (tls    true)
       (host   ai.example.com)
       (port   443)
       (prefix "")
       (lines  false)))
     (normalized wss://ai.example.com/ws)
     (display    wss://ai.example.com)
     (authority  ai.example.com)
     (path       /ws))
    ((given HTTPS://ai.example.com/prigh/)
     (parsed (
       (tls    true)
       (host   ai.example.com)
       (port   443)
       (prefix /prigh)
       (lines  false)))
     (normalized wss://ai.example.com/prigh/ws)
     (display    wss://ai.example.com/prigh)
     (authority  ai.example.com)
     (path       /prigh/ws))
    ((given wss://ai.example.com/prigh/ws?x=1#y)
     (parsed (
       (tls    true)
       (host   ai.example.com)
       (port   443)
       (prefix /prigh)
       (lines  false)))
     (normalized wss://ai.example.com/prigh/ws)
     (display    wss://ai.example.com/prigh)
     (authority  ai.example.com)
     (path       /prigh/ws))
    ((given http://10.0.0.5:7790/)
     (parsed (
       (tls    false)
       (host   10.0.0.5)
       (port   7790)
       (prefix "")
       (lines  false)))
     (normalized ws://10.0.0.5:7790/ws)
     (display    ws://10.0.0.5:7790)
     (authority  10.0.0.5:7790)
     (path       /ws))
    ((given ws://[::1]:7790/ws)
     (parsed (
       (tls    false)
       (host   ::1)
       (port   7790)
       (prefix "")
       (lines  false)))
     (normalized ws://[::1]:7790/ws)
     (display    ws://[::1]:7790)
     (authority  [::1]:7790)
     (path       /ws))
    ((given wss://user@ai.example.com:8443/ws)
     (parsed (
       (tls    true)
       (host   ai.example.com)
       (port   8443)
       (prefix "")
       (lines  false)))
     (normalized wss://ai.example.com:8443/ws)
     (display    wss://ai.example.com:8443)
     (authority  ai.example.com:8443)
     (path       /ws))
    ((given " wss://ai.example.com/ws ")
     (parsed (
       (tls    true)
       (host   ai.example.com)
       (port   443)
       (prefix "")
       (lines  false)))
     (normalized wss://ai.example.com/ws)
     (display    wss://ai.example.com)
     (authority  ai.example.com)
     (path       /ws))
    |}]
;;

let%expect_test "addresses: what is wrong, and what is accepted" =
  List.iter
    [ "server"
    ; "server:0"
    ; "server:http"
    ; "ftp://server/"
    ; "wss:///ws"
    ; "wss://server:99999/ws"
    ; "a b:1"
    ]
    ~f:(fun text ->
      match Server_address.parse text with
      | Error e -> print_endline e
      | Ok t -> print_s [%sexp (t : Server_address.t)]);
  [%expect
    {|
    "server" is not HOST:PORT (prigh serve -listen, e.g. 127.0.0.1:7777) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)
    "server:0": bad port; expected HOST:PORT (prigh serve -listen, e.g. 127.0.0.1:7777) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)
    "server:http": bad port; expected HOST:PORT (prigh serve -listen, e.g. 127.0.0.1:7777) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)
    "ftp://server/": the scheme must be ws, wss, http or https; expected HOST:PORT (prigh serve -listen, e.g. 127.0.0.1:7777) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)
    "wss:///ws" has no host; expected HOST:PORT (prigh serve -listen, e.g. 127.0.0.1:7777) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)
    "wss://server:99999/ws": bad port; expected HOST:PORT (prigh serve -listen, e.g. 127.0.0.1:7777) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)
    "a b:1" is not HOST:PORT (prigh serve -listen, e.g. 127.0.0.1:7777) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)
    |}]
;;
