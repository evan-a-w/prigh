open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

(* Web search (run by the provider, a block in the reply), web_fetch and
   job_input in the terminal transcript. *)

let decode line =
  match P.Server_message.of_line line with
  | Ok m -> print_s (P.Server_message.sexp_of_t m)
  | Error e -> print_s [%message "error" (e : Error.t)]
;;

let results =
  "1. Eio docs — https://ocaml.org/p/eio\n\
   2. Eio on GitHub — https://github.com/ocaml-multicore/eio\n\
   3. Effects tutorial — https://example.com/effects\n\
   4. Lwt vs Eio — https://example.com/lwt-eio\n\
   5. Eio and Domains — https://example.com/domains\n\
   6. Structured concurrency — https://example.com/sc\n"
;;

let%expect_test
    "protocol: server tool blocks, their deltas, exec stdin and input"
  =
  decode
    {|{"type":"event","event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"server_tool","id":"srvtoolu_1","name":"web_search","input":"{\"query\":\"ocaml eio\"}","output":"1. Eio docs — https://ocaml.org/p/eio","is_error":false},{"type":"server_tool","id":"srvtoolu_2","name":"web_search","input":"{\"query\":\"x\"}"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}}|};
  [%expect
    {|
    (Event (
      Message_end (
        Assistant (
          (content (
            (Text Looking.)
            (Server_tool (
              (id    srvtoolu_1)
              (name  web_search)
              (input "{\"query\":\"ocaml eio\"}")
              (output ("1. Eio docs \226\128\148 https://ocaml.org/p/eio"))
              (is_error false)))
            (Server_tool (
              (id    srvtoolu_2)
              (name  web_search)
              (input "{\"query\":\"x\"}")
              (output ())
              (is_error false)))))
          (stop_reason End_turn)
          (usage (
            (input      0)
            (output     0)
            (cache_read 0)))
          (model m)))))
    |}];
  let update delta =
    decode
      (sprintf
         {|{"type":"event","event":"message_update","partial":{"role":"assistant","content":[],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},"delta":%s}|}
         delta)
  in
  update
    {|{"type":"server_tool_use","id":"srvtoolu_1","name":"web_search","input":"{\"query\":\"ocaml eio\"}"}|};
  update
    {|{"type":"server_tool_result","id":"srvtoolu_1","output":"searched","is_error":true}|};
  update {|{"type":"server_tool_result","id":"srvtoolu_1","output":"searched"}|};
  decode
    {|{"type":"event","event":"tool_exec","host":"h","exec_id":"e1","call_id":"c1","name":"bash","arguments":{"command":"python3"},"cwd":"/work","stdin":true}|};
  decode
    {|{"type":"event","event":"tool_exec_input","host":"h","exec_id":"e1","data":"print(1)\n","close":true}|};
  decode
    {|{"type":"event","event":"tool_exec_input","host":"h","exec_id":"e1","data":"x"}|};
  [%expect
    {|
    (Event (
      Message_update
      (partial (
        (content ())
        (stop_reason End_turn)
        (usage (
          (input      0)
          (output     0)
          (cache_read 0)))
        (model m)))
      (delta (
        Server_tool_use
        (id    srvtoolu_1)
        (name  web_search)
        (input "{\"query\":\"ocaml eio\"}")))))
    (Event (
      Message_update
      (partial (
        (content ())
        (stop_reason End_turn)
        (usage (
          (input      0)
          (output     0)
          (cache_read 0)))
        (model m)))
      (delta (
        Server_tool_result
        (id       srvtoolu_1)
        (output   searched)
        (is_error true)))))
    (Event (
      Message_update
      (partial (
        (content ())
        (stop_reason End_turn)
        (usage (
          (input      0)
          (output     0)
          (cache_read 0)))
        (model m)))
      (delta (
        Server_tool_result
        (id       srvtoolu_1)
        (output   searched)
        (is_error false)))))
    (Event (
      Tool_exec
      (exec_id e1)
      (call_id c1)
      (name    bash)
      (arguments ((command python3)))
      (cwd   /work)
      (stdin true)))
    (Event (
      Tool_exec_input
      (exec_id e1)
      (data    "print(1)\n")
      (close   true)))
    (Event (
      Tool_exec_input
      (exec_id e1)
      (data    x)
      (close   false)))
    |}]
;;

let server_tool
      ?(name = "web_search")
      ?(input = {|{"query":"ocaml eio"}|})
      ?output
      ?(is_error = false)
      id
  : P.Content.Server_tool.t
  =
  { id; name; input; output; is_error }
;;

let show_item ?(styled = false) item =
  List.iter Verbosity.all ~f:(fun verbosity ->
    printf "== %s ==\n" (Verbosity.name verbosity);
    let content = Transcript.render_item ~width:70 item ~verbosity in
    print_endline
      (if styled then Content.to_styled content else Content.to_plain content))
;;

let%expect_test "web search: running, its results folded, failed, unknown tools"
  =
  show_item (Server_tool (server_tool "s1"));
  [%expect
    {|
    == quiet ==
    … web_search "ocaml eio"
    == normal ==
    … web_search "ocaml eio"
    == verbose ==
    … web_search "ocaml eio"
        {
          query: "ocaml eio"
        }
    |}];
  show_item ~styled:true (Server_tool (server_tool ~output:results "s1"));
  [%expect
    {|
    == quiet ==
    [green]✓ [/][bold]web_search[/] "ocaml eio"  [dim]6 results[/]
    == normal ==
    [green]✓ [/][bold]web_search[/] "ocaml eio"  [dim]6 results[/]
        [gray]1. Eio docs[/][gray]  [/][dim][link=https://ocaml.org/p/eio]https://ocaml.org/p/eio[/]
        [gray]2. Eio on GitHub[/][gray]  [/][dim][link=https://github.com/ocaml-multicore/eio]https://github.com/ocaml-multicore/eio[/]
        [gray]3. Effects tutorial[/][gray]  [/][dim][link=https://example.com/effects]https://example.com/effects[/]
        [gray]4. Lwt vs Eio[/][gray]  [/][dim][link=https://example.com/lwt-eio]https://example.com/lwt-eio[/]
        [gray]5. Eio and Domains[/][gray]  [/][dim][link=https://example.com/domains]https://example.com/domains[/]
        [dim]… 1 more result[/]
    == verbose ==
    [green]✓ [/][bold]web_search[/] "ocaml eio"  [dim]6 results[/]
        [dim]{[/]
        [dim]  query: "ocaml eio"[/]
        [dim]}[/]
        [gray]1. Eio docs[/][gray]  [/][dim][link=https://ocaml.org/p/eio]https://ocaml.org/p/eio[/]
        [gray]2. Eio on GitHub[/][gray]  [/][dim][link=https://github.com/ocaml-multicore/eio]https://github.com/ocaml-multicore/eio[/]
        [gray]3. Effects tutorial[/][gray]  [/][dim][link=https://example.com/effects]https://example.com/effects[/]
        [gray]4. Lwt vs Eio[/][gray]  [/][dim][link=https://example.com/lwt-eio]https://example.com/lwt-eio[/]
        [gray]5. Eio and Domains[/][gray]  [/][dim][link=https://example.com/domains]https://example.com/domains[/]
        [gray]6. Structured concurrency[/][gray]  [/][dim][link=https://example.com/sc]https://example.com/sc[/]
    |}];
  show_item (Server_tool (server_tool ~output:"searched" "s1"));
  [%expect
    {|
    == quiet ==
    ✓ web_search "ocaml eio"  1 line
    == normal ==
    ✓ web_search "ocaml eio"
        searched
    == verbose ==
    ✓ web_search "ocaml eio"
        {
          query: "ocaml eio"
        }
        searched
    |}];
  show_item
    (Server_tool
       (server_tool
          ~output:"web search failed: too many requests"
          ~is_error:true
          "s1"));
  [%expect
    {|
    == quiet ==
    ✕ web_search "ocaml eio"
        web search failed: too many requests
    == normal ==
    ✕ web_search "ocaml eio"
        web search failed: too many requests
    == verbose ==
    ✕ web_search "ocaml eio"
        {
          query: "ocaml eio"
        }
        web search failed: too many requests
    |}];
  show_item
    (Server_tool
       (server_tool
          ~name:"code_execution"
          ~input:{|{"code":"print(1)"}|}
          ~output:"1"
          "s1"));
  [%expect
    {|
    == quiet ==
    ✓ code_execution {"code":"print(1)"}  1 line
    == normal ==
    ✓ code_execution {"code":"print(1)"}
        1
    == verbose ==
    ✓ code_execution {"code":"print(1)"}
        {
          code: "print(1)"
        }
        1
    |}]
;;

let tool ?result name arguments =
  Transcript.Item.Tool
    { call = { id = "c1"; name; arguments }
    ; result =
        Option.map result ~f:(fun (is_error, text) ->
          { P.Message.Tool_result.tool_call_id = "c1"
          ; tool_name = name
          ; text
          ; is_error
          ; images = []
          ; at = None
          })
    ; live_tail = None
    ; subagent = None
    }
;;

let page =
  "https://ocaml.org/p/eio/latest (HTTP 200, text/html, 12.3 KB)\n\n\
   # Eio\n\n\
   Effects-based direct-style IO.\n\
   line 4\n\
   line 5\n\
   line 6\n\
   line 7\n"
;;

let%expect_test "web_fetch: the URL as a link, where it ended up, the page" =
  let arguments = {|{"url":"https://ocaml.org/p/eio"}|} in
  show_item ~styled:true (tool "web_fetch" arguments);
  [%expect
    {|
    == quiet ==
    [yellow]… [/][bold]web_fetch[/][link=https://ocaml.org/p/eio] https://ocaml.org/p/eio[/]
    == normal ==
    [yellow]… [/][bold]web_fetch[/][link=https://ocaml.org/p/eio] https://ocaml.org/p/eio[/]
    == verbose ==
    [yellow]… [/][bold]web_fetch[/][link=https://ocaml.org/p/eio] https://ocaml.org/p/eio[/]
        [dim]{[/]
        [dim]  url: "https://ocaml.org/p/eio"[/]
        [dim]}[/]
    |}];
  show_item (tool "web_fetch" arguments ~result:(false, page));
  [%expect
    {|
    == quiet ==
    ✓ web_fetch https://ocaml.org/p…  HTTP 200, text/html, 12.3 KB 7 lines
    == normal ==
    ✓ web_fetch https://ocaml.org/p/eio  HTTP 200, text/html, 12.3 KB
        → https://ocaml.org/p/eio/latest
        # Eio

        Effects-based direct-style IO.
        line 4
        line 5
        … 2 more lines
    == verbose ==
    ✓ web_fetch https://ocaml.org/p/eio  HTTP 200, text/html, 12.3 KB
        {
          url: "https://ocaml.org/p/eio"
        }
        → https://ocaml.org/p/eio/latest
        # Eio

        Effects-based direct-style IO.
        line 4
        line 5
        line 6
        line 7
    |}];
  show_item
    (tool "web_fetch" arguments ~result:(true, "fetch failed: HTTP 404"));
  [%expect
    {|
    == quiet ==
    ✕ web_fetch https://ocaml.org/p/eio
        fetch failed: HTTP 404
    == normal ==
    ✕ web_fetch https://ocaml.org/p/eio
        fetch failed: HTTP 404
    == verbose ==
    ✕ web_fetch https://ocaml.org/p/eio
        {
          url: "https://ocaml.org/p/eio"
        }
        fetch failed: HTTP 404
    |}]
;;

let%expect_test "job_input: the job, its input escaped and cut, closing stdin" =
  let arguments = {|{"id":"j1","input":"print(1)\n","close":true}|} in
  show_item (tool "job_input" arguments);
  [%expect
    {|
    == quiet ==
    … job_input j1 "print(1)\n"  close stdin
    == normal ==
    … job_input j1 "print(1)\n"  close stdin
    == verbose ==
    … job_input j1 "print(1)\n"  close stdin
        {
          id: "j1"
          input: "print(1)\n"
          close: true
        }
    |}];
  show_item
    (tool "job_input" arguments ~result:(false, "sent 9 bytes to job j1\n1"));
  [%expect
    {|
    == quiet ==
    ✓ job_input j1 "print(1)\n"  close stdin 2 lines
    == normal ==
    ✓ job_input j1 "print(1)\n"  close stdin
        sent 9 bytes to job j1
        1
    == verbose ==
    ✓ job_input j1 "print(1)\n"  close stdin
        {
          id: "j1"
          input: "print(1)\n"
          close: true
        }
        sent 9 bytes to job j1
        1
    |}];
  show_item
    (tool
       "job_input"
       {|{"id":"j2","input":"let x = \"a long line of text\" in\n\tprint x; more and more"}|});
  [%expect
    {|
    == quiet ==
    … job_input j2 "let x = \"a long line of text\" in\n\tp…"
    == normal ==
    … job_input j2 "let x = \"a long line of text\" in\n\tp…"
    == verbose ==
    … job_input j2 "let x = \"a long line of text\" in\n\tp…"
        {
          id: "j2"
          input: "let x = \"a long line of text\" in\n\tprint x; more and
        more"
        }
    |}];
  show_item (tool "job_input" {|{"id":"j3","input":"","close":true}|});
  [%expect
    {|
    == quiet ==
    … job_input j3  close stdin
    == normal ==
    … job_input j3  close stdin
    == verbose ==
    … job_input j3  close stdin
        {
          id: "j3"
          input: ""
          close: true
        }
    |}]
;;

let event json =
  match P.Event.of_json (Or_error.ok_exn (P.Json.parse json)) with
  | Ok e -> e
  | Error e -> raise_s [%message "bad event" json (e : Error.t)]
;;

let partial_with content =
  sprintf
    {|{"role":"assistant","content":[%s],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
    content
;;

let update ?(content = "") delta =
  event
    (sprintf
       {|{"event":"message_update","partial":%s,"delta":%s}|}
       (partial_with content)
       delta)
;;

let text_delta text = update (sprintf {|{"type":"text_delta","text":%S}|} text)

let search_use =
  update
    {|{"type":"server_tool_use","id":"s1","name":"web_search","input":"{\"query\":\"ocaml eio\"}"}|}
;;

let search_result =
  update
    (sprintf
       {|{"type":"server_tool_result","id":"s1","output":%s,"is_error":false}|}
       (P.Json.to_string (P.Json.str results)))
;;

let%expect_test "streamed: text, a search running, its results, more text" =
  let h = Test_app.connected ~width:70 ~height:18 () in
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (user "what is eio?"));
  H.event h (text_delta "Let me look that up.");
  H.event h search_use;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ what is eio?
      Let me look that up.
    … web_search "ocaml eio"
    ──────────────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  ctx:0.1%/1.0M  ⠋ working · Esc aborts · Enter steers
    |}];
  H.event h search_result;
  H.event h (text_delta "Eio is an effects-based ");
  H.event h (text_delta "IO library.");
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ what is eio?
      Let me look that up.
    ✓ web_search "ocaml eio"  6 results
        1. Eio docs  https://ocaml.org/p/eio
        2. Eio on GitHub  https://github.com/ocaml-multicore/eio
        3. Effects tutorial  https://example.com/effects
        4. Lwt vs Eio  https://example.com/lwt-eio
        5. Eio and Domains  https://example.com/domains
        … 1 more result
      Eio is an effects-based IO library.
    ──────────────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  ctx:0.1%/1.0M  ⠋ working · Esc aborts · Enter steers
    |}];
  H.event
    h
    (Message_end
       (assistant "Let me look that up.Eio is an effects-based IO library."));
  H.event h (State (state ()));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ what is eio?
      Let me look that up.
    ✓ web_search "ocaml eio"  6 results
        1. Eio docs  https://ocaml.org/p/eio
        2. Eio on GitHub  https://github.com/ocaml-multicore/eio
        3. Effects tutorial  https://example.com/effects
        4. Lwt vs Eio  https://example.com/lwt-eio
        5. Eio and Domains  https://example.com/domains
        … 1 more result
      Eio is an effects-based IO library.
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "a result before its use, and a use that only updates the query"
  =
  let t =
    List.fold
      ~init:Transcript.empty
      ~f:Transcript.apply
      [ update
          ~content:
            {|{"type":"server_tool","id":"s9","name":"web_search","input":"{\"query\":\"late\"}"}|}
          {|{"type":"server_tool_result","id":"s9","output":"searched"}|}
      ; search_use
      ; update
          {|{"type":"server_tool_use","id":"s1","name":"web_search","input":"{\"query\":\"ocaml eio 5\"}"}|}
      ]
  in
  print_endline
    (Content.to_plain (Transcript.render_all t ~width:70 ~verbosity:Normal));
  [%expect
    {|
    ✓ web_search "late"
        searched
    … web_search "ocaml eio 5"
    |}]
;;

let messages_json =
  sprintf
    {|[{"role":"user","text":"what is eio?"},{"role":"assistant","content":[{"type":"text","text":"Let me look that up."},{"type":"server_tool","id":"s1","name":"web_search","input":"{\"query\":\"ocaml eio\"}","output":%s,"is_error":false},{"type":"server_tool","id":"s2","name":"web_search","input":"{\"query\":\"eio rate\"}","output":"web search failed: too many requests","is_error":true},{"type":"text","text":"Eio is an effects-based IO library."}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}]|}
    (P.Json.to_string (P.Json.str results))
;;

let%expect_test "loaded: searches in place among the text, folded then verbose" =
  let h = Test_app.connected ~width:70 ~height:22 () in
  H.reply ~quiet:true h Initial_messages messages_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ what is eio?
      Let me look that up.
    ✓ web_search "ocaml eio"  6 results
        1. Eio docs  https://ocaml.org/p/eio
        2. Eio on GitHub  https://github.com/ocaml-multicore/eio
        3. Effects tutorial  https://example.com/effects
        4. Lwt vs Eio  https://example.com/lwt-eio
        5. Eio and Domains  https://example.com/domains
        … 1 more result
    ✕ web_search "eio rate"
        web search failed: too many requests
      Eio is an effects-based IO library.
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  String.iter "/verbosity verbose" ~f:(fun c ->
    H.step ~quiet:true h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h;
  H.show h;
  [%expect
    {|
    ▌ what is eio?
      Let me look that up.
    ✓ web_search "ocaml eio"  6 results
        {
          query: "ocaml eio"
        }
        1. Eio docs  https://ocaml.org/p/eio
        2. Eio on GitHub  https://github.com/ocaml-multicore/eio
        3. Effects tutorial  https://example.com/effects
        4. Lwt vs Eio  https://example.com/lwt-eio
        5. Eio and Domains  https://example.com/domains
        6. Structured concurrency  https://example.com/sc
    ✕ web_search "eio rate"
        {
          query: "eio rate"
        }
        web search failed: too many requests
      Eio is an effects-based IO library.
      view: verbose — everything is shown
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "a subagent's card names its searches as its steps" =
  let t =
    List.fold
      ~init:Transcript.empty
      ~f:Transcript.apply
      [ event
          {|{"event":"tool_start","call":{"id":"c1","name":"subagent","arguments":"{\"task\":\"research eio\"}"}}|}
      ; event
          {|{"event":"subagent_start","call_id":"c1","agent_id":"a1","task":"research eio","model":"m","tools":[]}|}
      ; event
          (sprintf
             {|{"event":"subagent","call_id":"c1","agent_id":"a1","inner":{"event":"message_end","message":%s}}|}
             (partial_with
                {|{"type":"server_tool","id":"s1","name":"web_search","input":"{\"query\":\"ocaml eio\"}","output":"searched"}|}))
      ]
  in
  print_endline
    (Content.to_plain (Transcript.render_all t ~width:70 ~verbosity:Normal));
  [%expect
    {|
    … subagent research eio
        ↳ web_search "ocaml eio"
    |}]
;;
