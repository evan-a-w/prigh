open! Core
module H = Harness

let sessions =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ H.session_json
           ~name:"Make the port configurable"
           ~first_prompt:"prigh serve should take its port from the config"
           ~updated_at:"2026-10-05 09:00:00Z"
           "s1"
       ; H.session_json
           ~first_prompt:"fix the parser bug"
           ~updated_at:"2026-10-05 08:00:00Z"
           "s2"
       ; H.session_json
           ~name:"Release notes"
           ~first_prompt:"write the release notes"
           ~updated_at:"2026-10-04 08:00:00Z"
           "s3"
       ])
;;

let hits ~query =
  sprintf
    {|{"query":%S,"hits":[
      {"id":"s2","path":"/sessions/s2.jsonl","score":44,
       "snippet":{"text":"…then make the port configurable via the config file","ranges":[[17,4],[22,6],[43,6]]}},
      {"id":"s1","path":"/sessions/s1.jsonl","score":30,"snippet":null}]}|}
    query
;;

let%expect_test
    "titles match at once; the messages once the typing pauses, merged in with \
     their passage"
  =
  let h = H.create ~sessions () in
  H.quiet h (fun () -> H.act h Open_control);
  H.act h (Control_query "port");
  H.act h (Control_query "port config");
  H.text h ~selector:".control-list";
  [%expect
    {|
    (Search_sessions_later (search 1) (after_ms 150))
    (Search_sessions_later (search 2) (after_ms 150))
    Sessions
    Make the port config urable
    1h ago · /work
    |}];
  (* The first pause came too soon: the query changed since. *)
  H.act h (Search_sessions 1);
  H.act h (Search_sessions 2);
  [%expect
    {|
    (Rpc (method_ search_sessions) (params ((query "port config") (limit 50)))
     (tag (Session_hits 2)))
    |}];
  H.reply h "search_sessions" (hits ~query:"port config");
  H.text h ~selector:".control-list";
  H.show h ~selector:".control-snippet";
  [%expect
    {|
    Sessions
    Make the port config urable
    1h ago · /work
    fix the parser bug …then make the port config urable via the config file
    2h ago · /work
    <span dir="auto" class="control-snippet">
      …then make the
      <mark> port </mark>

      <mark> config </mark>
      urable via the
      <mark> config </mark>
       file
    </span>
    |}]
;;

let%expect_test
    "a reply to an older search is ignored; Esc closes the control centre"
  =
  let h = H.create ~sessions () in
  H.quiet h (fun () -> H.act h Open_control);
  H.act h (Control_query "port");
  H.act h (Search_sessions 1);
  H.act h (Control_query "port config");
  H.reply h "search_sessions" (hits ~query:"port");
  H.text h ~selector:".control-list";
  [%expect
    {|
    (Search_sessions_later (search 1) (after_ms 150))
    (Rpc (method_ search_sessions) (params ((query port) (limit 50)))
     (tag (Session_hits 1)))
    (Search_sessions_later (search 2) (after_ms 150))
    Sessions
    Make the port config urable
    1h ago · /work
    |}];
  H.key h "Escape" ~target:Field;
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  [%expect
    {|
    Close_dialog
    (Focus editor)
    false
    |}]
;;

let%expect_test
    "nothing found says where search looks; a backend without content search \
     still finds titles"
  =
  let h = H.create ~sessions () in
  H.quiet h (fun () -> H.act h Open_control);
  H.act h (Control_query "zzz");
  H.act h (Search_sessions 1);
  H.fail h "search_sessions" "unknown method search_sessions";
  H.text h ~selector:"#control-centre";
  [%expect
    {|
    (Search_sessions_later (search 1) (after_ms 150))
    (Rpc (method_ search_sessions) (params ((query zzz) (limit 50)))
     (tag (Session_hits 1)))
    [zzz] (Close (Esc))
    Nothing matches “zzz”: Backspace widens the search.
    ↑↓ move ↵ open ⌫ back esc close
    |}]
;;

let%expect_test
    "on a phone, a search typed while the session is still arriving stays, its \
     results shown"
  =
  let h = H.connecting () in
  H.quiet h (fun () ->
    H.act h (Set_narrow true);
    H.act h Open_control);
  H.act h (Control_query "port config");
  H.act h (Search_sessions 1);
  [%expect
    {|
    (Search_sessions_later (search 1) (after_ms 150))
    (Rpc (method_ search_sessions) (params ((query "port config") (limit 50)))
     (tag (Session_hits 1)))
    |}];
  H.quiet h (fun () ->
    H.reply h "get_state" (H.state_json ());
    H.reply h "list_models" H.models_json;
    H.reply h "auth_status" H.auth_json;
    H.reply h "get_config" H.config_json;
    H.reply h "forge_status" H.forges_json;
    H.reply h "get_messages_page" (H.page "[]");
    H.reply
      h
      "get_pending"
      {|{"steer_texts":[],"follow_up_texts":[],"confirms":[],"questions":[]}|};
    H.reply h "get_prefs" {|{"prefs":{"web.migrated":true}}|};
    H.reply h "list_sessions" sessions;
    H.act h (Tick H.now));
  H.reply h "search_sessions" (hits ~query:"port config");
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  H.show h ~selector:"#control-input";
  H.text h ~selector:".control-list";
  [%expect {| false |}]
;;
