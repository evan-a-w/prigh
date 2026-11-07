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
    "titles match at once; the messages once the typing pauses, merged in \
     with their passage"
  =
  let h = H.create ~sessions () in
  H.act h (Set_session_query "port");
  H.act h (Set_session_query "port config");
  H.text h ~selector:".session-main";
  [%expect
    {|
    (Search_sessions_later (search 1) (after_ms 150))
    (Search_sessions_later (search 2) (after_ms 150))
    Make the port config urable
    1h ago /work
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
  H.text h ~selector:".session-main";
  H.show h ~selector:".session-snippet";
  [%expect {|
    Make the port config urable
    1h ago /work
    fix the parser bug …then make the port config urable via the config file
    2h ago /work
    <span dir="auto" class="session-snippet">
      …then make the
      <mark> port </mark>

      <mark> config </mark>
      urable via the
      <mark> config </mark>
       file
    </span>
    |}]
;;

let%expect_test "a reply to an older search is ignored; Esc clears the search"
  =
  let h = H.create ~sessions () in
  H.act h (Set_session_query "port");
  H.act h (Search_sessions 1);
  H.act h (Set_session_query "port config");
  H.reply h "search_sessions" (hits ~query:"port");
  H.text h ~selector:".session-main";
  [%expect {|
    (Search_sessions_later (search 1) (after_ms 150))
    (Rpc (method_ search_sessions) (params ((query port) (limit 50)))
     (tag (Session_hits 1)))
    (Search_sessions_later (search 2) (after_ms 150))
    Make the port config urable
    1h ago /work
    |}];
  H.key h "Escape" ~target:Session_search;
  H.text h ~selector:".session-title";
  H.key h "Escape" ~target:Session_search;
  [%expect {|
    (Set_session_query "")
    Make the port configurable
    fix the parser bug
    Release notes
    Leave_sidebar
    (Focus editor)
    |}]
;;

let%expect_test
    "nothing found says where search looks; a backend without content \
     search still finds titles"
  =
  let h = H.create ~sessions () in
  H.act h (Set_session_query "zzz");
  H.act h (Search_sessions 1);
  H.fail h "search_sessions" "unknown method search_sessions";
  H.text h ~selector:".sessions";
  [%expect {|
    (Search_sessions_later (search 1) (after_ms 150))
    (Rpc (method_ search_sessions) (params ((query zzz) (limit 50)))
     (tag (Session_hits 1)))
    No sessions match “zzz”. Search looks in titles and messages.
    |}]
;;
