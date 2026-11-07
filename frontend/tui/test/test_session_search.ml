open! Core
open! Expect_test_helpers_core
open Prigh_ui
module P = Prigh_protocol

let session
      ?name
      ?description
      ?first_prompt
      ?(cwd = "/home/u/src/prigh")
      ?branch
      ?(worktrees = [])
      ~updated
      id
  =
  { P.Session_summary.id
  ; path = "/s/" ^ id ^ ".jsonl"
  ; name
  ; description
  ; cwd
  ; created_at = "2026-01-01T00:00:00Z"
  ; updated_at = Some updated
  ; first_prompt
  ; message_count = 4
  ; parent = None
  ; live = false
  ; running = false
  ; background = 0
  ; clients = 0
  ; waiting = 0
  ; failed = false
  ; archived = false
  ; bytes = 0
  ; host = None
  ; repo =
      Option.map branch ~f:(fun branch ->
        { P.Repo.root = "/home/u/src/prigh"
        ; worktree = cwd
        ; branch = Some branch
        ; main = String.equal branch "main"
        })
  ; worktrees =
      List.map worktrees ~f:(fun branch ->
        { P.Session_worktree.path = "/home/u/.prigh/worktrees/prigh/" ^ branch
        ; branch = Some branch
        ; root = "/home/u/src/prigh"
        ; source = Created
        })
  ; home = None
  }
;;

(* Most recent first, as [list_sessions] gives them. *)
let sessions =
  [ session
      "s01"
      ~updated:"2026-11-07T10:00:00Z"
      ~name:"Fix flaky websocket reconnect"
      ~first_prompt:"the web client sometimes never reconnects after sleep"
      ~branch:"fix/ws-reconnect"
  ; session
      "s02"
      ~updated:"2026-11-07T09:00:00Z"
      ~name:"Make the port configurable"
      ~first_prompt:"prigh serve should take the listen port from the config file"
      ~branch:"feat/port-config"
      ~worktrees:[ "feat/port-config" ]
  ; session
      "s03"
      ~updated:"2026-11-06T18:00:00Z"
      ~first_prompt:"why does the compaction summary lose the plan?"
      ~branch:"main"
  ; session
      "s04"
      ~updated:"2026-11-06T12:00:00Z"
      ~name:"Session search"
      ~description:"fuzzy search over sessions and chat contents"
      ~first_prompt:"the current search sessions of prigh seems bad"
      ~branch:"feat/session-search"
  ; session
      "s05"
      ~updated:"2026-11-05T12:00:00Z"
      ~name:"Configure nix cache"
      ~first_prompt:"add prigh.cachix.org to the flake config"
      ~cwd:"/home/u/src/dotfiles"
  ; session
      "s06"
      ~updated:"2026-11-04T12:00:00Z"
      ~first_prompt:"port the tmux tests to the private socket"
      ~branch:"main"
  ; session
      "s07"
      ~updated:"2026-11-03T12:00:00Z"
      ~name:"Phone layout of the sidebar"
      ~first_prompt:"on a 320px phone the session list overflows sideways"
      ~branch:"fix/phone-sidebar"
  ; session
      "s08"
      ~updated:"2026-11-02T12:00:00Z"
      ~name:"Anthropic retry on overload"
      ~first_prompt:"retry 529 overloaded errors with backoff"
      ~branch:"main"
  ; session
      "s09"
      ~updated:"2026-11-01T12:00:00Z"
      ~first_prompt:"configure the reverse proxy for the web server port"
      ~cwd:"/home/u/src/infra"
  ; session
      "s10"
      ~updated:"2026-10-30T12:00:00Z"
      ~name:"Compaction keeps pinned messages"
      ~first_prompt:"pinned messages vanish after /compact"
      ~branch:"main"
  ; session
      "s11"
      ~updated:"2026-10-20T12:00:00Z"
      ~name:"Session search"
      ~first_prompt:"an older attempt at searching sessions"
      ~branch:"main"
  ; session
      "s12"
      ~updated:"2026-10-10T12:00:00Z"
      ~first_prompt:"hello"
      ~cwd:"/tmp"
  ]
;;

let queries =
  [ "port"
  ; "port config"
  ; "config port"
  ; "configurable port"
  ; "websocket"
  ; "web socket"
  ; "reconect"
  ; "compaction plan"
  ; "plan compaction"
  ; "sesion search"
  ; "search"
  ; "cfg"
  ; "ws"
  ; "e"
  ; "pr"
  ; "nix"
  ; "dotfiles cache"
  ; "phone overflow"
  ; "retry anthropic"
  ; "pinned"
  ; "xyz"
  ]
;;

let title (s : P.Session_summary.t) =
  match s.name with
  | Some name -> name
  | None -> P.Session_summary.blurb s
;;

(* What prigh-web's sidebar searched before: one subsequence over the title,
   first prompt, cwd and places run together. *)
let before ~query =
  Fuzzy.rank ~query sessions ~key:(fun (s : P.Session_summary.t) ->
    String.concat
      ~sep:" "
      ([ title s; Option.value s.first_prompt ~default:""; s.cwd ]
       @ Option.to_list (Session_worktrees.place s)
       @ List.map s.worktrees ~f:P.Session_worktree.label))
;;

let%expect_test "before: one subsequence over everything" =
  List.iter queries ~f:(fun query ->
    let ranked = before ~query in
    printf "%S (%d)\n" query (List.length ranked);
    List.iteri ranked ~f:(fun i s ->
      if i < 4 then printf "  %s\n" (title s));
    if List.length ranked > 4 then printf "  ...\n");
  [%expect {|
    "port" (10)
      port the tmux tests to the private socket
      configure the reverse proxy for the web server port
      Make the port configurable
      Configure nix cache
      ...
    "port config" (7)
      configure the reverse proxy for the web server port
      Make the port configurable
      Configure nix cache
      Anthropic retry on overload
      ...
    "config port" (7)
      Configure nix cache
      configure the reverse proxy for the web server port
      Make the port configurable
      Anthropic retry on overload
      ...
    "configurable port" (1)
      Make the port configurable
    "websocket" (1)
      Fix flaky websocket reconnect
    "web socket" (1)
      Fix flaky websocket reconnect
    "reconect" (4)
      Fix flaky websocket reconnect
      Configure nix cache
      Session search
      Make the port configurable
    "compaction plan" (1)
      why does the compaction summary lose the plan?
    "plan compaction" (1)
      why does the compaction summary lose the plan?
    "sesion search" (8)
      Session search
      Session search
      Phone layout of the sidebar
      why does the compaction summary lose the plan?
      ...
    "search" (8)
      Session search
      Session search
      Compaction keeps pinned messages
      Phone layout of the sidebar
      ...
    "cfg" (7)
      Configure nix cache
      Anthropic retry on overload
      Compaction keeps pinned messages
      Session search
      ...
    "ws" (5)
      Fix flaky websocket reconnect
      Phone layout of the sidebar
      Anthropic retry on overload
      configure the reverse proxy for the web server port
      ...
    "e" (12)
      hello
      Configure nix cache
      Session search
      Anthropic retry on overload
      ...
    "pr" (11)
      Configure nix cache
      Session search
      Anthropic retry on overload
      Compaction keeps pinned messages
      ...
    "nix" (4)
      Configure nix cache
      configure the reverse proxy for the web server port
      Phone layout of the sidebar
      Fix flaky websocket reconnect
    "dotfiles cache" (2)
      Configure nix cache
      Make the port configurable
    "phone overflow" (2)
      Phone layout of the sidebar
      Fix flaky websocket reconnect
    "retry anthropic" (1)
      Anthropic retry on overload
    "pinned" (2)
      Compaction keeps pinned messages
      Phone layout of the sidebar
    "xyz" (0)
    |}]
;;

let render_ranges text ranges =
  String.concat
    (List.map (Session_search.pieces text ranges) ~f:(function
       | `Plain, s -> s
       | `Match, s -> "[" ^ s ^ "]"))
;;

let why (w : Session_search.Why.t) =
  sprintf
    "%s:%s/%s"
    w.word
    (Sexp.to_string (Session_search.Field.sexp_of_t w.field))
    (Sexp.to_string (Session_search.Kind.sexp_of_t w.kind))
;;

let%expect_test "after: every word, in any field, best field and match first" =
  List.iter queries ~f:(fun query ->
    let ranked = Session_search.sessions ~query sessions in
    printf "%S (%d)\n" query (List.length ranked);
    List.iteri ranked ~f:(fun i (r : Session_search.Result.t) ->
      if i < 4
      then
        printf
          "  %s %3d %s  (%s)\n"
          r.session.id
          r.score
          (render_ranges (Session_search.title r.session) r.title)
          (String.concat ~sep:" " (List.map r.why ~f:why)));
    if List.length ranked > 4 then printf "  ...\n");
  [%expect {|
    "port" (3)
      s02  40 Make the [port] configurable  (port:Name/Exact)
      s06  30 [port] the tmux tests to the private socket  (port:Prompt/Exact)
      s09  30 configure the reverse proxy for the web server [port]  (port:Prompt/Exact)
    "port config" (2)
      s02  72 Make the [port] [config]urable  (port:Name/Exact config:Name/Prefix)
      s09  54 [config]ure the reverse proxy for the web server [port]  (port:Prompt/Exact config:Prompt/Prefix)
    "config port" (2)
      s02  72 Make the [port] [config]urable  (config:Name/Prefix port:Name/Exact)
      s09  54 [config]ure the reverse proxy for the web server [port]  (config:Prompt/Prefix port:Prompt/Exact)
    "configurable port" (1)
      s02  80 Make the [port] [configurable]  (configurable:Name/Exact port:Name/Exact)
    "websocket" (1)
      s01  40 Fix flaky [websocket] reconnect  (websocket:Name/Exact)
    "web socket" (1)
      s01  56 Fix flaky [websocket] reconnect  (web:Name/Prefix socket:Name/Substring)
    "reconect" (1)
      s01  16 Fix flaky websocket [reconnect]  (reconect:Name/Typo)
    "compaction plan" (1)
      s03  60 why does the [compaction] summary lose the [plan]?  (compaction:Prompt/Exact plan:Prompt/Exact)
    "plan compaction" (1)
      s03  60 why does the [compaction] summary lose the [plan]?  (plan:Prompt/Exact compaction:Prompt/Exact)
    "sesion search" (2)
      s04  56 [Session] [search]  (sesion:Name/Typo search:Name/Exact)
      s11  56 [Session] [search]  (sesion:Name/Typo search:Name/Exact)
    "search" (2)
      s04  40 Session [search]  (search:Name/Exact)
      s11  40 Session [search]  (search:Name/Exact)
    "cfg" (3)
      s02  12 Make the port [c]on[f]i[g]urable  (cfg:Name/Subsequence)
      s05  12 [C]on[f]i[g]ure nix cache  (cfg:Name/Subsequence)
      s09   9 [c]on[f]i[g]ure the reverse proxy for the web server port  (cfg:Prompt/Subsequence)
    "ws" (1)
      s01  10 Fix flaky websocket reconnect  (ws:Place/Exact)
    "e" (1)
      s08  24 Anthropic retry on overload  (e:Prompt/Prefix)
    "pr" (11)
      s02  24 Make the port configurable  (pr:Prompt/Prefix)
      s04  24 Session search  (pr:Prompt/Prefix)
      s05  24 Configure nix cache  (pr:Prompt/Prefix)
      s06  24 port the tmux tests to the [pr]ivate socket  (pr:Prompt/Prefix)
      ...
    "nix" (1)
      s05  40 Configure [nix] cache  (nix:Name/Exact)
    "dotfiles cache" (1)
      s05  50 Configure nix [cache]  (dotfiles:Place/Exact cache:Name/Exact)
    "phone overflow" (1)
      s07  64 [Phone] layout of the sidebar  (phone:Name/Exact overflow:Prompt/Prefix)
    "retry anthropic" (1)
      s08  80 [Anthropic] [retry] on overload  (retry:Name/Exact anthropic:Name/Exact)
    "pinned" (1)
      s10  40 Compaction keeps [pinned] messages  (pinned:Name/Exact)
    "xyz" (0)
    |}]
;;

let%expect_test "content hits merge in; a reply to another query is ignored" =
  let content query =
    { P.Session_hits.query
    ; hits =
        [ { id = "s12"
          ; path = "/s/s12.jsonl"
          ; score = 36
          ; snippet =
              Some
                { text = "…so we make the port configurable via the config file"
                ; ranges = [ 18, 4; 23, 6; 44, 6 ]
                }
          }
        ; { id = "s09"; path = "/s/s09.jsonl"; score = 60; snippet = None }
        ; { id = "gone"; path = "/s/gone.jsonl"; score = 80; snippet = None }
        ]
    }
  in
  let show ~query content =
    List.iter
      (Session_search.sessions ?content ~query sessions)
      ~f:(fun (r : Session_search.Result.t) ->
        printf
          "%s %3d %s\n"
          r.session.id
          r.score
          (render_ranges (Session_search.title r.session) r.title);
        Option.iter r.snippet ~f:(fun s ->
          printf "      %s\n" (render_ranges s.text s.ranges)))
  in
  show ~query:"port config" (Some (content "config  PORT"));
  [%expect {|
    s02  72 Make the [port] [config]urable
    s09  60 [config]ure the reverse proxy for the web server [port]
    s12  36 hello
          …so we make the [port] [config]urable via the [config] file
    |}];
  print_endline "-- stale:";
  show ~query:"port config" (Some (content "port"));
  [%expect {|
    -- stale:
    s02  72 Make the [port] [config]urable
    s09  54 [config]ure the reverse proxy for the web server [port]
    |}]
;;
