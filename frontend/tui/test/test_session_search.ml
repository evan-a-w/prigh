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
  [%expect {| |}]
;;
