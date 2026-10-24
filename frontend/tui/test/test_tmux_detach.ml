open! Core
open! Expect_test_helpers_core
module D = Prigh_tui.Tmux_detach

let none (_ : string) = false

let%expect_test "the tmux binary: $PRIGH_TMUX, else tmux" =
  List.iter
    [ None; Some ""; Some "/nix/store/x-tmux/bin/tmux" ]
    ~f:(fun prigh_tmux ->
      print_s
        [%sexp
          (prigh_tmux : string option), "->", (D.tmux ~prigh_tmux : string)]);
  [%expect
    {|
    (() -> tmux)
    (("") -> tmux)
    ((/nix/store/x-tmux/bin/tmux) -> /nix/store/x-tmux/bin/tmux)
    |}]
;;

let%expect_test "session names: prigh-<basename>, sanitised" =
  List.iter
    [ "/home/ann/lix"
    ; "/home/ann/lix.dev"
    ; "/srv/a:b"
    ; "/srv/my proj"
    ; "/srv/x#{session_name}"
    ; "/srv/semi;"
    ; "/srv/caf\xc3\xa9"
    ; "/srv/under_score-dash"
    ; "/"
    ]
    ~f:(fun dir ->
      print_s [%sexp (dir : string), "->", (D.base_name ~dir : string)]);
  [%expect
    {|
    (/home/ann/lix -> prigh-lix)
    (/home/ann/lix.dev -> prigh-lix_dev)
    (/srv/a:b -> prigh-a_b)
    ("/srv/my proj" -> prigh-my_proj)
    (/srv/x#{session_name} -> prigh-x__session_name_)
    ("/srv/semi;" -> prigh-semi_)
    ("/srv/caf\195\169" -> "prigh-caf\195\169")
    (/srv/under_score-dash -> prigh-under_score-dash)
    (/ -> prigh)
    |}]
;;

let%expect_test "a free name: -2, -3, ... after the taken ones" =
  let show taken =
    let exists name = List.mem taken name ~equal:String.equal in
    print_s
      [%sexp
        (taken : string list), "->", (D.free_name "prigh-lix" ~exists : string)]
  in
  show [];
  show [ "prigh-lix-2"; "prigh-other" ];
  show [ "prigh-lix" ];
  show [ "prigh-lix"; "prigh-lix-2"; "prigh-lix-3"; "prigh-lix-5" ];
  [%expect
    {|
    (() -> prigh-lix)
    ((prigh-lix-2 prigh-other) -> prigh-lix)
    ((prigh-lix) -> prigh-lix-2)
    ((prigh-lix prigh-lix-2 prigh-lix-3 prigh-lix-5) -> prigh-lix-4)
    |}]
;;

let%expect_test "the environment passed on" =
  let env =
    [ "PATH=/usr/bin:/bin"
    ; "HOME=/home/ann"
    ; "USER=ann"
    ; "SHELL=/bin/bash"
    ; "TERM=xterm-256color"
    ; "TMUX=/tmp/tmux-1000/default,123,0"
    ; "TMUX_PANE=%3"
    ; "TMUX_TMPDIR=/run/user/1000"
    ; "LANG=en_AU.UTF-8"
    ; "LC_ALL=C.UTF-8"
    ; "LC_TIME=en_GB.UTF-8"
    ; "EDITOR=nvim"
    ; "VISUAL=code -w"
    ; "XDG_CONFIG_HOME=/home/ann/.config"
    ; "PRIGH_BACKEND=/opt/prigh/bin/prigh"
    ; "PRIGH_TOKEN=s p a c e s \"double\" 'single' $HOME `tick` \\ ; trailing;"
    ; "PRIGH_CONNECT=srv:7777"
    ; "PRIGH_USER=ann"
    ; "PRIGH_TMUX=/nix/store/x-tmux/bin/tmux"
    ; "PRIGH_WEB_ROOT=/opt/prigh/share/web"
    ; "PRIGH_EMPTY="
    ; "PRIGH_MULTI=line one\nline two"
    ; "PRIGH_EQUALS=a=b=c"
    ; "ANTHROPIC_API_KEY=sk-ant"
    ; "ANTHROPIC_OAUTH_TOKEN=oat"
    ; "OPENAI_API_KEY=sk-oa"
    ; "DEEPSEEK_API_KEY=sk-ds"
    ; "MY_GATEWAY_API_KEY=sk-custom"
    ; "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
    ; "https_proxy=http://proxy:3128"
    ; "NO_PROXY=localhost"
    ; "OLDPWD=/tmp"
    ; "PWD=/home/ann"
    ; "SHLVL=2"
    ; "UNRELATED=dropped"
    ; "PRIGHT_NOT_A_PREFIX=dropped"
    ; "not-a-binding"
    ]
  in
  print_s [%sexp (D.environment env : (string * string) list)];
  [%expect
    {|
    ((PATH            /usr/bin:/bin)
     (HOME            /home/ann)
     (USER            ann)
     (SHELL           /bin/bash)
     (TMUX_TMPDIR     /run/user/1000)
     (LANG            en_AU.UTF-8)
     (LC_ALL          C.UTF-8)
     (LC_TIME         en_GB.UTF-8)
     (EDITOR          nvim)
     (VISUAL          "code -w")
     (XDG_CONFIG_HOME /home/ann/.config)
     (PRIGH_BACKEND   /opt/prigh/bin/prigh)
     (PRIGH_TOKEN "s p a c e s \"double\" 'single' $HOME `tick` \\ ; trailing;")
     (PRIGH_CONNECT         srv:7777)
     (PRIGH_USER            ann)
     (PRIGH_TMUX            /nix/store/x-tmux/bin/tmux)
     (PRIGH_WEB_ROOT        /opt/prigh/share/web)
     (PRIGH_EMPTY           "")
     (PRIGH_MULTI           "line one\nline two")
     (PRIGH_EQUALS          a=b=c)
     (ANTHROPIC_API_KEY     sk-ant)
     (ANTHROPIC_OAUTH_TOKEN oat)
     (OPENAI_API_KEY        sk-oa)
     (DEEPSEEK_API_KEY      sk-ds)
     (MY_GATEWAY_API_KEY    sk-custom)
     (SSL_CERT_FILE         /etc/ssl/certs/ca-bundle.crt)
     (https_proxy           http://proxy:3128)
     (NO_PROXY              localhost))
    |}]
;;

let show_create ?name ?(exists = none) ?(environment = []) ~dir command =
  let t = D.create ~tmux:"tmux" ~exists ?name ~environment ~dir ~command () in
  print_s [%sexp (t : D.t)];
  print_endline (D.message t ~inside_tmux:false)
;;

let%expect_test "the tmux command" =
  show_create
    ~environment:
      [ "PATH=/usr/bin"
      ; "HOME=/home/ann"
      ; "TMUX=/tmp/tmux-1000/default,1,0"
      ; "TERM=screen"
      ]
    ~dir:"/home/ann/lix"
    [ "/opt/prigh/bin/prigh-tui"; "-cwd"; "/home/ann/lix"; "-faux" ];
  [%expect
    {|
    ((tmux    tmux)
     (session prigh-lix)
     (args (
       new-session
       -d
       -s
       prigh-lix
       -c
       /home/ann/lix
       -e
       PATH=/usr/bin
       -e
       HOME=/home/ann
       --
       /bin/sh
       -c
       "\"$@\"\nstatus=$?\nif [ \"$status\" -ne 0 ]; then\n  printf '\\nprigh-tui exited with status %s: press Enter to close this session\\n' \"$status\"\n  read -r _\nfi\nexit \"$status\""
       prigh-tui
       /opt/prigh/bin/prigh-tui
       -cwd
       /home/ann/lix
       -faux)))
    prigh is running in tmux session prigh-lix: tmux attach -t prigh-lix
    |}]
;;

let args_after_script (t : D.t) =
  List.drop_while t.args ~f:(Fn.non (String.equal "prigh-tui")) |> List.tl_exn
;;

let%expect_test
    "values reach tmux intact: only a trailing ; is escaped, # in the \
     directory doubled"
  =
  let t =
    D.create
      ~tmux:"tmux"
      ~exists:none
      ~environment:
        [ "PRIGH_TOKEN=semi;"
        ; "PRIGH_USER=back\\;"
        ; "PRIGH_CONNECT=a;b"
        ; "PRIGH_X=new\nline \"q\" 'q' $V #{pane_id}"
        ]
      ~dir:"/srv/a#b;"
      ~command:[ "prigh-tui"; "-token"; "x;"; "-name"; "{ } ;;"; "#{x}"; ";" ]
      ()
  in
  print_s
    [%sexp
      { session : string = t.session
      ; dir : string = List.nth_exn t.args 5
      ; environment : string list =
          List.filter t.args ~f:(String.is_prefix ~prefix:"PRIGH_")
      ; command : string list = args_after_script t
      }];
  [%expect
    {|
    ((session prigh-a_b_)
     (dir     "/srv/a##b\\;")
     (environment (
       "PRIGH_TOKEN=semi\\;"
       "PRIGH_USER=back\\\\;"
       "PRIGH_CONNECT=a;b"
       "PRIGH_X=new\nline \"q\" 'q' $V #{pane_id}"))
     (command (prigh-tui -token "x\\;" -name "{ } ;\\;" #{x} "\\;")))
    |}]
;;

let%expect_test "taken names, -tmux-session, PRIGH_TMUX and inside tmux" =
  let exists = List.mem [ "prigh-lix"; "work" ] ~equal:String.equal in
  let message ?name ?(tmux = "tmux") ~inside_tmux () =
    let t =
      D.create
        ~tmux
        ~exists
        ?name
        ~environment:[]
        ~dir:"/home/ann/lix"
        ~command:[ "x" ]
        ()
    in
    print_endline (D.message t ~inside_tmux)
  in
  message ~inside_tmux:false ();
  message ~name:"work" ~inside_tmux:false ();
  message ~name:"my.work" ~inside_tmux:false ();
  message ~tmux:"/nix/store/x-tmux-3.7c/bin/tmux" ~inside_tmux:false ();
  message ~tmux:"/opt/my tmux/tmux" ~inside_tmux:false ();
  message ~inside_tmux:true ();
  [%expect
    {|
    prigh is running in tmux session prigh-lix-2: tmux attach -t prigh-lix-2
    prigh is running in tmux session work-2: tmux attach -t work-2
    prigh is running in tmux session my_work: tmux attach -t my_work
    prigh is running in tmux session prigh-lix-2: /nix/store/x-tmux-3.7c/bin/tmux attach -t prigh-lix-2
    prigh is running in tmux session prigh-lix-2: '/opt/my tmux/tmux' attach -t prigh-lix-2
    prigh is running in tmux session prigh-lix-2: tmux switch-client -t prigh-lix-2
    |}]
;;
