open! Core
module H = Harness

(* The idle composer says where the session works: its cwd (under the tool
   host's home as ~), its branch, and the host when it is not the usual
   one. *)

let hosts =
  `Array
    [ `Object
        [ "id", `String "backend"
        ; "name", `String "srv"
        ; "cwd", `String "/home/prigh"
        ; "home", `String "/home/prigh"
        ]
    ; `Object
        [ "id", `String "c7"
        ; "name", `String "laptop"
        ; "cwd", `String "/Users/me/projects/prigh"
        ; "home", `String "/Users/me"
        ]
    ]
;;

let create ?sessions ?(fields = []) () =
  H.create
    ?sessions
    ~state:(H.state_json ~fields:(("hosts", hosts) :: fields) ())
    ()
;;

(* The editor's placeholder and tooltip (what screen readers get), then the
   lines the page stacks: the question's, shortest path first (style.css
   shows the longest that fits, on top), and the one without it. *)
let show h =
  H.names h ~selector:"#editor";
  H.text h ~selector:".cwd-hint-fit";
  H.text h ~selector:".cwd-hint-last"
;;

let%expect_test "a repository with a branch: the path under ~, the branch" =
  let h = create ~fields:[ "cwd", `String "/home/prigh/projects/prigh" ] () in
  show h;
  H.show h ~selector:".editor-wrap";
  [%expect
    {|
    textarea "Message" -- Working in ~/projects/prigh on main. Ask prigh anything…
    Working in …/prigh on main . Ask prigh anything…
    Working in ~/projects/prigh on main . Ask prigh anything…
    Working in …/prigh on main .
    <div class="editor-wrap">
      <textarea id="editor"
                placeholder="Working in ~/projects/prigh on main. Ask prigh anything…"
                title="Working in ~/projects/prigh on main. Ask prigh anything…"
                aria-label="Message"
                dir="auto"
                rows="1"
                autocomplete="off"
                enterkeyhint="send"
                class="cwd-placeholder editor"
                #value=""
                @on_input> </textarea>
      <div aria-hidden="true" class="cwd-hint">
        <span class="cwd-hint-stack">
          <span class="cwd-hint-last">
            <span class="cwd-hint-cut">
              Working
              in
              <span class="cwd-path"> …/prigh </span>
               on
              <span class="cwd-branch"> main </span>
              .
            </span>
          </span>
          <span class="cwd-hint-fit">
            <span class="cwd-hint-line">
              Working
              in
              <span class="cwd-path"> …/prigh </span>
               on
              <span class="cwd-branch"> main </span>
              . Ask prigh anything…
            </span>
          </span>
          <span class="cwd-hint-fit">
            <span class="cwd-hint-line">
              Working
              in
              <span class="cwd-path"> ~/projects/prigh </span>
               on
              <span class="cwd-branch"> main </span>
              . Ask prigh anything…
            </span>
          </span>
        </span>
      </div>
    </div>
    |}]
;;

let%expect_test "no repository (or a detached head): no branch" =
  let h =
    create
      ~fields:[ "cwd", `String "/home/prigh/notes"; "git_branch", `Null ]
      ()
  in
  show h;
  [%expect {|
    textarea "Message" -- Working in ~/notes. Ask prigh anything…
    Working in ~/notes . Ask prigh anything…
    Working in ~/notes .
    |}]
;;

let%expect_test "the home directory itself, and a path outside it" =
  let h = create ~fields:[ "cwd", `String "/home/prigh" ] () in
  show h;
  H.quiet h (fun () ->
    H.event
      h
      (sprintf
         {|{"event":"state","state":%s}|}
         (H.state_json
            ~fields:[ "hosts", hosts; "cwd", `String "/srv/data/a/b/c" ]
            ())));
  show h;
  [%expect {|
    textarea "Message" -- Working in ~ on main. Ask prigh anything…
    Working in ~ on main . Ask prigh anything…
    Working in ~ on main .
    textarea "Message" -- Working in /srv/data/a/b/c on main. Ask prigh anything…
    Working in …/c on main . Ask prigh anything…
    Working in …/b/c on main . Ask prigh anything…
    Working in /srv/data/a/b/c on main . Ask prigh anything…
    Working in …/c on main .
    |}]
;;

let%expect_test "another host: named first; its branch from the session list" =
  let fields =
    [ "cwd", `String "/Users/me/projects/prigh"
    ; "active_host", `String "c7"
    ; "git_branch", `Null
    ]
  in
  show (create ~fields ());
  let h =
    create
      ~fields
      ~sessions:
        (sprintf
           "[%s]"
           (H.session_json
              ~cwd:"/Users/me/projects/prigh"
              ~repo:
                ( "/Users/me/projects/prigh"
                , "/Users/me/projects/prigh"
                , Some "feat/x" )
              ~host:"c7"
              "s1"))
      ()
  in
  show h;
  (* This browser's own host, or the default for new sessions, is not
     named. *)
  H.quiet h (fun () ->
    H.act
      h
      (Hello
         { client_id = "c7"; namespace = None; user = None; superuser = false }));
  show h;
  H.quiet h (fun () ->
    H.act
      h
      (Hello
         { client_id = "c1"; namespace = None; user = None; superuser = false });
    H.act
      h
      (Reply
         ( Config
         , Ok (Jsonaf.of_string {|{"scoped_models":[],"default_host":"c7"}|}) )));
  show h;
  [%expect {|
    textarea "Message" -- Working on laptop in ~/projects/prigh. Ask prigh anything…
    Working on laptop in …/prigh . Ask prigh anything…
    Working on laptop in ~/projects/prigh . Ask prigh anything…
    Working on laptop in …/prigh .
    textarea "Message" -- Working on laptop in ~/projects/prigh on feat/x. Ask prigh anything…
    Working on laptop in …/prigh on feat/x . Ask prigh anything…
    Working on laptop in ~/projects/prigh on feat/x . Ask prigh anything…
    Working on laptop in …/prigh on feat/x .
    textarea "Message" -- Working in ~/projects/prigh on feat/x. Ask prigh anything…
    Working in …/prigh on feat/x . Ask prigh anything…
    Working in ~/projects/prigh on feat/x . Ask prigh anything…
    Working in …/prigh on feat/x .
    textarea "Message" -- Working in ~/projects/prigh on feat/x. Ask prigh anything…
    Working in …/prigh on feat/x . Ask prigh anything…
    Working in ~/projects/prigh on feat/x . Ask prigh anything…
    Working in …/prigh on feat/x .
    |}]
;;

let%expect_test "narrow: one line, the path cut first, then the question" =
  let h =
    create
      ~fields:
        [ "cwd", `String "/home/prigh/src/github.com/evan-a-w/prigh-worktrees/a"
        ]
      ()
  in
  H.quiet h (fun () -> H.act h (Set_narrow true));
  show h;
  H.show h ~selector:".cwd-hint";
  [%expect {|
    textarea "Message"
    Working in …/a on main . Ask prigh anything…
    Working in …/prigh-worktrees/a on main . Ask prigh anything…
    Working in ~/src/github.com/evan-a-w/prigh-worktrees/a on main . Ask prigh anything…
    Working in …/a on main .
    <div aria-hidden="true" class="cwd-hint">
      <span class="cwd-hint-stack">
        <span class="cwd-hint-last">
          <span class="cwd-hint-cut">
            Working
            in
            <span class="cwd-path"> …/a </span>
             on
            <span class="cwd-branch"> main </span>
            .
          </span>
        </span>
        <span class="cwd-hint-fit">
          <span class="cwd-hint-line">
            Working
            in
            <span class="cwd-path"> …/a </span>
             on
            <span class="cwd-branch"> main </span>
            . Ask prigh anything…
          </span>
        </span>
        <span class="cwd-hint-fit">
          <span class="cwd-hint-line">
            Working
            in
            <span class="cwd-path"> …/prigh-worktrees/a </span>
             on
            <span class="cwd-branch"> main </span>
            . Ask prigh anything…
          </span>
        </span>
        <span class="cwd-hint-fit">
          <span class="cwd-hint-line">
            Working
            in
            <span class="cwd-path"> ~/src/github.com/evan-a-w/prigh-worktrees/a </span>
             on
            <span class="cwd-branch"> main </span>
            . Ask prigh anything…
          </span>
        </span>
      </span>
    </div>
    |}]
;;

let%expect_test "typing hides it; no cwd yet, temporary, busy: the usual text" =
  let placeholder h =
    H.show h ~selector:"#editor";
    H.elements h ~selector:".cwd-hint"
  in
  let h = create ~fields:[ "cwd", `String "/home/prigh/p" ] () in
  H.type_ h "hi";
  placeholder h;
  placeholder (create ~fields:[ "cwd", `String "" ] ());
  placeholder (create ~fields:[ "temporary", `True ] ());
  let h = create () in
  H.quiet h (fun () -> H.act h (Set_temporary true));
  placeholder h;
  placeholder (H.connecting ());
  placeholder (create ~fields:[ "running", `True ] ());
  [%expect {|
    <textarea id="editor"
              placeholder="Working in ~/p on main. Ask prigh anything…"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="cwd-placeholder editor"
              #value="hi"
              @on_input> </textarea>
    <textarea id="editor"
              placeholder="Ask prigh anything…  / commands, @ files"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value=""
              @on_input> </textarea>
    <textarea id="editor"
              placeholder="Ask prigh anything…  / commands, @ files"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value=""
              @on_input> </textarea>
    <textarea id="editor"
              placeholder="Ask prigh anything…  / commands, @ files"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value=""
              @on_input> </textarea>
    <textarea id="editor"
              placeholder="Offline: write now, send once it is back"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value=""
              @on_input> </textarea>
    <textarea id="editor"
              placeholder="Steer (Enter) or queue a follow-up (Alt+Enter)…"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value=""
              @on_input> </textarea>
    |}]
;;
