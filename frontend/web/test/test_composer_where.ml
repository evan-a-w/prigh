open! Core
module H = Harness

(* The line under the composer says where the session works: its cwd (under
   the tool host's home as ~), its branch, and the host when it is not the
   usual one. A session with no messages yet shows the tray instead
   (Temporary_view), which says the same and more. *)

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
    ~state:
      (H.state_json
         ~fields:(("hosts", hosts) :: ("message_count", `Number "2") :: fields)
         ())
    ()
;;

(* The line's buttons, by their accessible names. *)
let show h = H.names ~selector:".composer-where button" h

let%expect_test "a repository with a branch: the path under ~, the branch" =
  let h = create ~fields:[ "cwd", `String "/home/prigh/projects/prigh" ] () in
  show h;
  H.show h ~selector:".composer-where";
  [%expect
    {|
    button "Folder: ~/projects/prigh. Change it" -- ~/projects/prigh. Start in another folder (/cd)
    button "Branch: main. Change it" -- Work in another worktree (/worktree)
    <div class="composer-where flex font-mono gap-4 items-center loon min-w-0 pt-1.5 px-2 text-muted-foreground text-xs">
      <button type="button"
              title="~/projects/prigh. Start in another folder (/cd)"
              aria-label="Folder: ~/projects/prigh. Change it"
              class="flex focus-visible:outline-1 focus-visible:outline-ring gap-1.5 hover:text-foreground items-center min-w-0 narrow:min-h-11 px-1 py-0.5 rounded-sm where-path"
              @on_click>
        <icon class="folder"> </icon>
        <span class="truncate"> ~/projects/prigh </span>
      </button>
      <button type="button"
              title="Work in another worktree (/worktree)"
              aria-label="Branch: main. Change it"
              class="flex focus-visible:outline-1 focus-visible:outline-ring gap-1.5 hover:text-foreground items-center min-w-0 ml-auto narrow:min-h-11 px-1 py-0.5 rounded-sm shrink-0 where-branch"
              @on_click>
        <icon class="branch"> </icon>
        <span class="max-w-[24ch] truncate"> main </span>
      </button>
    </div>
    |}]
;;

let%expect_test "its buttons run /cd and /worktree" =
  let h = create ~fields:[ "cwd", `String "/home/prigh/projects/prigh" ] () in
  H.click h ~selector:".where-path";
  H.click h ~selector:".where-branch";
  [%expect
    {|
    (Run /cd)
    (Focus dialog-input)
    (Rpc (method_ list_dirs)
     (params ((prefix /home/prigh/projects/prigh) (host backend)))
     (tag (Prompt_paths /home/prigh/projects/prigh)))
    (Run /worktree)
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag
      (Worktrees (place  "s1\
                        \nbackend\
                        \n/home/prigh/projects/prigh")
       (purpose Picker))))
    |}]
;;

let%expect_test "no repository (or a detached head): no branch" =
  let h =
    create
      ~fields:[ "cwd", `String "/home/prigh/notes"; "git_branch", `Null ]
      ()
  in
  show h;
  [%expect
    {| button "Folder: ~/notes. Change it" -- ~/notes. Start in another folder (/cd) |}]
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
  [%expect
    {|
    button "Folder: ~. Change it" -- ~. Start in another folder (/cd)
    button "Branch: main. Change it" -- Work in another worktree (/worktree)
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
         { client_id = "c7"
         ; namespace = None
         ; user = None
         ; superuser = false
         ; features = []
         }));
  show h;
  H.quiet h (fun () ->
    H.act
      h
      (Hello
         { client_id = "c1"
         ; namespace = None
         ; user = None
         ; superuser = false
         ; features = []
         });
    H.act
      h
      (Reply
         ( Config
         , Ok (Jsonaf.of_string {|{"scoped_models":[],"default_host":"c7"}|}) )));
  show h;
  [%expect
    {|
    button "Folder: laptop: ~/projects/prigh. Change it" -- On laptop, in ~/projects/prigh. Start in another folder (/cd)
    button "Folder: laptop: ~/projects/prigh. Change it" -- On laptop, in ~/projects/prigh. Start in another folder (/cd)
    button "Branch: feat/x. Change it" -- Work in another worktree (/worktree)
    button "Folder: ~/projects/prigh. Change it" -- ~/projects/prigh. Start in another folder (/cd)
    button "Branch: feat/x. Change it" -- Work in another worktree (/worktree)
    button "Folder: ~/projects/prigh. Change it" -- ~/projects/prigh. Start in another folder (/cd)
    button "Branch: feat/x. Change it" -- Work in another worktree (/worktree)
    |}]
;;

let%expect_test "narrow: the same line, its buttons a thumb's height" =
  let h =
    create
      ~fields:
        [ "cwd", `String "/home/prigh/src/github.com/evan-a-w/prigh-worktrees/a"
        ]
      ()
  in
  H.quiet h (fun () -> H.act h (Set_narrow true));
  show h;
  [%expect
    {|
    button "Folder: ~/src/github.com/evan-a-w/prigh-worktrees/a. Change it" -- ~/src/github.com/evan-a-w/prigh-worktrees/a. Start in another folder (/cd)
    button "Branch: main. Change it" -- Work in another worktree (/worktree)
    |}]
;;

let%expect_test "typing keeps it; no cwd yet, temporary, new: no line" =
  let line h =
    H.names ~selector:"#editor" h;
    H.elements ~selector:".composer-where" h
  in
  let h = create ~fields:[ "cwd", `String "/home/prigh/p" ] () in
  H.type_ h "hi";
  line h;
  line (create ~fields:[ "cwd", `String "" ] ());
  line (create ~fields:[ "temporary", `True ] ());
  let h = create () in
  H.quiet h (fun () -> H.act h (Set_temporary true));
  line h;
  line (H.connecting ());
  line (create ~fields:[ "running", `True ] ());
  line (H.create ~state:(H.state_json ~fields:[ "hosts", hosts ] ()) ());
  [%expect
    {|
    textarea "Message"
    <div class="composer-where flex font-mono gap-4 items-center loon min-w-0 pt-1.5 px-2 text-muted-foreground text-xs">
    textarea "Message"
    textarea "Message"
    textarea "Message"
    textarea "Message"
    textarea "Message"
    <div class="composer-where flex font-mono gap-4 items-center loon min-w-0 pt-1.5 px-2 text-muted-foreground text-xs">
    textarea "Message"
    |}]
;;
