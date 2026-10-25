open! Core
open Prigh_web
module H = Harness

(* Signing in to GitHub and Forgejo hosts: the settings page's Git hosting
   section, /login and /logout with hosts, their pickers and completion, and
   the login dialog the auth events drive. *)

let auth fields = sprintf {|{"event":"auth",%s}|} fields

let forges ?(github_user = "") () =
  sprintf
    {|{"accounts":[%s,%s,%s]}|}
    (if String.is_empty github_user
     then H.forge_json ~kind:"github" ~label:"GitHub" "github.com"
     else
       H.forge_json
         ~user:github_user
         ~kind:"github"
         ~label:"GitHub"
         "github.com")
    (H.forge_json ~kind:"forgejo" ~label:"Forgejo" "codeberg.org")
    (H.forge_json
       ~oauth:false
       ~note:
         "no OAuth client id for git.example.com: set \
          forges.git.example.com.client_id in ~/.prigh/config.json"
       ~kind:"forgejo"
       ~label:"Forgejo"
       "git.example.com")
;;

let device_code =
  auth
    {|"kind":"auth_url","url":"https://github.com/login/device","instructions":"Enter the code ABCD-1234 at https://github.com/login/device"|}
;;

let section h = H.text h ~selector:".settings-section:nth-child(3)"

let%expect_test
    "settings: the git hosts, connecting with the device code, back to \
     settings, disconnecting"
  =
  let h = H.create () in
  H.quiet h (fun () -> H.act h Open_settings);
  H.act h (Reply (Forge Status, Ok (Jsonaf.of_string (forges ()))));
  section h;
  [%expect
    {|
    Git hosting
    GitHub github.com
    Not signed in.
    (Use a token) (Connect)
    Forgejo codeberg.org
    Not signed in.
    (Use a token) (Connect)
    Forgejo git.example.com
    Not signed in. No OAuth here: no OAuth client id for git.example.com: set forges.git.example.com.client_id in ~/.prigh/config.json
    (Connect with a token)
    Another Forgejo host A self-hosted Forgejo or Gitea, by its host name.
    (Add host…)
    |}];
  H.click h ~selector:".git-host .btn.primary";
  H.event h device_code;
  H.reply h "forge_login" "{}";
  H.text h ~selector:".modal";
  H.show h ~selector:".device-code";
  [%expect
    {|
    (Run "/login github")
    (Focus dialog)
    (Rpc (method_ forge_login) (params ((host github.com) (kind github)))
     (tag Login_started))
    Sign in to GitHub (github.com)
    (Close (Esc))
    Enter the code ABCD-1234 at https://github.com/login/device
    ABCD-1234 (Copy)
    (Open the login page)
    https://github.com/login/device
    Waiting for github.com…
    (Cancel)
    <div class="copyable device-code">
      <code aria-label="Your code: ABCD-1234" class="copy-text"> ABCD-1234 </code>
      <button type="button" title="Copy the code" class="btn copy small"> Copy </button>
    </div>
    |}];
  (* Signed in: back to the settings page, which asks again. *)
  H.event
    h
    (auth
       {|"kind":"forge_done","host":"github.com","user":"octo","method":"oauth"|});
  H.reply h "forge_status" (forges ~github_user:"octo" ());
  section h;
  [%expect
    {|
    (Focus dialog)
    (Expire_toast (id 0) (after_ms 3060))
    (Rpc (method_ forge_status) (params ()) (tag (Forge Status)))
    Git hosting
    GitHub github.com
    Signed in as octo (oauth).
    (Disconnect)
    Forgejo codeberg.org
    Not signed in.
    (Use a token) (Connect)
    Forgejo git.example.com
    Not signed in. No OAuth here: no OAuth client id for git.example.com: set forges.git.example.com.client_id in ~/.prigh/config.json
    (Connect with a token)
    Another Forgejo host A self-hosted Forgejo or Gitea, by its host name.
    (Add host…)
    |}];
  H.text h ~selector:".toast";
  [%expect {| Signed in to github.com as octo (oauth) |}];
  (* Disconnecting shows at once, until the backend says it is done. *)
  H.click h ~selector:".git-host .btn";
  H.text h ~selector:".git-host";
  [%expect
    {|
    (Run "/logout github.com")
    (Expire_toast (id 1) (after_ms 2620))
    (Rpc (method_ forge_logout) (params ((host github.com)))
     (tag (Forge (Logout github.com))))
    GitHub github.com
    Signed in as octo (oauth).
    (Disconnecting…)
    Forgejo codeberg.org
    Not signed in.
    (Use a token) (Connect)
    Forgejo git.example.com
    Not signed in. No OAuth here: no OAuth client id for git.example.com: set forges.git.example.com.client_id in ~/.prigh/config.json
    (Connect with a token)
    |}];
  H.reply h "forge_logout" "{}";
  H.event h (auth {|"kind":"forge_logged_out","host":"github.com"|});
  H.reply h "forge_status" (forges ());
  H.text h ~selector:".git-host:first-of-type";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Expire_toast (id 2) (after_ms 2500))
    (Rpc (method_ forge_status) (params ()) (tag (Forge Status)))
    GitHub github.com
    Not signed in.
    (Use a token) (Connect)
    Signed in to github.com as octo (oauth)
    Signed out of github.com
    |}]
;;

let%expect_test
    "a token, from the settings page; Esc cancels and goes back to it"
  =
  let h = H.create () in
  H.quiet h (fun () -> H.act h Open_settings);
  H.click h ~selector:".git-host .btn.ghost";
  H.event
    h
    (auth
       {|"kind":"prompt","id":"p1","prompt":"secret","message":"A personal access token for github.com (repo scope)","allow_empty":false|});
  H.text h ~selector:".modal";
  [%expect
    {|
    (Run "/login github token")
    (Focus dialog)
    (Rpc (method_ forge_login)
     (params ((host github.com) (kind github) (method token)))
     (tag Login_started))
    (Focus dialog-input)
    Sign in to GitHub (github.com)
    (Close (Esc))
    A personal access token for github.com (repo scope)
    []
    (Cancel) (Continue)
    |}];
  H.key h "Escape" ~target:Field;
  H.text h ~selector:".modal-head";
  [%expect
    {|
    Close_dialog
    (Focus dialog)
    (Rpc (method_ auth_cancel) (params ()) (tag Show_error))
    Settings
    (Close (Esc))
    |}];
  (* Its failure is the cancel's: nothing more is said. *)
  H.event
    h
    (auth {|"kind":"failed","provider":"github.com","error":"cancelled"|});
  H.text h ~selector:".toast";
  [%expect {| |}]
;;

let%expect_test "a failed sign-in says how to try again" =
  let h = H.create () in
  H.type_ h "/login forgejo codeberg.org";
  H.quiet h (fun () -> H.act h Send);
  H.event
    h
    (auth
       {|"kind":"failed","provider":"codeberg.org","error":"codeberg.org refused the token (401)"|});
  H.text h ~selector:".modal";
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    Sign in to Forgejo (codeberg.org)
    (Close (Esc))
    Couldn't sign in to codeberg.org: codeberg.org refused the token (401). /login forgejo codeberg.org tries again.
    (Close)
    Dialog_accept
    (Focus editor)
    |}]
;;

let%expect_test "another Forgejo host: from settings, or /login forgejo" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h Open_settings);
  H.click h ~selector:".git-host-add .btn";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Run "/login forgejo")
    (Focus dialog-input)
    Sign in to a Forgejo host
    (Close (Esc))
    A Forgejo or Gitea instance, by its host name: codeberg.org is listed already.
    []
    (Cancel) (Continue)
    |}];
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".dialog-error";
  [%expect
    {|
    Dialog_accept
    Type the host, e.g. codeberg.org (Esc cancels).
    |}];
  H.input h ~selector:"#dialog-input" "https://Git.Example.com/";
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".modal-head";
  [%expect
    {|
    (Dialog_input https://Git.Example.com/)
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ forge_login) (params ((host git.example.com) (kind forgejo)))
     (tag Login_started))
    Sign in to git.example.com
    (Close (Esc))
    |}];
  (* Done: back to the settings page it came from. *)
  H.event
    h
    (auth
       {|"kind":"forge_done","host":"git.example.com","user":"me","method":"oauth"|});
  H.text h ~selector:".modal-head";
  [%expect
    {|
    (Focus dialog)
    (Expire_toast (id 0) (after_ms 3180))
    (Rpc (method_ forge_status) (params ()) (tag (Forge Status)))
    Settings
    (Close (Esc))
    |}];
  (* From the editor: Esc closes it, nothing sent. *)
  H.act h Close_dialog;
  H.type_ h "/login forgejo";
  H.act h Send;
  H.key h "Escape" ~target:Field;
  [%expect
    {|
    (Focus editor)
    (Save_history ("/login forgejo"))
    (Focus dialog-input)
    Close_dialog
    (Focus editor)
    |}]
;;

let%expect_test "/login and /logout with git hosts" =
  let h = H.create () in
  H.act
    h
    (Reply (Forge Status, Ok (Jsonaf.of_string (forges ~github_user:"octo" ()))));
  let run command =
    H.quiet h (fun () -> H.act h Close_dialog);
    H.type_ h command;
    let m, commands = App.update (H.model h) Send in
    let shown =
      List.filter_map commands ~f:(function
        | Rpc { method_; params; _ } ->
          Some (sprintf "%s %s" method_ (Jsonaf.to_string (`Object params)))
        | _ -> None)
    in
    print_endline
      (String.concat
         ~sep:"; "
         ((command
           ::
           (match m.toasts with
            | t :: _ when t.error -> [ "error: " ^ t.text ]
            | _ -> []))
          @ (match m.dialog with
             | Some (Login flow) -> [ "login dialog for " ^ flow.provider ]
             | Some (Forge_host _) -> [ "which host?" ]
             | _ -> [])
          @ shown))
  in
  List.iter
    ~f:run
    [ "/login github"
    ; "/login github token"
    ; "/login github ghe.corp.com token"
    ; "/login forgejo codeberg.org"
    ; "/login codeberg.org token"
    ; "/login git.example.com"
    ; "/login forgejo"
    ; "/login unknown.host"
    ; "/login github later"
    ; "/login anthropic"
    ; "/login openai api_key"
    ; "/logout github"
    ; "/logout codeberg.org"
    ; "/logout anthropic"
    ];
  [%expect
    {|
    /login github; login dialog for github.com; forge_login {"host":"github.com","kind":"github"}
    /login github token; login dialog for github.com; forge_login {"host":"github.com","kind":"github","method":"token"}
    /login github ghe.corp.com token; login dialog for ghe.corp.com; forge_login {"host":"ghe.corp.com","kind":"github","method":"token"}
    /login forgejo codeberg.org; login dialog for codeberg.org; forge_login {"host":"codeberg.org","kind":"forgejo"}
    /login codeberg.org token; login dialog for codeberg.org; forge_login {"host":"codeberg.org","kind":"forgejo","method":"token"}
    /login git.example.com; login dialog for git.example.com; forge_login {"host":"git.example.com","kind":"forgejo"}
    /login forgejo; which host?
    /login unknown.host; error: No git host unknown.host is set up: /login forgejo unknown.host signs in to a Forgejo or Gitea there, /login github unknown.host to GitHub Enterprise.
    /login github later; error: Usage: /login github [HOST] [token]
    /login anthropic; login dialog for anthropic; login {"provider":"anthropic"}
    /login openai api_key; login dialog for openai; login {"provider":"openai","method":"api_key"}
    /logout github; forge_logout {"host":"github.com"}
    /logout codeberg.org; forge_logout {"host":"codeberg.org"}
    /logout anthropic; logout {"provider":"anthropic"}
    |}]
;;

let%expect_test "the /login and /logout pickers list the git hosts" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act
      h
      (Reply
         (Forge Status, Ok (Jsonaf.of_string (forges ~github_user:"octo" ()))));
    H.type_ h "/login";
    H.act h Send;
    H.reply h "auth_status" H.auth_json);
  H.act h (Picker_query "git");
  H.text h ~selector:".picker-items";
  [%expect
    {|
    GitHub (github.com) signed in as octo (oauth) ✓
    Forgejo: another host… a self-hosted Forgejo or Gitea: type its host
    Forgejo (codeberg.org) not signed in
    Forgejo (git.example.com) not signed in; with a token: no OAuth client id for git.example.com: set forges.git.example.com.client_id in ~/.prigh/config.json
    |}];
  H.act h (Picker_query "codeberg");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ forge_login) (params ((host codeberg.org) (kind forgejo)))
     (tag Login_started))
    |}];
  H.quiet h (fun () ->
    H.act h Close_dialog;
    H.type_ h "/login";
    H.act h Send;
    H.reply h "auth_status" H.auth_json);
  H.act h (Picker_query "another host");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Focus dialog-input)
    |}];
  H.quiet h (fun () ->
    H.act h Close_dialog;
    H.type_ h "/logout";
    H.act h Send;
    H.reply h "auth_status" H.auth_json);
  H.text h ~selector:".picker-items";
  H.act h (Picker_query "github");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Anthropic oauth via auth.json
    GitHub (github.com) signed in as octo (oauth)
    Dialog_accept
    (Focus editor)
    (Expire_toast (id 0) (after_ms 2620))
    (Rpc (method_ forge_logout) (params ((host github.com)))
     (tag (Forge (Logout github.com))))
    |}];
  H.text h ~selector:".toast";
  [%expect {| Signing out of github.com… |}];
  (* Refused: said, and no longer shown as on its way. *)
  H.fail h "forge_logout" "github.com: not signed in";
  H.text h ~selector:".toast";
  print_s [%sexp (Set.is_empty (H.model h).forges.signing_out : bool)];
  [%expect
    {|
    github.com: not signed in (Dismiss)
    true
    |}]
;;

let%expect_test "/auth lists the git hosts with the providers" =
  let h = H.create () in
  H.type_ h "/auth";
  H.quiet h (fun () -> H.act h Send);
  H.reply h "auth_status" H.auth_json;
  H.reply h "forge_status" (forges ~github_user:"octo" ());
  H.text h ~selector:".auth-git-hosts";
  [%expect
    {|
    (Focus dialog)
    Git hosting
    GitHub github.com
    Signed in as octo (oauth).
    (Disconnect)
    Forgejo codeberg.org
    Not signed in.
    (Use a token) (Connect)
    Forgejo git.example.com
    Not signed in. No OAuth here: no OAuth client id for git.example.com: set forges.git.example.com.client_id in ~/.prigh/config.json
    (Connect with a token)
    Another Forgejo host A self-hosted Forgejo or Gitea, by its host name.
    (Add host…)
    |}];
  (* Disconnecting from it keeps it open. *)
  H.click h ~selector:".auth-git-hosts .git-host .btn";
  H.elements h ~selector:".auth-dialog";
  [%expect
    {|
    (Run "/logout github.com")
    (Expire_toast (id 0) (after_ms 2620))
    (Rpc (method_ forge_logout) (params ((host github.com)))
     (tag (Forge (Logout github.com))))
    <div class="auth-dialog modal">
    |}]
;;

let%expect_test "completion offers the git hosts" =
  let h = H.create () in
  H.act
    h
    (Reply (Forge Status, Ok (Jsonaf.of_string (forges ~github_user:"octo" ()))));
  H.type_ h "/login git";
  H.text h ~selector:".popup-items";
  H.type_ h "/logout ";
  H.text h ~selector:".popup-items";
  [%expect
    {|
    github GitHub (github.com), signed in as octo (oauth)
    forgejo git.example.com Forgejo (git.example.com), not signed in
    forgejo another Forgejo or Gitea host: /login forgejo HOST
    Anthropic oauth via auth.json
    github.com GitHub (github.com), signed in as octo (oauth)
    |}];
  H.type_ h "/login forgejo cod";
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Save_history ("/login forgejo codeberg.org"))
    (Focus dialog)
    (Rpc (method_ forge_login) (params ((host codeberg.org) (kind forgejo)))
     (tag Login_started))
    |}]
;;

let%expect_test "a backend without git hosting; switching user asks again" =
  let h = H.create () in
  let switch () =
    H.quiet h (fun () ->
      H.act
        h
        (Reply
           ( User_switched
           , Ok
               (Jsonaf.of_string
                  {|{"client_id":"c1","user":"root","namespace":"bob"}|}) )))
  in
  switch ();
  print_s [%sexp ((H.model h).forges : Forges.t)];
  H.fail h "forge_status" "unknown method forge_status";
  H.quiet h (fun () -> H.act h Open_settings);
  section h;
  [%expect
    {|
    ((accounts Loading) (signing_out ()))
    Git hosting
    Git hosts This backend can't sign in to git hosts. Unknown method forge_status
    |}];
  switch ();
  H.reply h "forge_status" (forges ~github_user:"octo" ());
  print_s
    [%sexp
      (List.map (Forges.list (H.model h).forges) ~f:(fun a -> a.host, a.user)
       : (string * string option) list)];
  [%expect
    {|
    ((github.com (octo)) (codeberg.org ()) (git.example.com ()))
    |}]
;;
