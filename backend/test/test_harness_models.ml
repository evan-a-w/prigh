open! Core
open! Prigh

(* What the model chip and picker say of an agent CLI that could not list
   its models, by what asking it said. *)

let show ?prigh_login ?install why =
  print_s
    [%sexp
      (Harness_models.failed
         ~label:"Antigravity"
         ~host_name:"desktop"
         ~prigh_login
         ~install
         (Error.of_string why)
       : Harness_models.t)]
;;

let%expect_test "failure kinds, and what to do" =
  show "tool host \"host-desktop\" is not connected";
  show
    ~install:(Not_installed { login_shell_path = None })
    "exited before answering";
  show "agy: command not found";
  show "Antigravity is not logged in on this host: run `agy acp --login` there";
  show
    ~prigh_login:"google-antigravity"
    "it was refused prigh's google-antigravity login \"Work\", or could not \
     use it: /login google-antigravity logs in again, or Settings → Engines \
     runs Antigravity on its own login";
  show
    ~install:(Installed { version = Some "1.5.13"; path = "/bin/agy" })
    "initialize: timed out after 30s";
  [%expect
    {|
    (Failed (problem Unreachable) (summary "desktop not connected")
     (message
      "Antigravity on desktop could not list its models: tool host \"host-desktop\" is not connected. Start prigh's tool host there, or pick another host.")
     (fixes (Retry)))
    (Failed (problem Not_installed) (summary "not installed on desktop")
     (message
      "Antigravity on desktop could not list its models: exited before answering. Install it there (prigh never installs it), then retry; Settings \226\134\146 Engines says what is missing.")
     (fixes (Engines Retry)))
    (Failed (problem Not_installed) (summary "not installed on desktop")
     (message
      "Antigravity on desktop could not list its models: agy: command not found. Install it there (prigh never installs it), then retry; Settings \226\134\146 Engines says what is missing.")
     (fixes (Engines Retry)))
    (Failed (problem Not_logged_in) (summary "not logged in on desktop")
     (message
      "Antigravity on desktop could not list its models: Antigravity is not logged in on this host: run `agy acp --login` there. Check that it is installed and logged in there (Settings \226\134\146 Engines).")
     (fixes (Engines Retry)))
    (Failed (problem Login_refused) (summary "login refused")
     (message
      "Antigravity on desktop could not list its models: it was refused prigh's google-antigravity login \"Work\", or could not use it: /login google-antigravity logs in again, or Settings \226\134\146 Engines runs Antigravity on its own login.")
     (fixes ((Login (provider google-antigravity)) Engines Retry)))
    (Failed (problem Failed) (summary "couldn't list models")
     (message
      "Antigravity on desktop could not list its models: initialize: timed out after 30s. Check that it is installed and logged in there (Settings \226\134\146 Engines).")
     (fixes (Engines Retry)))
    |}]
;;
