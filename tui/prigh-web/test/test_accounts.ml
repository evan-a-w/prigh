open! Core
open Prigh_web
module H = Harness
module Account = Accounts.Account

let backend = "ws://127.0.0.1:7790/ws"
let other_backend = "wss://prigh.example.com/ws"

let dump (storage : Accounts.Storage.t) =
  List.iter [ Accounts.key; "prigh.user"; "prigh.token" ] ~f:(fun key ->
    printf "%s = %s\n" key (Option.value (storage.get key) ~default:"-"))
;;

let%expect_test "the account store: migration, sign-ins, sessions, removal" =
  let storage = Accounts.Storage.in_memory () in
  (* A login from before the list (the -web page's, or ours) is migrated once
     the backend accepts it. *)
  storage.set "prigh.user" "alice";
  storage.set "prigh.token" "a";
  print_s [%sexp (Accounts.load storage : Account.t list)];
  print_s [%sexp (Accounts.remember storage ~backend : Account.t list)];
  [%expect
    {|
    ()
    (((backend ws://127.0.0.1:7790/ws) (user (alice)) (token (a)) (session ())))
    |}];
  (* Signing in as bob: active once accepted, alice kept. *)
  Accounts.activate
    storage
    { backend; user = Some "bob"; token = Some "b"; session = None };
  ignore (Accounts.remember storage ~backend : Account.t list);
  dump storage;
  [%expect
    {|
    prigh-web.accounts = [{"backend":"ws://127.0.0.1:7790/ws","user":"alice","token":"a"},{"backend":"ws://127.0.0.1:7790/ws","user":"bob","token":"b"}]
    prigh.user = bob
    prigh.token = b
    |}];
  (* Sessions are remembered per account; acting accounts are matched by
     backend and user, so a new token replaces the old one. *)
  let bob = Option.value_exn (Accounts.current storage ~backend) in
  Accounts.set_session storage bob "s7";
  storage.set "prigh.token" "b2";
  ignore (Accounts.remember storage ~backend : Account.t list);
  print_s [%sexp (Accounts.current storage ~backend : Account.t option)];
  [%expect
    {| (((backend ws://127.0.0.1:7790/ws) (user (bob)) (token (b2)) (session (s7)))) |}];
  (* Another backend with the same user name is another account. *)
  Accounts.activate
    storage
    { backend = other_backend
    ; user = Some "bob"
    ; token = Some "x"
    ; session = None
    };
  ignore (Accounts.remember storage ~backend:other_backend : Account.t list);
  print_s
    [%sexp
      (List.map (Accounts.load storage) ~f:(fun a ->
         Account.name a, Account.host a)
       : (string * string) list)];
  [%expect
    {| ((alice 127.0.0.1:7790) (bob 127.0.0.1:7790) (bob prigh.example.com)) |}];
  (* Removing the active account signs it out; another just goes. *)
  Accounts.remove storage { bob with backend = other_backend };
  dump storage;
  [%expect
    {|
    prigh-web.accounts = [{"backend":"ws://127.0.0.1:7790/ws","user":"alice","token":"a"},{"backend":"ws://127.0.0.1:7790/ws","user":"bob","token":"b2","session":"s7"}]
    prigh.user = -
    prigh.token = -
    |}];
  Accounts.remove storage { bob with session = None };
  print_s
    [%sexp (List.map (Accounts.load storage) ~f:Account.name : string list)];
  [%expect {| (alice) |}]
;;

let%expect_test "accounts without users, and a corrupt list" =
  let storage = Accounts.Storage.in_memory () in
  storage.set Accounts.key "{not json";
  print_s [%sexp (Accounts.load storage : Account.t list)];
  storage.set "prigh.token" "sekrit";
  let accounts = Accounts.remember storage ~backend in
  print_s
    [%sexp
      (List.map accounts ~f:(fun a -> Account.name a, Account.host a)
       : (string * string) list)];
  print_s
    [%sexp
      (Account.same
         { backend; user = None; token = Some "sekrit"; session = None }
         { backend; user = None; token = Some "other"; session = None }
       : bool)];
  [%expect
    {|
    ()
    ((token 127.0.0.1:7790))
    false
    |}];
  (* Nothing signed in: nothing to remember. *)
  let storage = Accounts.Storage.in_memory () in
  print_s [%sexp (Accounts.remember storage ~backend : Account.t list)];
  print_s [%sexp (Accounts.current storage ~backend : Account.t option)];
  [%expect
    {|
    ()
    ()
    |}]
;;

let account ?(backend = backend) ?session user =
  { Account.backend
  ; user = Some user
  ; token = Some (String.prefix user 1)
  ; session
  }
;;

let alice = account "alice"
let bob = account ~session:"b7" "bob"

let signed_in_as ?(users = false) user =
  let h = H.create () in
  H.act h (Hello { client_id = "c1"; namespace = Some user; user = Some user });
  H.act
    h
    (Set_accounts
       { accounts = [ alice; bob; account ~backend:other_backend "carol" ]
       ; current = Some (account user)
       });
  if users
  then H.act h (Reply (Users Probe, Ok (Jsonaf.of_string {|["alice","bob"]|})));
  h
;;

let%expect_test "the account menu: who we are, the other accounts, actions" =
  let h = signed_in_as "alice" in
  H.text h ~selector:".sidebar-footer";
  H.text h ~selector:".status-item.account";
  [%expect
    {|
    (A alice) (Commands and keys (/help))
    (alice)
    |}];
  H.act h Open_accounts;
  H.text h ~selector:".account-menu";
  [%expect
    {|
    (Focus dialog)
    A
    alice 127.0.0.1:7790
    signed in
    Switch to
    B
    bob 127.0.0.1:7790
    C
    carol prigh.example.com
    Add account… sign in as another user, or to another backend
    Sign out forget this account in this browser
    ↑↓ move · Enter choose · Esc close
    |}];
  (* Enter on the highlighted account switches to it (the page reloads). *)
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    (Switch_account
     ((backend ws://127.0.0.1:7790/ws) (user (bob)) (token (b)) (session (b7))))
    |}];
  H.act h Open_accounts;
  H.key h "ArrowDown" ~target:Page;
  H.key h "ArrowDown" ~target:Page;
  H.key h "Enter" ~target:Page;
  H.act h Open_accounts;
  H.act h (Picker_choose "signout");
  [%expect
    {|
    (Focus dialog)
    (Dialog_move 1)
    (Dialog_move 1)
    Dialog_accept
    (Focus editor)
    Add_account
    (Focus dialog)
    (Focus editor)
    Sign_out
    |}];
  (* Esc closes it without doing anything. *)
  H.act h Open_accounts;
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    (Focus dialog)
    Close_dialog
    (Focus editor)
    |}]
;;

let%expect_test "a superuser acts as another user from the account menu" =
  let h = H.create () in
  H.act
    h
    (Hello { client_id = "c1"; namespace = Some "alice"; user = Some "alice" });
  H.act h Start;
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_users) (params ()) (tag (Users Probe)))
    |}];
  H.reply h "list_users" {|["alice","bob"]|};
  H.act h Open_accounts;
  H.text h ~selector:".account-menu";
  [%expect
    {|
    (Focus dialog)
    A
    alice
    signed in
    Act as… see and work in another user's sessions
    Add account… sign in as another user, or to another backend
    Sign out forget this account in this browser
    ↑↓ move · Enter choose · Esc close
    |}];
  H.act h (Picker_choose "act");
  H.reply h "list_users" {|["alice","bob"]|};
  H.act h (Picker_choose "bob");
  H.reply h "set_user" {|{"client_id":"c1","namespace":"bob","user":"alice"}|};
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ list_users) (params ()) (tag (Users Picker)))
    (Focus picker-input)
    (Focus editor)
    (Rpc (method_ set_user) (params ((user bob))) (tag User_switched))
    (Expire_toast (id 0) (after_ms 4000))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    |}];
  H.act h Open_accounts;
  H.text h ~selector:".account-menu";
  [%expect
    {|
    (Focus dialog)
    A
    alice acting as bob
    signed in
    Act as… now acting as bob
    Back to alice your own sessions
    Add account… sign in as another user, or to another backend
    Sign out forget this account in this browser
    ↑↓ move · Enter choose · Esc close
    |}];
  H.act h (Picker_choose "back");
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ set_user) (params ((user alice))) (tag User_switched))
    |}];
  (* Others are not offered acting as anyone. *)
  let h = H.create () in
  H.act
    h
    (Hello { client_id = "c1"; namespace = Some "bob"; user = Some "bob" });
  H.act h Start;
  H.fail h "list_users" "unauthorised: bob is not a superuser";
  H.act h Open_accounts;
  H.text h ~selector:".menu-section";
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_users) (params ()) (tag (Users Probe)))
    (Focus dialog)
    Add account… sign in as another user, or to another backend
    Sign out forget this account in this browser
    |}]
;;

let%expect_test "switching user resets everything that was the old user's" =
  let h =
    H.create ~sessions:(sprintf "[%s]" (H.session_json ~name:"Alice's" "s2")) ()
  in
  H.act
    h
    (Hello { client_id = "c1"; namespace = Some "alice"; user = Some "alice" });
  H.act
    h
    (Reply
       ( Messages "s1"
       , Ok (Jsonaf.of_string {|[{"role":"user","text":"secret"}]|}) ));
  H.act h (Run "/btw what was that?");
  H.act h (Set_session_query "ali");
  H.act h Open_model_picker;
  H.act h (Run "/setusr bob");
  H.reply h "set_user" {|{"client_id":"c1","namespace":"bob","user":"alice"}|};
  let m = H.model h in
  print_s
    [%message
      ""
        ~state:(Option.is_some m.state : bool)
        ~entries:(List.length (Chat.entries m.chat) : int)
        ~sessions:(List.length m.sessions : int)
        ~query:m.session_query
        ~dialog:(Option.is_some m.dialog : bool)
        ~btw:(Option.is_some m.btw : bool)];
  [%expect
    {|
    Follow_chat
    (Rpc (method_ btw) (params ((question "what was that?") (btw_id btw-1)))
     (tag (Btw btw-1)))
    (Focus picker-input)
    (Rpc (method_ set_user) (params ((user bob))) (tag User_switched))
    (Rpc (method_ btw_cancel) (params ((btw_id btw-1))) (tag Ignore))
    (Expire_toast (id 0) (after_ms 4000))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    ((state false) (entries 0) (sessions 0) (query "") (dialog false)
     (btw false))
    |}]
;;

let%expect_test "token-only backends show the account too" =
  let h = H.create () in
  H.act h Saved_login;
  H.act
    h
    (Set_accounts
       { accounts =
           [ { backend; user = None; token = Some "t"; session = None } ]
       ; current =
           Some { backend; user = None; token = Some "t"; session = None }
       });
  H.text h ~selector:".sidebar-footer";
  [%expect {| (T token) (Commands and keys (/help)) |}]
;;
