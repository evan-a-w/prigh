open! Core
open! Prigh
module Json = Jsonaf

(* A tool host with files in memory: [bash] answers the locate command as
   the real one would for [home] ([keychain]: macOS's keychain has Claude
   Code's login). *)
module Host = struct
  type t =
    { files : string String.Table.t
    ; mutable keychain : bool
    ; mutable keyring : string option
    ; mutable keyring_line : string option
      (** what the keyring probe says, when not derived from [keyring] *)
    ; mutable lookup_error : string option
    ; mutable calls : string list
    }

  let create files =
    { files = String.Table.of_alist_exn files
    ; keychain = false
    ; keyring = None
    ; keyring_line = None
    ; lookup_error = None
    ; calls = []
    }
  ;;

  let field json key =
    match json with
    | `Object fields -> List.Assoc.find fields ~equal:String.equal key
    | _ -> None
  ;;

  let str json key =
    match field json key with
    | Some (`String s) -> s
    | _ -> failwithf "no %s" key ()
  ;;

  let exec t ~name args : Tool.Result.t =
    t.calls <- name :: t.calls;
    match name with
    | "bash" ->
      let command = str args "command" in
      let has substring = String.is_substring command ~substring in
      let locate =
        has "antigravity-oauth-token"
        || has "auth.json"
        || has ".credentials.json"
      in
      if (not locate) && has "st lookup service gemini username antigravity"
      then (
        match t.lookup_error, t.keyring with
        | Some e, _ -> Tool.Result.error e
        | None, Some s -> Tool.Result.ok s
        | None, None -> Tool.Result.error "exit 1")
      else if (not locate) && has "st clear service gemini username antigravity"
      then (
        t.keyring <- None;
        Tool.Result.ok "")
      else if String.is_prefix command ~prefix:"mv -f"
      then (
        let from = "/home/u/.gemini/antigravity-cli/antigravity-oauth-token" in
        match Hashtbl.find_and_remove t.files from with
        | None -> Tool.Result.error "mv: cannot stat: No such file or directory"
        | Some _ ->
          Hashtbl.set
            t.files
            ~key:(from ^ ".moved-to-prigh")
            ~data:"{\"moved_to_prigh\":true}";
          Tool.Result.ok "")
      else (
        let path =
          if has "antigravity-oauth-token"
          then "/home/u/.gemini/antigravity-cli/antigravity-oauth-token"
          else if has "CODEX_HOME"
          then "/home/u/.codex/auth.json"
          else "/home/u/.claude/.credentials.json"
        in
        Tool.Result.ok
          (sprintf
             "%s\n%s\n%s\n"
             path
             (if Hashtbl.mem t.files path then "file" else "none")
             (if t.keychain
              then "keychain"
              else if not (has "antigravity-oauth-token")
              then ""
              else (
                match t.keyring_line, t.keyring with
                | Some line, _ -> line
                | None, Some _ ->
                  "keyring secret-tool\n\
                   client 1-bogus.apps.googleusercontent.com\n\
                   client GOCSPX-bogus"
                | None, None -> "keyring-empty"))))
    | "$fs_read" ->
      (match Hashtbl.find t.files (str args "path") with
       | None -> Tool.Result.error "No such file or directory"
       | Some data ->
         Tool.Result.ok
           (Json.to_string
              (`Object
                  [ "size", `Number (Int.to_string (String.length data))
                  ; "data", `String (Base64.encode_string data)
                  ; "eof", `True
                  ])))
    | "$fs_rename" ->
      let from = str args "from" in
      (match Hashtbl.find_and_remove t.files from with
       | None -> Tool.Result.error "No such file or directory"
       | Some data ->
         Hashtbl.set t.files ~key:(str args "to") ~data;
         Tool.Result.ok "{}")
    | _ -> Tool.Result.error ("unknown host tool " ^ name)
  ;;

  let show t =
    print_s
      [%sexp
        (Hashtbl.keys t.files |> List.sort ~compare:String.compare
         : string list)]
  ;;
end

let codex_file =
  {|{"OPENAI_API_KEY":null,"tokens":{"id_token":"i","access_token":"host-access","refresh_token":"host-refresh","account_id":"acct-1"},"last_refresh":"2026-11-01T00:00:00Z"}|}
;;

let claude_file =
  {|{"claudeAiOauth":{"accessToken":"sk-ant-oat01-host","refreshToken":"sk-ant-ort01-host","expiresAt":1,"scopes":["user:inference"],"subscriptionType":"max"}}|}
;;

let refreshed = ref []

let refresh provider ~refresh_token =
  refreshed := (provider, refresh_token) :: !refreshed;
  Ok
    { Credential.Oauth.access = "prigh-access"
    ; refresh = "prigh-refresh"
    ; expires_ms = 1_000
    ; account_id = None
    }
;;

let with_store f =
  Tool_test_helpers.with_sandbox (fun t ->
    f (Auth_store.create ~path:(t.dir ^/ "auth.json")))
;;

let show_find host adapter =
  match Cli_import.find ~exec:(Host.exec host) ~adapter ~host:"laptop" () with
  | Ok found ->
    print_s [%sexp (found : Cli_import.Found.t)];
    print_endline (Cli_import.Found.warning found)
  | Error e -> print_endline (Error.to_string_hum e)
;;

let%expect_test "finding a host's login reads it and changes nothing" =
  let host =
    Host.create
      [ "/home/u/.codex/auth.json", codex_file
      ; "/home/u/.claude/.credentials.json", claude_file
      ]
  in
  show_find host "codex";
  show_find host "claude";
  Host.show host;
  [%expect
    {|
    ((adapter codex) (provider Openai_codex) (host laptop)
     (path /home/u/.codex/auth.json) (credential <opaque>) (keyring false)
     (client ()))
    Codex on laptop will stop being logged in: prigh takes its login from /home/u/.codex/auth.json (renamed to /home/u/.codex/auth.json.moved-to-prigh) and refreshes it, which retires the copy there. Its sessions there then run on prigh's login.
    ((adapter claude) (provider Anthropic) (host laptop)
     (path /home/u/.claude/.credentials.json) (credential <opaque>)
     (keyring false) (client ()))
    Claude Code on laptop will stop being logged in: prigh takes its login from /home/u/.claude/.credentials.json (renamed to /home/u/.claude/.credentials.json.moved-to-prigh) and refreshes it, which retires the copy there. Its sessions there then run on prigh's login.
    (/home/u/.claude/.credentials.json /home/u/.codex/auth.json)
    |}]
;;

let%expect_test "what a host without a movable login is told" =
  let host = Host.create [] in
  show_find host "codex";
  host.keychain <- true;
  show_find host "claude";
  show_find host "gemini";
  Hashtbl.set
    host.files
    ~key:"/home/u/.codex/auth.json"
    ~data:{|{"OPENAI_API_KEY":"sk-x","tokens":null}|};
  show_find host "codex";
  Hashtbl.set
    host.files
    ~key:"/home/u/.claude/.credentials.json"
    ~data:{|{"claudeAiOauth":{"accessToken":"sk-ant-oat01-long"}}|};
  show_find host "claude";
  [%expect
    {|
    Codex is not logged in on laptop (no /home/u/.codex/auth.json): log prigh in with /login openai-codex
    Claude Code on laptop keeps its login in the macOS keychain, which prigh does not read: /login anthropic logs prigh in instead
    prigh imports only the logins of: codex, claude, acp:antigravity
    Codex on laptop is logged in with an API key, not ChatGPT: add the key in prigh (/login openai) instead
    Claude Code on laptop has a long-lived token (claude setup-token), which cannot be refreshed or moved: /login anthropic logs prigh in
    |}]
;;

let%expect_test "taking a login refreshes it, stores it and renames the file" =
  with_store (fun store ->
    let host =
      Host.create
        [ "/home/u/.codex/auth.json", codex_file
        ; "/home/u/.claude/.credentials.json", claude_file
        ]
    in
    List.iter [ "codex"; "claude" ] ~f:(fun adapter ->
      let found =
        Cli_import.find ~exec:(Host.exec host) ~adapter ~host:"laptop" ()
        |> Or_error.ok_exn
      in
      let id, note =
        Cli_import.take ~exec:(Host.exec host) ~refresh store found
        |> Or_error.ok_exn
      in
      print_s
        [%sexp
          (id : Account_id.t)
        , (note : string option)
        , (Provider_accounts.list store found.provider |> Or_error.ok_exn
           : Provider_accounts.Account.t list)]);
    print_s [%sexp (List.rev !refreshed : (Provider_id.t * string) list)];
    Host.show host;
    (* The file went meanwhile: the login is prigh's all the same. *)
    let found =
      { (Cli_import.find
           ~exec:
             (Host.exec
                (Host.create [ "/home/u/.codex/auth.json", codex_file ]))
           ~adapter:"codex"
           ~host:"laptop"
           ()
         |> Or_error.ok_exn)
        with
        path = "/home/u/gone.json"
      }
    in
    print_s
      [%sexp
        (Cli_import.take ~exec:(Host.exec host) ~refresh store found
         : (Account_id.t * string option) Or_error.t)]);
  [%expect
    {|
    (main ()
     (((id main) (label "imported from laptop")
       (credential
        (Oauth
         ((access prigh-access) (refresh prigh-refresh) (expires_ms 1000)
          (account_id (acct-1)))))
       (default true) (fallback true) (shared_with ()))))
    (main ()
     (((id main) (label "imported from laptop")
       (credential
        (Oauth
         ((access prigh-access) (refresh prigh-refresh) (expires_ms 1000)
          (account_id ()))))
       (default true) (fallback true) (shared_with ()))))
    ((Openai_codex host-refresh) (Anthropic sk-ant-ort01-host))
    (/home/u/.claude/.credentials.json.moved-to-prigh
     /home/u/.codex/auth.json.moved-to-prigh)
    (Ok
     (main
      ("/home/u/gone.json on laptop could not be renamed (No such file or directory): delete it there, it no longer works")))
    |}]
;;

let%expect_test "a refused refresh moves nothing" =
  with_store (fun store ->
    let host = Host.create [ "/home/u/.codex/auth.json", codex_file ] in
    let found =
      Cli_import.find ~exec:(Host.exec host) ~adapter:"codex" ~host:"laptop" ()
      |> Or_error.ok_exn
    in
    print_s
      [%sexp
        (Cli_import.take
           ~exec:(Host.exec host)
           ~refresh:(fun _ ~refresh_token:_ ->
             Or_error.error_string "invalid_grant")
           store
           found
         : (Account_id.t * string option) Or_error.t)];
    print_s
      [%sexp
        (Provider_accounts.list store Openai_codex |> Or_error.ok_exn
         : Provider_accounts.Account.t list)];
    Host.show host);
  [%expect
    {|
    (Error
     ("Codex's login on laptop could not be refreshed, so nothing was moved"
      invalid_grant))
    ()
    (/home/u/.codex/auth.json)
    |}]
;;

(* The real command, in bash, with CODEX_HOME and CLAUDE_CONFIG_DIR. *)
let%expect_test "the locate command respects CODEX_HOME and CLAUDE_CONFIG_DIR" =
  let dir = Filename_unix.temp_dir "prigh-import" "" in
  Out_channel.write_all (dir ^/ "auth.json") ~data:"{}";
  List.iter [ "codex"; "claude" ] ~f:(fun adapter ->
    let command =
      Option.value_exn (Cli_import.For_testing.locate_command adapter)
    in
    let ic =
      Core_unix.open_process_in
        (sprintf
           "CODEX_HOME=%s CLAUDE_CONFIG_DIR=%s/c bash -c %s"
           dir
           dir
           (Filename.quote command))
    in
    let out = In_channel.input_all ic in
    ignore (Core_unix.close_process_in ic : Core_unix.Exit_or_signal.t);
    print_string (String.substr_replace_all out ~pattern:dir ~with_:"$DIR"));
  ignore (Sys_unix.command ("rm -rf " ^ dir) : int);
  [%expect
    {|
    $DIR/auth.json
    file

    $DIR/c/.credentials.json
    none
    |}]
;;

let agy_file =
  {|{"token":{"access_token":"ya29.host","token_type":"Bearer","refresh_token":"1//host-refresh","expiry":"2026-11-26T05:00:00Z"},"auth_method":"consumer"}|}
;;

let%expect_test "Antigravity: from its token file or the keyring, with consent" =
  with_store (fun store ->
    refreshed := [];
    let take host =
      match
        Cli_import.find
          ~exec:(Host.exec host)
          ~adapter:"acp:antigravity"
          ~host:"laptop"
          ()
      with
      | Error e -> print_s [%sexp (e : Error.t)]
      | Ok found ->
        print_endline (Cli_import.Found.warning found);
        print_s
          [%sexp
            (found.keyring : bool)
          , (found.client : Oauth_antigravity.Client.t option)];
        print_s
          [%sexp
            (Cli_import.take ~exec:(Host.exec host) ~refresh store found
             : (Account_id.t * string option) Or_error.t)]
    in
    let file_host =
      Host.create
        [ "/home/u/.gemini/antigravity-cli/antigravity-oauth-token", agy_file ]
    in
    take file_host;
    Host.show file_host;
    print_s
      [%sexp
        (Hashtbl.find
           file_host.files
           "/home/u/.gemini/antigravity-cli/antigravity-oauth-token.moved-to-prigh"
         : string option)];
    let keyring_host = Host.create [] in
    keyring_host.keyring <- Some agy_file;
    take keyring_host;
    print_s [%sexp (keyring_host.keyring : string option)];
    print_s [%sexp (List.rev !refreshed : (Provider_id.t * string) list)];
    print_s
      [%sexp
        (List.map
           (Provider_accounts.list store Google_antigravity |> Or_error.ok_exn)
           ~f:(fun a -> a.id, a.label)
         : (Account_id.t * string) list)];
    (* Not signed in, a work login, and the macOS keychain. *)
    take (Host.create []);
    take
      (Host.create
         [ ( "/home/u/.gemini/antigravity-cli/antigravity-oauth-token"
           , {|{"token":{"access_token":"a","refresh_token":"r"},"auth_method":"gcp"}|}
           )
         ]);
    let mac = Host.create [] in
    mac.keychain <- true;
    take mac);
  [%expect
    {|
    Antigravity on laptop will stop being logged in: prigh takes its Google login from /home/u/.gemini/antigravity-cli/antigravity-oauth-token (renamed to /home/u/.gemini/antigravity-cli/antigravity-oauth-token.moved-to-prigh, its tokens blanked, as Google does not retire other copies when prigh refreshes it). Its sessions there then run on prigh's login.
    (false ())
    (Ok (main ()))
    (/home/u/.gemini/antigravity-cli/antigravity-oauth-token.moved-to-prigh)
    ("{\"moved_to_prigh\":true}")
    Antigravity on laptop will stop being logged in: prigh takes its Google login from the keyring (service gemini, username antigravity) and clears it there, as Google does not retire other copies when prigh refreshes it. Its sessions there then run on prigh's login.
    (true ())
    (Ok (main ()))
    ()
    ((Google_antigravity 1//host-refresh) (Google_antigravity 1//host-refresh))
    ((main "imported from laptop"))
    "Antigravity on laptop has no login in the keyring or /home/u/.gemini/antigravity-cli/antigravity-oauth-token: run `agy acp --login` there, or log prigh in with /login google-antigravity"
    "Antigravity on laptop signs in with \"gcp\", not a personal Google login, which is the one prigh takes: sign agy in with Google there, or use /login google-antigravity"
    "Antigravity on laptop keeps its login in the macOS keychain, which prigh does not read: /login google-antigravity logs prigh in instead"
    |}]
;;

let%expect_test "Antigravity: what the keyring said, logged in or not" =
  let find ?logged_in ?line ?lookup_error ?keyring () =
    let host = Host.create [] in
    host.keyring <- keyring;
    host.keyring_line <- line;
    host.lookup_error <- lookup_error;
    match
      Cli_import.find
        ?logged_in
        ~exec:(Host.exec host)
        ~adapter:"acp:antigravity"
        ~host:"desktop"
        ()
    with
    | Ok found -> print_s [%sexp (found.path : string), (found.keyring : bool)]
    | Error e -> print_endline (Error.to_string_hum e)
  in
  (* Read with secret-tool, or with it through nix. *)
  find ~keyring:agy_file ();
  find ~line:"keyring nix" ~keyring:agy_file ();
  (* busctl found it, but nothing here reads it. *)
  find ~line:"keyring -" ();
  find ~line:"keyring-locked" ();
  find ~line:"keyring-empty" ();
  find ~logged_in:true ~line:"keyring-empty" ();
  find ~line:"keyring-error nobus" ();
  find ~line:"keyring-error none" ();
  find
    ~line:
      "keyring-error busctl Call failed: The name org.freedesktop.secrets was \
       not provided by any .service files"
    ();
  find
    ~logged_in:true
    ~line:
      "keyring-error busctl Call failed: The name org.freedesktop.secrets was \
       not provided by any .service files"
    ();
  find
    ~line:
      "keyring-error secret-tool Cannot autolaunch D-Bus without X11 $DISPLAY"
    ();
  (* Found, then refused when read. *)
  find ~logged_in:true ~keyring:agy_file ~lookup_error:"secret-tool: locked" ();
  [%expect
    {|
    (keyring true)
    (keyring true)
    Antigravity's login on desktop is in the keyring, but prigh needs secret-tool to read it and desktop has neither secret-tool nor nix: install libsecret there and Import again, or log prigh in with /login google-antigravity
    Antigravity's login on desktop is in a locked keyring: unlock it (log in to the desktop session there) and Import again, or log prigh in with /login google-antigravity
    Antigravity on desktop has no login in the keyring or /home/u/.gemini/antigravity-cli/antigravity-oauth-token: run `agy acp --login` there, or log prigh in with /login google-antigravity
    Antigravity is logged in on desktop, but not with a Google login prigh can find (none in the keyring or /home/u/.gemini/antigravity-cli/antigravity-oauth-token; an API key or gcloud credentials?): log prigh in with /login google-antigravity
    prigh couldn't tell whether Antigravity is logged in on desktop: its login isn't in /home/u/.gemini/antigravity-cli/antigravity-oauth-token, and prigh's tool host on desktop has no D-Bus session bus to reach the keyring (DBUS_SESSION_BUS_ADDRESS is unset and /run/user/UID/bus is missing). To import it, run prigh's tool host from the desktop session there (or log in to it) and Import again; or log prigh in with /login google-antigravity
    prigh couldn't tell whether Antigravity is logged in on desktop: its login isn't in /home/u/.gemini/antigravity-cli/antigravity-oauth-token, and neither secret-tool (libsecret) nor busctl is installed on desktop to read the keyring. To import it, install libsecret (secret-tool) there and Import again; or log prigh in with /login google-antigravity
    prigh couldn't tell whether Antigravity is logged in on desktop: its login isn't in /home/u/.gemini/antigravity-cli/antigravity-oauth-token, and secret-tool isn't installed on desktop and busctl couldn't reach the Secret Service (Call failed: The name org.freedesktop.secrets was not provided by any .service files). To import it, install libsecret (secret-tool) there, check that a keyring (GNOME Keyring, KWallet) runs, and Import again; or log prigh in with /login google-antigravity
    Antigravity is logged in on desktop, but prigh couldn't read its login there: secret-tool isn't installed on desktop and busctl couldn't reach the Secret Service (Call failed: The name org.freedesktop.secrets was not provided by any .service files). To import it, install libsecret (secret-tool) there, check that a keyring (GNOME Keyring, KWallet) runs, and Import again; or log prigh in with /login google-antigravity
    prigh couldn't tell whether Antigravity is logged in on desktop: its login isn't in /home/u/.gemini/antigravity-cli/antigravity-oauth-token, and secret-tool couldn't read the keyring on desktop (Cannot autolaunch D-Bus without X11 $DISPLAY). To import it, check that the keyring runs and is unlocked there, and Import again; or log prigh in with /login google-antigravity
    Antigravity is logged in on desktop, but prigh couldn't read its login there: secret-tool couldn't read the keyring on desktop (secret-tool: locked). To import it, check that the keyring runs and is unlocked there, and Import again; or log prigh in with /login google-antigravity
    |}]
;;

(* The real probe, in bash, with stand-ins for secret-tool, busctl and id. *)
let%expect_test "the keyring probe in bash: the bus, busctl, secret-tool" =
  let dir = Filename_unix.temp_dir "prigh-keyring" "" in
  let script =
    String.substr_replace_all
      Agy_keyring.script
      ~pattern:"/run/user/$(id -u)"
      ~with_:dir
  in
  let case ~name ?(bus = true) ?(tools = []) stubs =
    let stubs =
      sprintf
        {|have() { case " %s " in *" $1 "*) return 0;; *) return 1;; esac; }
%s|}
        (String.concat ~sep:" " tools)
        stubs
    in
    (* The prelude's [have] is replaced by the stand-in's. *)
    let script =
      String.substr_replace_first
        script
        ~pattern:{|have() { command -v "$1" >/dev/null 2>&1; }|}
        ~with_:stubs
    in
    if bus
    then Out_channel.write_all (dir ^/ "bus") ~data:""
    else (
      try Core_unix.unlink (dir ^/ "bus") with
      | _ -> ());
    let ic =
      Core_unix.open_process_in
        (sprintf
           "env -u DBUS_SESSION_BUS_ADDRESS bash -c %s"
           (Filename.quote
              (script ^ "\necho \"bus=${DBUS_SESSION_BUS_ADDRESS:-}\"")))
    in
    let out = In_channel.input_all ic in
    ignore (Core_unix.close_process_in ic : Core_unix.Exit_or_signal.t);
    printf
      "%s: %s"
      name
      (String.substr_replace_all out ~pattern:dir ~with_:"$RUN")
  in
  let search answer =
    sprintf {|busctl() { case "$*" in *SearchItems*) %s;; esac; }|} answer
  in
  case ~name:"no bus" ~bus:false ~tools:[ "busctl" ] "";
  case
    ~name:"busctl finds it, secret-tool reads"
    ~tools:[ "busctl"; "secret-tool" ]
    (search {|echo 'ao ao 1 "/org/freedesktop/secrets/collection/login/5" 0'|});
  case
    ~name:"busctl finds it, nothing reads"
    ~tools:[ "busctl" ]
    (search {|echo 'ao ao 1 "/org/freedesktop/secrets/collection/login/5" 0'|});
  case
    ~name:"locked"
    ~tools:[ "busctl" ]
    (search {|echo 'ao ao 0 1 "/org/freedesktop/secrets/collection/login/5"'|});
  case ~name:"empty" ~tools:[ "busctl" ] (search {|echo 'ao ao 0 0'|});
  case
    ~name:"no Secret Service"
    ~tools:[ "busctl" ]
    (search
       {|echo "Call failed: The name org.freedesktop.secrets was not provided"; return 1|});
  case
    ~name:"secret-tool finds it"
    ~tools:[ "secret-tool" ]
    {|secret-tool() { echo secret; }|};
  case
    ~name:"secret-tool: none"
    ~tools:[ "secret-tool" ]
    {|secret-tool() { return 1; }|};
  case
    ~name:"secret-tool fails"
    ~tools:[ "secret-tool" ]
    {|secret-tool() { echo "Cannot autolaunch D-Bus" >&2; return 1; }|};
  case ~name:"no tools" "";
  ignore (Sys_unix.command ("rm -rf " ^ dir) : int);
  [%expect
    {|
    no bus: keyring-error nobus
    bus=
    busctl finds it, secret-tool reads: keyring secret-tool
    bus=unix:path=$RUN/bus
    busctl finds it, nothing reads: keyring -
    bus=unix:path=$RUN/bus
    locked: keyring-locked
    bus=unix:path=$RUN/bus
    empty: keyring-empty
    bus=unix:path=$RUN/bus
    no Secret Service: keyring-error busctl Call failed: The name org.freedesktop.secrets was not provided
    bus=unix:path=$RUN/bus
    secret-tool finds it: keyring secret-tool
    bus=unix:path=$RUN/bus
    secret-tool: none: keyring-empty
    bus=unix:path=$RUN/bus
    secret-tool fails: keyring-error secret-tool Cannot autolaunch D-Bus
    bus=unix:path=$RUN/bus
    no tools: keyring-error none
    bus=unix:path=$RUN/bus
    |}]
;;

let%expect_test "Oauth_antigravity: token answers, agy's document, the URL" =
  let body =
    Oauth_antigravity.For_testing.credential_of_body ~now_ms:1_000_000
  in
  List.iter
    [ body
        ~refresh_token:""
        {|{"access_token":"ya29.a","expires_in":3599,"refresh_token":"1//r"}|}
    ; (* Google answers a refresh without a refresh token: the old one stays *)
      body
        ~refresh_token:"1//old"
        {|{"access_token":"ya29.b","expires_in":3599}|}
    ; body ~refresh_token:"" {|{"access_token":"ya29.c"}|}
    ; body ~refresh_token:"1//old" {|{"error":"invalid_grant"}|}
    ]
    ~f:(fun r ->
      print_s
        [%sexp
          (Result.map r ~f:(fun (c : Credential.Oauth.t) ->
             c.access, c.refresh, c.expires_ms)
           : (string * string * int) Or_error.t)]);
  let doc =
    Oauth_antigravity.agy_token_document
      ~access:"ya29.x"
      ~expires_ms:1_800_000_000_000
  in
  print_endline doc;
  print_s
    [%sexp
      (Oauth_antigravity.Agy_token.parse doc
       |> Or_error.map ~f:(fun (t : Oauth_antigravity.Agy_token.t) ->
         t.access, t.refresh, t.auth_method)
       : (string option * string option * string option) Or_error.t)];
  print_s
    [%sexp
      (Oauth_antigravity.Client.of_candidates
         [ "123456-abc.apps.googleusercontent.com" ]
       : Oauth_antigravity.Client.t option)];
  print_endline
    (Oauth_antigravity.For_testing.authorize_url
       ~client:{ id = "id-1"; secret = "s" }
       ~challenge:"ch"
       ~state:"st");
  [%expect
    {|
    (Ok (ya29.a 1//r 4599000))
    (Ok (ya29.b 1//old 4599000))
    (Error "Google's token answer has no refresh_token")
    (Error "Google's token answer has no access_token")
    {"token":{"access_token":"ya29.x","token_type":"Bearer","expiry":"2027-01-15T08:00:00.000000000Z"},"auth_method":"consumer"}
    (Ok ((ya29.x) () (consumer)))
    ()
    https://accounts.google.com/o/oauth2/auth?client_id=id-1&response_type=code&redirect_uri=https://antigravity.google/oauth-callback&scope=https://www.googleapis.com/auth/cloud-platform%20https://www.googleapis.com/auth/userinfo.email%20https://www.googleapis.com/auth/userinfo.profile%20https://www.googleapis.com/auth/cclog%20https://www.googleapis.com/auth/experimentsandconfigs%20https://www.googleapis.com/auth/aicode%20openid&code_challenge=ch&code_challenge_method=S256&state=st&access_type=offline&prompt=consent
    |}]
;;
