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
    ; mutable calls : string list
    }

  let create files =
    { files = String.Table.of_alist_exn files; keychain = false; calls = [] }
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
      let path =
        if String.is_substring command ~substring:"CODEX_HOME"
        then "/home/u/.codex/auth.json"
        else "/home/u/.claude/.credentials.json"
      in
      Tool.Result.ok
        (sprintf
           "%s\n%s\n%s\n"
           path
           (if Hashtbl.mem t.files path then "file" else "none")
           (if t.keychain then "keychain" else ""))
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
  match Cli_import.find ~exec:(Host.exec host) ~adapter ~host:"laptop" with
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
     (path /home/u/.codex/auth.json) (credential <opaque>))
    Codex on laptop will stop being logged in: prigh takes its login from /home/u/.codex/auth.json (renamed to /home/u/.codex/auth.json.moved-to-prigh) and refreshes it, which retires the copy there. Its sessions there then run on prigh's login.
    ((adapter claude) (provider Anthropic) (host laptop)
     (path /home/u/.claude/.credentials.json) (credential <opaque>))
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
    codex is not logged in on laptop (no /home/u/.codex/auth.json): log prigh in with /login openai-codex
    Claude Code on laptop keeps its login in the macOS keychain, which prigh does not read: /login anthropic logs prigh in instead
    prigh imports only the logins of: codex, claude
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
        Cli_import.find ~exec:(Host.exec host) ~adapter ~host:"laptop"
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
      Cli_import.find ~exec:(Host.exec host) ~adapter:"codex" ~host:"laptop"
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
