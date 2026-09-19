open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

let show_credential r = print_s [%sexp (r : Credential.t Or_error.t)]

let%expect_test "credential json: api key, oauth, legacy string, errors" =
  let round_trip s =
    let c = Credential.of_json (Json.of_string s) in
    show_credential c;
    Result.iter c ~f:(fun c ->
      print_endline (Json.to_string (Credential.to_json c)))
  in
  round_trip {|{"type":"api_key","key":"sk-1"}|};
  [%expect
    {|
    (Ok (Api_key sk-1))
    {"type":"api_key","key":"sk-1"}
    |}];
  round_trip
    {|{"type":"oauth","access":"a","refresh":"r","expires":1700000000000,"accountId":"acct"}|};
  [%expect
    {|
    (Ok
     (Oauth
      ((access a) (refresh r) (expires_ms 1700000000000) (account_id (acct)))))
    {"type":"oauth","access":"a","refresh":"r","expires":1700000000000,"accountId":"acct"}
    |}];
  round_trip {|{"type":"oauth","access":"a","refresh":"r","expires":1.7e12}|};
  [%expect
    {|
    (Ok
     (Oauth ((access a) (refresh r) (expires_ms 1700000000000) (account_id ()))))
    {"type":"oauth","access":"a","refresh":"r","expires":1700000000000}
    |}];
  round_trip {|"sk-legacy"|};
  [%expect
    {|
    (Ok (Api_key sk-legacy))
    {"type":"api_key","key":"sk-legacy"}
    |}];
  round_trip {|{"type":"oauth","access":"a"}|};
  round_trip {|{"type":"magic"}|};
  round_trip {|[1]|};
  [%expect
    {|
    (Error "credential: missing string \"refresh\"")
    (Error "credential: unknown type \"magic\"")
    (Error "credential must be an object")
    |}]
;;

let%expect_test "oauth_needs_refresh uses a five minute margin" =
  let o =
    { Credential.Oauth.access = "a"
    ; refresh = "r"
    ; expires_ms = 1_000_000
    ; account_id = None
    }
  in
  List.iter [ 0; 699_999; 700_000; 2_000_000 ] ~f:(fun now_ms ->
    printf "%d -> %b\n" now_ms (Credential.oauth_needs_refresh ~now_ms o));
  [%expect
    {|
    0 -> false
    699999 -> false
    700000 -> true
    2000000 -> true
    |}]
;;

let%expect_test "store: set/read/list/remove, unknown providers preserved" =
  with_sandbox
  @@ fun t ->
  let path = Filename.concat t.dir "cfg/auth.json" in
  let store = Auth_store.create ~path in
  print_s
    [%sexp
      (Auth_store.list store : (Provider_id.t * Credential.t) list Or_error.t)];
  [%expect {| (Ok ()) |}];
  Or_error.ok_exn (Auth_store.set store Deepseek (Api_key "sk-ds"));
  Or_error.ok_exn
    (Auth_store.set
       store
       Anthropic
       (Oauth
          { access = "acc"
          ; refresh = "ref"
          ; expires_ms = 42
          ; account_id = None
          }));
  print_string (In_channel.read_all path);
  [%expect
    {|
    {
      "deepseek": {
        "type": "api_key",
        "key": "sk-ds"
      },
      "anthropic": {
        "type": "oauth",
        "access": "acc",
        "refresh": "ref",
        "expires": 42
      }
    }
    |}];
  printf "%o\n" ((Core_unix.stat path).st_perm land 0o777);
  [%expect {| 600 |}];
  print_s
    [%sexp (Auth_store.read store Deepseek : Credential.t option Or_error.t)];
  print_s
    [%sexp (Auth_store.read store Openai : Credential.t option Or_error.t)];
  [%expect
    {|
    (Ok ((Api_key sk-ds)))
    (Ok ())
    |}];
  (* Hand-edited file with a foreign provider and the legacy string form. *)
  Out_channel.write_all
    path
    ~data:{|{"deepseek": "sk-legacy", "groq": {"type":"api_key","key":"g"}}|};
  print_s
    [%sexp
      (Auth_store.list store : (Provider_id.t * Credential.t) list Or_error.t)];
  [%expect {| (Ok ((Deepseek (Api_key sk-legacy)))) |}];
  Or_error.ok_exn (Auth_store.set store Openai (Api_key "sk-oa"));
  Or_error.ok_exn (Auth_store.remove store Deepseek);
  print_string (In_channel.read_all path);
  [%expect
    {|
    {
      "groq": {
        "type": "api_key",
        "key": "g"
      },
      "openai": {
        "type": "api_key",
        "key": "sk-oa"
      }
    }
    |}];
  (* modify sees the current value and can leave it unchanged. *)
  let mtime () = (Core_unix.stat path).st_mtime in
  let before = mtime () in
  let result =
    Auth_store.modify store Openai ~f:(fun current ->
      print_s [%sexp (current : Credential.t option)];
      Ok current)
  in
  print_s [%sexp (result : Credential.t option Or_error.t)];
  printf "rewritten: %b\n" Float.(mtime () <> before);
  [%expect
    {|
    ((Api_key sk-oa))
    (Ok ((Api_key sk-oa)))
    rewritten: false
    |}];
  Out_channel.write_all path ~data:"not json";
  print_s [%sexp (Or_error.is_error (Auth_store.list store) : bool)];
  Out_channel.write_all path ~data:{|{"openai": {"type":"oauth"}}|};
  print_endline
    (mask
       t
       (Sexp.to_string_hum
          [%sexp
            (Auth_store.read store Openai : Credential.t option Or_error.t)]));
  [%expect
    {|
    true
    (Error
     ("auth file: bad credential"
      (file $DIR/cfg/auth.json)
      (provider Openai) (e "credential: missing string \"access\"")))
    |}]
;;
