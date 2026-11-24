open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

let store t = Auth_store.create ~path:(Filename.concat t.dir "auth.json")
let path t = Filename.concat t.dir "auth.json"

let oauth ?account_id access =
  Credential.Oauth
    { access; refresh = "r-" ^ access; expires_ms = Int.max_value; account_id }
;;

let id s = Or_error.ok_exn (Account_id.of_string s)

let show_accounts store provider =
  match Provider_accounts.list store provider with
  | Error e -> print_s [%sexp (e : Error.t)]
  | Ok accounts ->
    List.iter accounts ~f:(fun (a : Provider_accounts.Account.t) ->
      printf
        "%s%-10s %-30s %s fallback=%b\n"
        (if a.default then "*" else " ")
        (Account_id.to_string a.id)
        (sprintf "%S" a.label)
        (match a.credential with
         | Api_key k -> "key:" ^ k
         | Oauth o -> "oauth:" ^ o.access)
        a.fallback)
;;

let show_file t =
  In_channel.read_all (path t)
  |> String.split_lines
  |> List.iter ~f:(fun line ->
    print_endline
      (if String.is_substring line ~substring:"\"created_ms\""
       then
         Staged.unstage
           (String.tr_multi ~target:"0123456789" ~replacement:"N")
           line
       else line))
;;

let ok r = Or_error.ok_exn r

let%expect_test "account ids" =
  List.iter
    [ "main"; "work"; "Work"; ""; "a b"; String.make 33 'a' ]
    ~f:(fun s ->
      print_s
        [%sexp (s : string), (Account_id.of_string s : Account_id.t Or_error.t)]);
  [%expect
    {|
    (main (Ok main))
    (work (Ok work))
    (Work
     (Error
      "\"Work\" is not an account id: use 1 to 32 lowercase letters, digits and -"))
    (""
     (Error
      "\"\" is not an account id: use 1 to 32 lowercase letters, digits and -"))
    ("a b"
     (Error
      "\"a b\" is not an account id: use 1 to 32 lowercase letters, digits and -"))
    (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     (Error
      "\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\" is not an account id: use 1 to 32 lowercase letters, digits and -"))
    |}];
  let taken = [ id "work"; id "work-2" ] in
  List.iter
    [ "Work"; "Personal Pro!"; "main"; "!!!"; String.make 40 'x' ]
    ~f:(fun l ->
      print_s
        [%sexp (l : string), (Account_id.of_label l ~taken : Account_id.t)]);
  [%expect
    {|
    (Work work-3)
    ("Personal Pro!" personal-pro)
    (main main-2)
    (!!! account)
    (xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxxxx)
    |}];
  print_s
    [%sexp
      ([ Account_id.same None None
       ; Account_id.same None (Some Account_id.main)
       ; Account_id.same (Some "work") (Some (id "work"))
       ; Account_id.same (Some "work") None
       ; Account_id.same None (Some (id "work"))
       ]
       : bool list)];
  [%expect {| (true true true false false) |}]
;;

let%expect_test
    "a file from before accounts reads as main, and reading writes nothing"
  =
  with_sandbox
  @@ fun t ->
  let legacy =
    {|{"anthropic":{"type":"oauth","access":"a1","refresh":"r1","expires":99999999999999},"deepseek":{"type":"api_key","key":"sk-d"},"forge:x":{"y":1}}|}
  in
  Out_channel.write_all (path t) ~data:legacy;
  let store = store t in
  show_accounts store Anthropic;
  show_accounts store Deepseek;
  show_accounts store Openai;
  [%expect
    {|
    *main       "Anthropic (Claude Pro/Max)"   oauth:a1 fallback=true
    *main       "DeepSeek API key"             key:sk-d fallback=false
    |}];
  ignore (Provider_auth.status store : Provider_auth.Status.t list Or_error.t);
  ignore (Provider_accounts.find store Anthropic "work" : _ Or_error.t);
  print_s [%sexp (String.equal (In_channel.read_all (path t)) legacy : bool)];
  [%expect {| true |}];
  print_s
    [%sexp
      (Provider_accounts.find store Anthropic "Anthropic (claude pro/max)"
       |> Or_error.map ~f:(fun a -> a.id)
       : Account_id.t Or_error.t)];
  print_s
    [%sexp
      (Provider_accounts.find store Anthropic "work"
       |> Or_error.map ~f:(fun a -> a.id)
       : Account_id.t Or_error.t)];
  [%expect
    {|
    (Ok main)
    (Error
     "Anthropic has no account \"work\": it has Anthropic (Claude Pro/Max) (main)")
    |}]
;;

let%expect_test "add, relabel, reorder, default, fallback, remove" =
  with_sandbox
  @@ fun t ->
  let store = store t in
  let save ?account ?label c =
    print_s
      [%sexp
        (Provider_accounts.save_login ?account ?label store Openai_codex c
         : Account_id.t Or_error.t)]
  in
  save (oauth ~account_id:"acct-1" "p1");
  save ~label:"Work" (oauth ~account_id:"acct-2" "w1");
  save ~label:"Work Two" (oauth ~account_id:"acct-3" "w2");
  [%expect
    {|
    (Ok main)
    (Ok work)
    (Ok work-two)
    |}];
  show_accounts store Openai_codex;
  show_file t;
  [%expect
    {|
    *main       "OpenAI (ChatGPT Plus/Pro)"    oauth:p1 fallback=true
     work       "Work"                         oauth:w1 fallback=true
     work-two   "Work Two"                     oauth:w2 fallback=true
    {
      "openai-codex": {
        "type": "oauth",
        "access": "p1",
        "refresh": "r-p1",
        "expires": 4611686018427387903,
        "accountId": "acct-1"
      },
      "account:openai-codex:work": {
        "type": "oauth",
        "access": "w1",
        "refresh": "r-w1",
        "expires": 4611686018427387903,
        "accountId": "acct-2"
      },
      "prigh:accounts": {
        "version": 1,
        "providers": {
          "openai-codex": {
            "default": "main",
            "accounts": [
              {
                "id": "main",
                "label": "OpenAI (ChatGPT Plus/Pro)",
                "fallback": true
              },
              {
                "id": "work",
                "label": "Work",
                "fallback": true,
                "created_ms": NNNNNNNNNNNNN
              },
              {
                "id": "work-two",
                "label": "Work Two",
                "fallback": true,
                "created_ms": NNNNNNNNNNNNN
              }
            ]
          }
        }
      },
      "account:openai-codex:work-two": {
        "type": "oauth",
        "access": "w2",
        "refresh": "r-w2",
        "expires": 4611686018427387903,
        "accountId": "acct-3"
      }
    }
    |}];
  (* The same subscription logged in again replaces its token. *)
  save ~label:"again" (oauth ~account_id:"acct-2" "w1b");
  (* Re-login by label, case-insensitively; and by id. *)
  save ~label:"work two" (oauth ~account_id:"acct-3" "w2b");
  save ~account:(id "main") (oauth ~account_id:"acct-1" "p1b");
  save ~account:(id "nope") (oauth "x");
  (* Neither: the default. *)
  save (oauth ~account_id:"acct-1" "p1c");
  show_accounts store Openai_codex;
  [%expect
    {|
    (Ok work)
    (Ok work-two)
    (Ok main)
    (Error
     "OpenAI Codex (ChatGPT) has no account \"nope\": it has OpenAI (ChatGPT Plus/Pro) (main), Work (work), Work Two (work-two)")
    (Ok main)
    *main       "OpenAI (ChatGPT Plus/Pro)"    oauth:p1c fallback=true
     work       "Work"                         oauth:w1b fallback=true
     work-two   "Work Two"                     oauth:w2b fallback=true
    |}];
  let update a u =
    print_s
      [%sexp
        (Provider_accounts.update store Openai_codex (id a) u : unit Or_error.t)]
  in
  update
    "work"
    { Provider_accounts.Update.empty with
      label = Some "Job"
    ; make_default = true
    };
  update
    "work-two"
    { Provider_accounts.Update.empty with
      move_before = Some (Some (id "main"))
    ; fallback = Some false
    };
  update "main" { Provider_accounts.Update.empty with move_before = Some None };
  update "nope" { Provider_accounts.Update.empty with make_default = true };
  update "main" { Provider_accounts.Update.empty with label = Some "job" };
  show_accounts store Openai_codex;
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    (Error
     "OpenAI Codex (ChatGPT) has no account \"nope\": it has Work Two (work-two), Job (work), OpenAI (ChatGPT Plus/Pro) (main)")
    (Error
     "OpenAI Codex (ChatGPT) already has an account labelled \"job\": pick another label")
     work-two   "Work Two"                     oauth:w2b fallback=false
    *work       "Job"                          oauth:w1b fallback=true
     main       "OpenAI (ChatGPT Plus/Pro)"    oauth:p1c fallback=true
    |}];
  print_s
    [%sexp
      (Provider_accounts.remove store Openai_codex (id "work")
       : unit Or_error.t)];
  show_accounts store Openai_codex;
  print_s
    [%sexp
      (Provider_accounts.remove store Openai_codex (id "main")
       : unit Or_error.t)];
  print_s
    [%sexp
      (Provider_accounts.remove store Openai_codex (id "work-two")
       : unit Or_error.t)];
  show_accounts store Openai_codex;
  show_file t;
  [%expect
    {|
    (Ok ())
    *work-two   "Work Two"                     oauth:w2b fallback=false
     main       "OpenAI (ChatGPT Plus/Pro)"    oauth:p1c fallback=true
    (Ok ())
    (Ok ())
    {}
    |}]
;;

let%expect_test "other tools' keys survive account writes" =
  with_sandbox
  @@ fun t ->
  Out_channel.write_all
    (path t)
    ~data:
      {|{"deepseek":{"type":"api_key","key":"sk-d"},"forge:x":{"y":1},"zzz":"pi"}|};
  let store = store t in
  ok
    (Provider_accounts.save_login
       ~label:"Spare"
       store
       Deepseek
       (Api_key "sk-2"))
  |> Account_id.to_string
  |> print_endline;
  show_accounts store Deepseek;
  show_file t;
  [%expect
    {|
    spare
    *main       "DeepSeek API key"             key:sk-d fallback=false
     spare      "Spare"                        key:sk-2 fallback=false
    {
      "deepseek": {
        "type": "api_key",
        "key": "sk-d"
      },
      "forge:x": {
        "y": 1
      },
      "zzz": "pi",
      "account:deepseek:spare": {
        "type": "api_key",
        "key": "sk-2"
      },
      "prigh:accounts": {
        "version": 1,
        "providers": {
          "deepseek": {
            "default": "main",
            "accounts": [
              {
                "id": "main",
                "label": "DeepSeek API key",
                "fallback": false
              },
              {
                "id": "spare",
                "label": "Spare",
                "fallback": false,
                "created_ms": NNNNNNNNNNNNN
              }
            ]
          }
        }
      }
    }
    |}]
;;

let%expect_test "resolve, login and logout by account" =
  with_sandbox
  @@ fun t ->
  let store = store t in
  ignore
    (ok (Provider_accounts.save_login store Anthropic (oauth "a-main"))
     : Account_id.t);
  ignore
    (ok
       (Provider_accounts.save_login
          ~label:"Work"
          store
          Anthropic
          (Api_key "sk-work"))
     : Account_id.t);
  let resolve ?account () =
    print_s
      [%sexp
        (Provider_auth.resolve
           ~env:t.env
           ~getenv:(fun _ -> None)
           ?account
           store
           Anthropic
         |> Or_error.map
              ~f:
                (Option.map ~f:(fun (r : Provider_auth.Resolved.t) ->
                   r.token, r.source, r.account))
         : (string * string * Account_id.t option) option Or_error.t)]
  in
  resolve ();
  resolve ~account:(id "work") ();
  resolve ~account:(id "gone") ();
  [%expect
    {|
    (Ok ((a-main oauth (main))))
    (Ok ((sk-work "stored api key" (work))))
    (Error
     "Anthropic's account \"gone\" is not logged in (any more): /login anthropic logs it in, or pick another (main, work)")
    |}];
  ok
    (Provider_accounts.update
       store
       Anthropic
       (id "work")
       { Provider_accounts.Update.empty with make_default = true });
  resolve ();
  print_s
    [%sexp
      (Provider_auth.status ~getenv:(fun _ -> None) store
       |> Or_error.map ~f:(fun l -> List.hd_exn l)
       : Provider_auth.Status.t Or_error.t)];
  [%expect
    {|
    (Ok ((sk-work "stored api key" (work))))
    (Ok
     ((provider Anthropic) (methods (Oauth Api_key))
      (configured ((Api_key "stored api key"))) (expires_ms ()) (custom ())
      (accounts
       (((id main) (label "Anthropic (Claude Pro/Max)") (method_ Oauth)
         (source oauth) (expires_ms (4611686018427387903)) (default false)
         (fallback true))
        ((id work) (label Work) (method_ Api_key) (source "stored api key")
         (expires_ms ()) (default true) (fallback false))))))
    |}];
  print_s
    [%sexp
      (Provider_auth.login
         ~env:t.env
         ~label:"Third"
         store
         Anthropic
         Api_key
         (Auth_interaction.scripted [ "sk-3" ])
       : Account_id.t Or_error.t)];
  print_s [%sexp (Provider_auth.logout store Anthropic : unit Or_error.t)];
  print_s
    [%sexp
      (Provider_auth.logout ~account:(id "third") store Anthropic
       : unit Or_error.t)];
  show_accounts store Anthropic;
  [%expect
    {|
    (Ok third)
    (Ok ())
    (Ok ())
    *main       "Anthropic (Claude Pro/Max)"   oauth:a-main fallback=true
    |}]
;;
