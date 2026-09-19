open! Core
open! Prigh

let show r = print_s [%sexp (r : string Or_error.t)]
let no_env _ = None

let%expect_test "env wins, then file, then a helpful error" =
  let dir = Filename_unix.temp_dir "prigh-auth" "" in
  Sys_unix.chdir dir;
  let auth_file = "auth.json" in
  show (Auth.deepseek_api_key ~env:(fun _ -> Some "sk-env") ~auth_file ());
  [%expect {| (Ok sk-env) |}];
  show (Auth.deepseek_api_key ~env:no_env ~auth_file ());
  [%expect
    {|
    (Error
     ("no DeepSeek API key: set DEEPSEEK_API_KEY or add {\"deepseek\": \"<key>\"} to"
      (file auth.json)))
    |}];
  Out_channel.write_all auth_file ~data:{|{"deepseek": "sk-file"}|};
  show (Auth.deepseek_api_key ~env:no_env ~auth_file ());
  show (Auth.deepseek_api_key ~env:(fun _ -> Some "") ~auth_file ());
  [%expect
    {|
    (Ok sk-file)
    (Ok sk-file)
    |}];
  Out_channel.write_all auth_file ~data:{|{"deepseek": 5}|};
  show (Auth.deepseek_api_key ~env:no_env ~auth_file ());
  [%expect
    {| (Error ("auth file: \"deepseek\" must be a string" (file auth.json))) |}];
  Out_channel.write_all auth_file ~data:{|not json|};
  print_s
    [%sexp
      (Or_error.is_error (Auth.deepseek_api_key ~env:no_env ~auth_file ())
       : bool)];
  [%expect {| true |}]
;;
