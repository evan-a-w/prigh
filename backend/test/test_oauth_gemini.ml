open! Core
open! Prigh
module G = Oauth_gemini
module T = G.For_testing

let config ?(project = "test-project") model =
  G.Config.of_env () ~getenv:(function
    | "PRIGH_GEMINI_OAUTH_PROJECT" -> Some project
    | "PRIGH_GEMINI_OAUTH_VERTEX_MODEL" -> Some model
    | "PRIGH_GEMINI_OAUTH_CLIENT_ID" -> Some "id"
    | "PRIGH_GEMINI_OAUTH_CLIENT_SECRET" -> Some "secret"
    | _ -> None)
;;

let%expect_test "Vertex resources stay on Google's global endpoint" =
  let c = config "gemini-test" |> Or_error.ok_exn in
  print_endline (T.stream_url c);
  List.iter
    [ config "../evil"
    ; config "gemini-test?key=abc"
    ; config ~project:"../evil" "gemini-test"
    ]
    ~f:(fun x -> print_endline (Bool.to_string (Result.is_error x)));
  [%expect
    {|
    https://aiplatform.googleapis.com/v1/projects/test-project/locations/global/publishers/google/models/gemini-test:streamGenerateContent?alt=sse
    true
    true
    true |}]
;;

let%expect_test "manual input requires the exact callback and matching state" =
  let redirect_uri = T.redirect_uri 1234 in
  List.iter
    [ redirect_uri ^ "?code=ok&state=s"
    ; redirect_uri ^ "?code=ok&state=no"
    ; "http://elsewhere/?code=ok&state=s"
    ; "just-a-code"
    ]
    ~f:(fun input ->
      print_endline
        (T.manual_code ~redirect_uri ~expected_state:"s" input
         |> Result.is_ok
         |> Bool.to_string));
  [%expect
    {|
    true
    false
    false
    false |}]
;;

let%expect_test
    "refresh can omit refresh_token; reject partial grants and wrong token \
     types"
  =
  let parse extra =
    T.credential_of_body
      ~now_ms:1
      ~previous_refresh:"old"
      ("{\"access_token\":\"new\",\"expires_in\":60," ^ extra ^ "}")
  in
  let refreshed = parse {|"token_type":"Bearer"|} |> Or_error.ok_exn in
  printf "%s %d\n" refreshed.refresh refreshed.expires_ms;
  List.iter
    [ {|"token_type":"Basic"|}; {|"token_type":"Bearer","scope":"email"|} ]
    ~f:(fun extra ->
      print_endline (parse extra |> Result.is_error |> Bool.to_string));
  [%expect
    {|
    old 60001
    true
    true |}]
;;
