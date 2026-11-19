open! Core
open! Prigh
open Tool_test_helpers

let getenv = function
  | "PRIGH_GEMINI_OAUTH_PROJECT" -> Some "test-project"
  | "PRIGH_GEMINI_OAUTH_VERTEX_MODEL" -> Some "gemini-test"
  | "PRIGH_GEMINI_OAUTH_CLIENT_ID" -> Some "desktop-id"
  | "PRIGH_GEMINI_OAUTH_CLIENT_SECRET" -> Some "desktop-secret"
  | "GEMINI_API_KEY" -> Some "ai-studio-only"
  | _ -> None
;;

let%expect_test "select Vertex, desktop sign-in, refresh and route inference" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let store user =
    Auth_store.create ~path:(Filename.concat t.dir (user ^ "-auth.json"))
  in
  let alice = store "alice" in
  let bob = store "bob" in
  let models =
    Model_registry.create ~env:t.env ~sw ~getenv ~home:t.dir ~store:alice ()
  in
  let login =
    Login_manager.create ~env:t.env ~sw ~getenv ~models ~store:alice ()
  in
  let provider = Provider_id.of_builtin_string "google-vertex" |> Option.value_exn in
  let status = Login_manager.status login |> Or_error.ok_exn in
  let status =
    List.find_exn status ~f:(fun (s : Provider_auth.Status.t) ->
      Provider_id.equal s.provider provider)
  in
  print_s [%sexp (status.methods : Provider_auth.Method.t list)];
  let selected =
    Model_registry.models models
    |> List.find_exn ~f:(fun (m : Model.t) -> Provider_id.equal m.provider provider)
    |> Model.key
    |> Model_registry.resolve models
    |> Or_error.ok_exn
  in
  let bad_state = ref true in
  let authorization = ref None in
  let query uri name = Option.value_exn (Uri.get_query_param uri name) in
  Login_manager.subscribe login ~f:(function
    | Auth_url { url; _ } ->
      let uri = Uri.of_string url in
      assert (String.equal (Option.value_exn (Uri.host uri)) "accounts.google.com");
      assert (String.equal (query uri "client_id") "desktop-id");
      assert (String.equal (query uri "code_challenge_method") "S256");
      assert (String.equal (query uri "access_type") "offline");
      authorization := Some uri
    | Prompt { id; prompt = Manual_code _ } ->
      let uri = Option.value_exn !authorization in
      let state = if !bad_state then "incorrect" else query uri "state" in
      let redirect = query uri "redirect_uri" in
      Login_manager.respond login ~id (redirect ^ "?code=dummy-code&state=" ^ state)
      |> Or_error.ok_exn
    | Done { provider; method_ } ->
      printf "signed in: %s %s\n" (Provider_id.to_string provider)
        (Provider_auth.Method.to_string method_)
    | Failed { error; _ } ->
      if String.is_substring error ~substring:"state"
      then print_endline "rejected wrong state"
      else failwith error
    | _ -> ());
  let exchanges = ref 0 in
  let refreshes = ref 0 in
  let inferences = ref 0 in
  let refresh_error = ref false in
  let inference_error = ref false in
  let post ~url ~headers ~body =
    let respond ?(status = 200) body =
      Ok ({ Http_client.Response.status; headers = [] }, body)
    in
    match url with
    | "https://oauth2.googleapis.com/token" ->
      let field = query (Uri.of_string ("https://invalid.test/?" ^ body)) in
      assert (String.equal (field "client_secret") "desktop-secret");
      (match field "grant_type" with
       | "authorization_code" ->
         incr exchanges;
         let uri = Option.value_exn !authorization in
         assert (String.equal (field "code") "dummy-code");
         assert (String.equal (field "redirect_uri") (query uri "redirect_uri"));
         assert (String.equal (Pkce.challenge_of_verifier (field "code_verifier"))
                   (query uri "code_challenge"));
         respond
           {|{"token_type":"Bearer","access_token":"first","refresh_token":"refresh-1","expires_in":60}|}
       | "refresh_token" ->
         incr refreshes;
         if !refresh_error
         then (
           assert (String.equal (field "refresh_token") "refresh-2");
           respond ~status:400 {|{"error":"DO-NOT-ECHO-REFRESH"}|})
         else (
           assert (String.equal (field "refresh_token") "refresh-1");
           respond
             {|{"token_type":"Bearer","access_token":"fresh","refresh_token":"refresh-2","expires_in":3600}|})
       | grant -> failwithf "unexpected grant %s" grant ())
    | "https://aiplatform.googleapis.com/v1/projects/test-project/locations/global/publishers/google/models/gemini-test:streamGenerateContent?alt=sse" ->
      incr inferences;
      let header name = List.Assoc.find headers name ~equal:String.Caseless.equal in
      assert (String.equal (Option.value_exn (header "Authorization")) "Bearer fresh");
      assert (String.equal (Option.value_exn (header "x-goog-user-project")) "test-project");
      assert (Option.is_none (header "x-goog-api-key"));
      assert (String.is_substring body ~substring:{|"text":"vertex prompt"|});
      if !inference_error
      then respond ~status:403 {|{"error":{"message":"Vertex permission denied"}}|}
      else
        respond
          ("data: "
           ^ {|{"candidates":[{"content":{"parts":[{"text":"vertex reply"}]},"finishReason":"STOP"}]}|}
           ^ "\n\n")
    | unexpected -> failwithf "unexpected outbound request %s" unexpected ()
  in
  let run_login () =
    Login_manager.start login provider Oauth |> Or_error.ok_exn;
    Login_manager.wait login
  in
  let infer store =
    let router = Provider_router.create ~env:t.env ~getenv ~models ~store () in
    router.stream
      { model = selected
      ; system = None
      ; messages = [ Message.user "vertex prompt" ]
      ; tools = []
      ; thinking = Off
      ; max_tokens = None
      ; cache_key = None
      }
      ~cancel:Cancellation.never
      ~on_event:ignore
  in
  let expect_error message hint =
    match (message : Message.Assistant.t).stop_reason with
    | Error detail when String.is_substring detail ~substring:hint ->
      assert (not (String.is_substring detail ~substring:"DO-NOT-ECHO-REFRESH"));
      print_endline hint
    | other -> raise_s [%sexp "unexpected result", (other : Stop_reason.t)]
  in
  Oauth_gemini.For_testing.with_post post
  @@ fun () ->
  run_login ();
  assert (!exchanges = 0);
  assert (Option.is_none (Auth_store.read alice provider |> Or_error.ok_exn));
  bad_state := false;
  run_login ();
  expect_error (infer bob) "/login google-vertex";
  print_endline (Message.Assistant.text (infer alice));
  let fresh =
    match Auth_store.read alice provider |> Or_error.ok_exn with
    | Some (Oauth token) -> token
    | _ -> failwith "missing Vertex credential"
  in
  printf "stored refresh: %s\n" fresh.refresh;
  inference_error := true;
  expect_error (infer alice) "403";
  Auth_store.set alice provider (Oauth { fresh with expires_ms = 0 })
  |> Or_error.ok_exn;
  refresh_error := true;
  expect_error (infer alice) "400";
  printf "code exchanges: %d, refreshes: %d, inferences: %d\n"
    !exchanges !refreshes !inferences;
  Login_manager.logout login provider |> Or_error.ok_exn;
  expect_error (infer alice) "/login google-vertex";
  let key =
    Provider_auth.resolve ~env:t.env ~getenv alice Gemini |> Or_error.ok_exn
    |> Option.value_exn
  in
  printf "AI Studio unchanged: %s\n" key.token;
  [%expect {|
    (Oauth)
    rejected wrong state
    signed in: google-vertex oauth
    /login google-vertex
    vertex reply
    stored refresh: refresh-2
    403
    400
    code exchanges: 1, refreshes: 2, inferences: 2
    /login google-vertex
    AI Studio unchanged: ai-studio-only |}]
;;
