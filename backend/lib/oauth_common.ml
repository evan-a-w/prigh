open! Core
open! Import

module Authorization_input = struct
  type t =
    { code : string option
    ; state : string option
    }
  [@@deriving sexp_of]

  let nonempty = function
    | Some "" -> None
    | x -> x
  ;;

  let of_query query =
    { code = nonempty (List.Assoc.find query ~equal:String.equal "code")
    ; state = nonempty (List.Assoc.find query ~equal:String.equal "state")
    }
  ;;

  let flatten_query uri =
    List.map (Uri.query uri) ~f:(fun (k, vs) -> k, String.concat ~sep:"," vs)
  ;;

  let parse input =
    let value = String.strip input in
    if String.is_empty value
    then { code = None; state = None }
    else if
      String.is_prefix value ~prefix:"http://"
      || String.is_prefix value ~prefix:"https://"
    then of_query (flatten_query (Uri.of_string value))
    else (
      match String.lsplit2 value ~on:'#' with
      | Some (code, state) ->
        { code = nonempty (Some code); state = nonempty (Some state) }
      | None ->
        if String.is_substring value ~substring:"code="
        then
          of_query
            (Uri.query_of_encoded value
             |> List.map ~f:(fun (k, vs) -> k, String.concat ~sep:"," vs))
        else { code = Some value; state = None })
  ;;
end

module Token_response = struct
  type t =
    { access_token : string
    ; refresh_token : string
    ; expires_in : int
    }
  [@@deriving sexp_of]

  let of_json json =
    let str name =
      match Json.member name json with
      | Some (`String s) -> Ok s
      | _ -> Or_error.errorf "token response: missing %S" name
    in
    let open Or_error.Let_syntax in
    let%bind access_token = str "access_token" in
    let%bind refresh_token = str "refresh_token" in
    let%map expires_in =
      match Option.bind (Json.member "expires_in" json) ~f:Json.int with
      | Some n -> Ok n
      | None -> Or_error.error_string "token response: missing expires_in"
    in
    { access_token; refresh_token; expires_in }
  ;;

  let parse body =
    match Json.parse body with
    | Error e ->
      Or_error.error_s
        [%message
          "token endpoint returned invalid JSON" (body : string) (e : Error.t)]
    | Ok json -> of_json json
  ;;

  let to_credential ?account_id ?(now_ms = Credential.now_ms ()) t =
    { Credential.Oauth.access = t.access_token
    ; refresh = t.refresh_token
    ; expires_ms = now_ms + (t.expires_in * 1000)
    ; account_id
    }
  ;;
end

let token_timeout = Time_ns.Span.of_sec 30.

let post ~env ~cancel ~url ~content_type ~body ~what =
  match
    Http_client.post
      ~env
      ~cancel
      ~timeout:token_timeout
      ~url
      ~headers:[ "Content-Type", content_type; "Accept", "application/json" ]
      ~body
      ()
  with
  | Error Cancelled -> Auth_interaction.cancelled ()
  | Error e ->
    Or_error.error_s
      [%message
        (what ^ " request failed")
          (url : string)
          ~error:(Http_client.Error.to_string e : string)]
  | Ok (response, body) when response.status / 100 <> 2 ->
    Or_error.error_s
      [%message
        (what ^ " failed")
          (url : string)
          ~status:(response.status : int)
          (body : string)]
  | Ok (_, body) -> Ok body
;;

let post_json ~env ~cancel ~url ~fields ~what =
  post
    ~env
    ~cancel
    ~url
    ~content_type:"application/json"
    ~body:
      (Json.to_string
         (`Object (List.map fields ~f:(fun (k, v) -> k, `String v))))
    ~what
;;

let post_form ~env ~cancel ~url ~fields ~what =
  post
    ~env
    ~cancel
    ~url
    ~content_type:"application/x-www-form-urlencoded"
    ~body:(Uri.encoded_of_query (List.map fields ~f:(fun (k, v) -> k, [ v ])))
    ~what
;;

let query_string params =
  Uri.encoded_of_query (List.map params ~f:(fun (k, v) -> k, [ v ]))
;;

(* Waits for either the loopback redirect or a pasted code; whichever comes
   first wins and the other is cancelled. *)
let wait_for_code
      ~(interaction : Auth_interaction.t)
      ~(server : Oauth_callback_server.t option)
      ~expected_state
      ~placeholder
  =
  let manual () =
    Or_error.bind
      (interaction.prompt
         (Manual_code
            { message =
                "Complete login in your browser, or paste the authorization \
                 code / redirect URL here:"
            ; placeholder
            }))
      ~f:(fun input ->
        let parsed = Authorization_input.parse input in
        match parsed.state with
        | Some state when not (String.equal state expected_state) ->
          Or_error.error_string "OAuth state mismatch"
        | _ ->
          (match parsed.code with
           | None -> Or_error.error_string "missing authorization code"
           | Some code -> Ok code))
  in
  match server with
  | None -> manual ()
  | Some server ->
    Fiber.first (fun () -> Ok (Oauth_callback_server.wait server).code) manual
;;
