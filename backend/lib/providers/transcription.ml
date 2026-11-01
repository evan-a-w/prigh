open! Core
open! Import

module Http = struct
  type t =
    ?cancel:Cancellation.t
    -> url:string
    -> headers:(string * string) list
    -> body:string
    -> unit
    -> (int * string) Or_error.t

  let timeout = Time_ns.Span.of_min 2.

  let real ~env ?cancel ~url ~headers ~body () =
    match Http_client.post ~env ?cancel ~timeout ~url ~headers ~body () with
    | Ok (response, body) -> Ok (response.status, body)
    | Error e ->
      Or_error.errorf
        "%s: %s"
        (Uri.host_with_default ~default:url (Uri.of_string url))
        (Http_client.Error.to_string e)
  ;;
end

module Audio = struct
  type t =
    { mime_type : string
    ; data : string
    }

  let max_bytes = 25 * 1024 * 1024
  let mb bytes = Float.of_int bytes /. 1024. /. 1024.

  let check_size size =
    if size = 0
    then Or_error.error_string "the recording is empty: nothing to transcribe"
    else if size > max_bytes
    then
      Or_error.errorf
        "the recording is %.1f MB, over the %.0f MB a transcription takes: \
         record a shorter one"
        (mb size)
        (mb max_bytes)
    else Ok ()
  ;;

  let essence mime_type =
    String.lsplit2 mime_type ~on:';'
    |> Option.value_map ~default:mime_type ~f:fst
    |> String.strip
    |> String.lowercase
  ;;

  let extensions =
    [ "webm", [ "audio/webm"; "video/webm" ]
    ; "ogg", [ "audio/ogg"; "audio/opus" ]
    ; "m4a", [ "audio/mp4"; "audio/m4a"; "audio/x-m4a"; "video/mp4" ]
    ; "mp3", [ "audio/mpeg"; "audio/mp3" ]
    ; "wav", [ "audio/wav"; "audio/x-wav"; "audio/wave" ]
    ]
  ;;

  let filename ~mime_type =
    let essence = essence mime_type in
    match
      List.find extensions ~f:(fun (_, types) ->
        List.mem types essence ~equal:String.equal)
    with
    | Some (extension, _) -> Ok ("speech." ^ extension)
    | None ->
      Or_error.errorf
        "cannot transcribe %s audio: record webm, ogg, mp4, mpeg or wav"
        (if String.is_empty essence then "untyped" else essence)
  ;;
end

module Target = struct
  type t =
    { url : string
    ; headers : (string * string) list
    ; model : string
    }

  (* Headers carry the key: only their names are shown. *)
  let sexp_of_t { url; headers; model } =
    [%sexp
      { url : string
      ; headers : string list = List.map headers ~f:fst
      ; model : string
      }]
  ;;
end

let openai_base_url = "https://api.openai.com/v1"
let pick = "choose another dictation model (/dictation model)"

(* The provider's name and the model's id, or what is wrong. *)
let split model =
  match String.lsplit2 model ~on:'/' with
  | Some (provider, id)
    when (not (String.is_empty provider)) && not (String.is_empty id) ->
    Ok (provider, id)
  | _ ->
    Or_error.errorf
      "dictation model %S is not provider/id: pick an OpenAI or \
       OpenAI-compatible model with /dictation model (e.g. %s)"
      model
      Config.default_dictation_model
;;

let cannot_transcribe what =
  Or_error.errorf
    "%s cannot transcribe: pick an OpenAI or OpenAI-compatible model with \
     /dictation model (e.g. %s)"
    what
    Config.default_dictation_model
;;

type provider =
  | Openai
  | Custom of Custom_provider.t

let provider ~providers name =
  match Provider_id.of_builtin_string name with
  | Some Openai -> Ok Openai
  | Some builtin -> cannot_transcribe (Provider_id.display_name builtin)
  | None ->
    (match
       List.find providers ~f:(fun (p : Custom_provider.t) ->
         String.equal p.name name)
     with
     | None ->
       Or_error.errorf
         "No transcription provider: there is no provider %S (add an \
          OpenAI-compatible one with /login custom) or %s"
         name
         pick
     | Some { api = Anthropic; _ } ->
       cannot_transcribe (sprintf "%s (an Anthropic API)" name)
     | Some ({ api = Chat | Responses; _ } as p) -> Ok (Custom p))
;;

let check_model ~providers model =
  let open Or_error.Let_syntax in
  let%bind name, _ = split model in
  let%map (_ : provider) = provider ~providers name in
  ()
;;

let bearer token = "Authorization", "Bearer " ^ token

let target ~env ?getenv ~store ~providers model =
  let open Or_error.Let_syntax in
  let%bind name, id = split model in
  let%bind provider = provider ~providers name in
  let resolve provider_id =
    Provider_auth.resolve ~env ?getenv store provider_id
    |> Or_error.tag ~tag:"auth"
  in
  match provider with
  | Openai ->
    (match%bind resolve Openai with
     | None ->
       Or_error.errorf
         "No transcription provider: sign in to openai with an API key (/login \
          openai) or %s"
         pick
     | Some auth ->
       Ok
         { Target.url = openai_base_url ^ "/audio/transcriptions"
         ; headers = [ bearer auth.token ]
         ; model = id
         })
  | Custom p ->
    let%map auth = resolve (Custom_provider.provider_id p) in
    { Target.url = p.base_url ^ "/audio/transcriptions"
    ; headers =
        Option.value_map
          auth
          ~default:[]
          ~f:(fun (r : Provider_auth.Resolved.t) -> [ bearer r.token ])
        @ p.headers
    ; model = id
    }
;;

let multipart_body ~boundary ~model ~filename (audio : Audio.t) =
  let field name value =
    sprintf
      "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s\r\n"
      boundary
      name
      value
  in
  String.concat
    [ field "model" model
    ; field "response_format" "json"
    ; sprintf
        "--%s\r\n\
         Content-Disposition: form-data; name=\"file\"; filename=\"%s\"\r\n\
         Content-Type: %s\r\n\
         \r\n"
        boundary
        filename
        (Audio.essence audio.mime_type)
    ; audio.data
    ; sprintf "\r\n--%s--\r\n" boundary
    ]
;;

let random_boundary () =
  let state = Random.State.make_self_init ~allow_in_tests:true () in
  "prigh-"
  ^ String.init 32 ~f:(fun _ -> "0123456789abcdef".[Random.State.int state 16])
;;

let transcribe
      ~env
      ?getenv
      ?cancel
      ?boundary
      ~(http : Http.t)
      ~store
      ~providers
      ~model
      (audio : Audio.t)
  =
  let open Or_error.Let_syntax in
  let%bind () = Audio.check_size (String.length audio.data) in
  let%bind filename = Audio.filename ~mime_type:audio.mime_type in
  let%bind target = target ~env ?getenv ~store ~providers model in
  let boundary = Option.value_or_thunk boundary ~default:random_boundary in
  let%bind status, body =
    http
      ?cancel
      ~url:target.url
      ~headers:
        (("Content-Type", "multipart/form-data; boundary=" ^ boundary)
         :: target.headers)
      ~body:(multipart_body ~boundary ~model:target.model ~filename audio)
      ()
    |> Or_error.tag ~tag:(sprintf "transcription with %s failed" model)
  in
  if status < 200 || status >= 300
  then (
    let hint =
      match status with
      | 401 | 403 ->
        sprintf
          " (the server refused the API key: /login %s changes it, or %s)"
          (fst (String.lsplit2_exn model ~on:'/'))
          pick
      | 404 ->
        sprintf
          " (is %s a transcription model? %s)"
          target.model
          (String.capitalize pick)
      | _ -> ""
    in
    Or_error.errorf
      "transcription with %s failed: %s%s"
      model
      (Sse_request.error_message_of_body ~status body)
      hint)
  else (
    match Json.parse body with
    | Ok json ->
      (match Json.member "text" json with
       | Some (`String text) -> Ok (String.strip text)
       | _ ->
         Or_error.errorf
           "transcription with %s failed: the response has no text: %s"
           model
           (String.prefix body 200))
    | Error _ ->
      Or_error.errorf
        "transcription with %s failed: the response is not JSON: %s"
        model
        (String.prefix body 200))
;;
