open! Core
open! Import

module Response = struct
  type t =
    { status : int
    ; headers : (string * string) list
    }
  [@@deriving sexp_of]

  let header t name =
    List.find_map t.headers ~f:(fun (k, v) ->
      Option.some_if (String.Caseless.equal k name) v)
  ;;
end

module Error = struct
  type t =
    | Connection_failed of string
    | Connection_lost of string
    | Cancelled
    | Timed_out
    | Stalled of Time_ns.Span.t
  [@@deriving sexp_of]

  let to_string = function
    | Connection_failed s -> "connection failed: " ^ s
    | Connection_lost s -> "connection lost mid-response: " ^ s
    | Cancelled -> "cancelled"
    | Timed_out -> "timed out"
    | Stalled span ->
      sprintf
        "timed out: no data from the server for %s"
        (Time_ns.Span.to_string_hum ~decimals:0 span)
  ;;
end

let tls_config =
  lazy
    (Mirage_crypto_rng_unix.use_default ();
     let authenticator =
       match Ca_certs.authenticator () with
       | Ok a -> a
       | Error (`Msg m) -> failwith ("no CA certificates: " ^ m)
     in
     match Tls.Config.client ~authenticator () with
     | Ok config -> config
     | Error (`Msg m) -> failwith ("bad TLS config: " ^ m))
;;

let https uri raw =
  let host =
    Option.map (Uri.host uri) ~f:(fun h ->
      Domain_name.host_exn (Domain_name.of_string_exn h))
  in
  Tls_eio.client_of_flow (force tls_config) ?host raw
;;

exception Read_failed of string

(* [idle] bounds each wait for data. Raises [Read_failed] when the
   connection breaks; [on_chunk]'s exceptions propagate. *)
let read_body body ~idle ~on_chunk =
  let buf = Cstruct.create 65536 in
  let read () =
    match idle () (fun () -> Eio.Flow.single_read body buf) with
    | n -> Some n
    | exception End_of_file -> None
    | exception
        (( Eio.Io _
         | Failure _
         | Core_unix.Unix_error _
         | Tls_eio.Tls_alert _
         | Tls_eio.Tls_failure _ ) as exn) ->
      raise (Read_failed (Net_error.to_string_hum exn))
  in
  let rec loop () =
    match read () with
    | None -> ()
    | Some n ->
      on_chunk (Cstruct.to_string buf ~len:n);
      loop ()
  in
  loop ()
;;

(* An explicit port makes the address lookup numeric: by name ("https") it
   needs /etc/services, which minimal containers lack. *)
let with_default_port uri =
  match Uri.port uri, Uri.scheme uri with
  | Some _, _ -> uri
  | None, Some "https" -> Uri.with_port uri (Some 443)
  | None, Some "http" -> Uri.with_port uri (Some 80)
  | None, _ -> uri
;;

let stream
      ~(env : Env.t)
      ?(cancel = Cancellation.never)
      ?timeout
      ?idle_timeout
      ?(on_response = ignore)
      ~url
      ~headers
      ~body
      ~on_chunk
      ()
  =
  let idle () f =
    match idle_timeout with
    | None -> f ()
    | Some span ->
      Eio.Time.with_timeout_exn
        (Eio.Stdenv.clock env)
        (Time_ns.Span.to_sec span)
        f
  in
  let request () =
    Switch.run
    @@ fun sw ->
    let client =
      Cohttp_eio.Client.make ~https:(Some https) (Eio.Stdenv.net env)
    in
    let headers = Http.Header.of_list headers in
    let uri = with_default_port (Uri.of_string url) in
    match
      idle () (fun () ->
        match body with
        | None -> Cohttp_eio.Client.get client ~sw ~headers uri
        | Some body ->
          Cohttp_eio.Client.post
            client
            ~sw
            ~headers
            ~body:(Cohttp_eio.Body.of_string body)
            uri)
    with
    | exception
        (( Eio.Io _
         | Failure _
         | Core_unix.Unix_error _
         | Tls_eio.Tls_alert _
         | Tls_eio.Tls_failure _ ) as exn) ->
      Error (Error.Connection_failed (Net_error.to_string_hum exn))
    | resp, body ->
      let response =
        { Response.status = Http.Status.to_int (Http.Response.status resp)
        ; headers = Http.Header.to_list (Http.Response.headers resp)
        }
      in
      on_response response;
      (match read_body body ~idle ~on_chunk with
       | () -> Ok response
       | exception Read_failed message -> Error (Error.Connection_lost message))
  in
  let with_timeout f =
    match timeout with
    | None -> f ()
    | Some span ->
      (match
         Eio.Time.with_timeout
           (Eio.Stdenv.clock env)
           (Time_ns.Span.to_sec span)
           (fun () -> Ok (f ()))
       with
       | Error `Timeout -> Error Error.Timed_out
       | Ok result -> result)
  in
  match Cancellation.protect cancel ~f:(fun () -> with_timeout request) with
  | None -> Error Error.Cancelled
  | Some result -> result
  | exception Eio.Time.Timeout ->
    Error (Error.Stalled (Option.value_exn idle_timeout))
;;

let post_stream
      ~env
      ?cancel
      ?timeout
      ?idle_timeout
      ?on_response
      ~url
      ~headers
      ~body
  =
  stream
    ~env
    ?cancel
    ?timeout
    ?idle_timeout
    ?on_response
    ~url
    ~headers
    ~body:(Some body)
;;

let get_stream ~env ?cancel ?timeout ?on_response ~url ~headers ~on_chunk () =
  stream
    ~env
    ?cancel
    ?timeout
    ?on_response
    ~url
    ~headers
    ~body:None
    ~on_chunk
    ()
;;

let collect ~env ?cancel ?timeout ~url ~headers ~body () =
  let buf = Buffer.create 1024 in
  Result.map
    (stream
       ~env
       ?cancel
       ?timeout
       ~url
       ~headers
       ~body
       ~on_chunk:(Buffer.add_string buf)
       ())
    ~f:(fun response -> response, Buffer.contents buf)
;;

let post ~env ?cancel ?timeout ~url ~headers ~body () =
  collect ~env ?cancel ?timeout ~url ~headers ~body:(Some body) ()
;;

let get ~env ?cancel ?timeout ~url ~headers () =
  collect ~env ?cancel ?timeout ~url ~headers ~body:None ()
;;
