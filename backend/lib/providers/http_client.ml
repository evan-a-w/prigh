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

let tls_client ~host raw =
  let host =
    Result.ok (Result.bind (Domain_name.of_string host) ~f:Domain_name.host)
  in
  Tls_eio.client_of_flow (force Http_pool.system_tls) ?host raw
;;

let is_net_error = function
  | Eio.Io _
  | Failure _
  | End_of_file
  | Core_unix.Unix_error _
  | Tls_eio.Tls_alert _
  | Tls_eio.Tls_failure _ -> true
  | _ -> false
;;

let describe = function
  | Failure message -> message
  | exn -> Net_error.to_string_hum exn
;;

(* Whether the server keeps the connection open after this response, read
   to its end. *)
let keeps_alive resp =
  let headers = Http.Response.headers resp in
  Poly.equal (Http.Response.version resp) `HTTP_1_1
  && (match Http.Header.connection headers with
      | Some `Close -> false
      | Some (`Keep_alive | `Unknown _) | None -> true)
  &&
  match Cohttp.Response.has_body resp with
  | `No -> true
  | `Yes | `Unknown ->
    (match Http.Header.get_transfer_encoding headers with
     | Chunked | Fixed _ -> true
     | Unknown -> false)
;;

exception Read_failed of string
exception Stalled_exn

(* [idle] bounds each wait for data. Raises [Read_failed] when the
   connection breaks; [on_chunk]'s exceptions propagate. *)
let read_body body ~idle ~on_chunk =
  let buf = Cstruct.create 65536 in
  let read () =
    match idle (fun () -> Eio.Flow.single_read body buf) with
    | n -> Some n
    | exception End_of_file -> None
    | exception exn when is_net_error exn -> raise (Read_failed (describe exn))
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
      ?(meth = `GET)
      ?(pool = Http_pool.default)
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
  let idle f =
    match idle_timeout with
    | None -> f ()
    | Some span ->
      (match
         Eio.Time.with_timeout
           (Eio.Stdenv.clock env)
           (Time_ns.Span.to_sec span)
           (fun () -> Ok (f ()))
       with
       | Ok x -> x
       | Error `Timeout -> raise Stalled_exn)
  in
  let uri = with_default_port (Uri.of_string url) in
  let headers = Http.Header.of_list headers in
  let send conn ~sw () =
    let client =
      Cohttp_eio.Client.make_generic (fun ~sw:_ _ ->
        Http_pool.Connection.flow conn)
    in
    match meth, body with
    | `GET, None -> Cohttp_eio.Client.get client ~sw ~headers uri
    | `POST, Some body ->
      Cohttp_eio.Client.post
        client
        ~sw
        ~headers
        ~body:(Cohttp_eio.Body.of_string body)
        uri
    | meth, body ->
      Cohttp_eio.Client.call
        client
        ~sw
        ~headers
        ?body:(Option.map body ~f:Cohttp_eio.Body.of_string)
        meth
        uri
  in
  (* One exchange on [conn]. [`Retry]: it failed before any reply on a
     reused connection, which the server may have closed while it was idle,
     so the request most likely never reached it. The connection goes back
     to the pool only once its response was read to the end. *)
  let exchange conn ~retry =
    let reusable = ref false in
    let result =
      Switch.run
      @@ fun sw ->
      match
        Trace.span ~detailed:true "http_send" @@ fun () -> idle (send conn ~sw)
      with
      | exception exn when is_net_error exn ->
        if retry && Http_pool.Connection.reused conn
        then `Retry
        else `Done (Error (Error.Connection_failed (describe exn)))
      | resp, body ->
        let response =
          { Response.status = Http.Status.to_int (Http.Response.status resp)
          ; headers = Http.Header.to_list (Http.Response.headers resp)
          }
        in
        on_response response;
        (match read_body body ~idle ~on_chunk with
         | () ->
           reusable := keeps_alive resp;
           `Done (Ok response)
         | exception Read_failed message ->
           `Done (Error (Error.Connection_lost message)))
    in
    if !reusable then Http_pool.release pool conn else Http_pool.close conn;
    result
  in
  let request () =
    let rec attempt ~retry =
      match idle (fun () -> Http_pool.acquire pool ~env uri) with
      | exception exn when is_net_error exn ->
        Error (Error.Connection_failed (describe exn))
      | conn ->
        (match exchange conn ~retry with
         | `Done result -> result
         | `Retry -> attempt ~retry:false
         | exception exn ->
           Http_pool.close conn;
           raise exn)
    in
    attempt ~retry:true
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
  | exception Stalled_exn ->
    Error (Error.Stalled (Option.value_exn idle_timeout))
;;

let post_stream
      ?pool
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
    ~meth:`POST
    ?pool
    ~env
    ?cancel
    ?timeout
    ?idle_timeout
    ?on_response
    ~url
    ~headers
    ~body:(Some body)
;;

let get_stream
      ?pool
      ~env
      ?cancel
      ?timeout
      ?on_response
      ~url
      ~headers
      ~on_chunk
      ()
  =
  stream
    ?pool
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

let collect ?meth ?pool ~env ?cancel ?timeout ~url ~headers ~body () =
  let buf = Buffer.create 1024 in
  Result.map
    (stream
       ?meth
       ?pool
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

let post ?pool ~env ?cancel ?timeout ~url ~headers ~body () =
  collect
    ~meth:`POST
    ?pool
    ~env
    ?cancel
    ?timeout
    ~url
    ~headers
    ~body:(Some body)
    ()
;;

let get ?pool ~env ?cancel ?timeout ~url ~headers () =
  collect ?pool ~env ?cancel ?timeout ~url ~headers ~body:None ()
;;

let request ?pool ~env ?cancel ?timeout ~meth ~url ~headers ?body () =
  collect ~meth ?pool ~env ?cancel ?timeout ~url ~headers ~body ()
;;
