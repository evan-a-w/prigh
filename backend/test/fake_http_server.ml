open! Core
open Eio.Std

module Request = struct
  type t =
    { request_line : string
    ; headers : (string * string) list
    ; body : string
    }
  [@@deriving sexp_of]

  let header t name =
    List.find_map t.headers ~f:(fun (k, v) ->
      Option.some_if (String.Caseless.equal k name) v)
  ;;
end

module Reply = struct
  type t =
    { status : int
    ; headers : (string * string) list
    ; chunks : (float * string) list
    }

  let simple ?(headers = []) status body =
    { status; headers; chunks = [ 0., body ] }
  ;;
end

type t =
  { port : int
  ; requests : Request.t Queue.t
  }

let url t path = sprintf "http://127.0.0.1:%d%s" t.port path

let read_request flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:(16 * 1024 * 1024) in
  let request_line = Eio.Buf_read.line reader in
  let rec headers acc =
    match Eio.Buf_read.line reader with
    | "" -> List.rev acc
    | line ->
      (match String.lsplit2 line ~on:':' with
       | Some (k, v) -> headers ((String.strip k, String.strip v) :: acc)
       | None -> headers acc)
  in
  let headers = headers [] in
  let content_length =
    List.find_map headers ~f:(fun (k, v) ->
      if String.Caseless.equal k "content-length"
      then Some (Int.of_string v)
      else None)
    |> Option.value ~default:0
  in
  let body = Eio.Buf_read.take content_length reader in
  { Request.request_line; headers; body }
;;

let serve_one t ~clock flow ~handler =
  let request = read_request flow in
  Queue.enqueue t.requests request;
  let reply : Reply.t = handler request in
  let head =
    sprintf "HTTP/1.1 %d X\r\n" reply.status
    ^ String.concat
        (List.map (("Connection", "close") :: reply.headers) ~f:(fun (k, v) ->
           sprintf "%s: %s\r\n" k v))
    ^ "\r\n"
  in
  let write s =
    try Eio.Flow.copy_string s flow with
    | Eio.Io _ -> ()
  in
  write head;
  List.iter reply.chunks ~f:(fun (delay, data) ->
    if Float.(delay > 0.) then Eio.Time.sleep clock delay;
    write data)
;;

let start ~sw ~(env : Eio_unix.Stdenv.base) ~handler =
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let socket =
    Eio.Net.listen
      ~sw
      ~reuse_addr:true
      ~backlog:8
      net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> assert false
  in
  let t = { port; requests = Queue.create () } in
  Fiber.fork_daemon ~sw (fun () ->
    while true do
      Eio.Net.accept_fork
        ~sw
        socket
        ~on_error:(fun exn -> eprintf "fake server: %s\n" (Exn.to_string exn))
        (fun flow _addr -> serve_one t ~clock flow ~handler)
    done;
    `Stop_daemon);
  t
;;

let requests t = Queue.to_list t.requests
