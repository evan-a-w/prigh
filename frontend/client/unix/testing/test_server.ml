open! Core
open! Async
open Prigh_client_unix

type t = (Socket.Address.Inet.t, int) Tcp.Server.t

let start ?tls handle =
  Tcp.Server.create
    ~on_handler_error:`Ignore
    (Tcp.Where_to_listen.bind_to Localhost On_port_chosen_by_os)
    (fun _addr reader writer ->
       let raw = Channel.of_reader_writer reader writer in
       match tls with
       | None -> handle raw
       | Some config ->
         (match%bind Tls_channel.server config raw with
          | Ok channel -> handle channel
          | Error _ -> return ()))
;;

let port = Tcp.Server.listening_on
let close t = Tcp.Server.close t

let tls_proxy config ~port =
  start ~tls:config (fun (front : Channel.t) ->
    match%bind Channel.tcp ~host:"127.0.0.1" ~port with
    | Error _ -> return ()
    | Ok back ->
      let pump (from : Channel.t) (to_ : Channel.t) =
        let buf = Bytes.create 16384 in
        let rec loop () =
          match%bind Reader.read from.reader buf with
          | `Eof -> return (to_.close ())
          | `Ok n ->
            to_.write (Bytes.To_string.sub buf ~pos:0 ~len:n);
            loop ()
        in
        loop ()
      in
      let%map () = pump front back
      and () = pump back front in
      ())
;;

module Websocket = struct
  module Request = struct
    type t =
      { line : string
      ; headers : (string * string) list
      }
    [@@deriving sexp_of]

    let show t =
      [%sexp
        { line = (t.line : string)
        ; headers =
            (List.filter t.headers ~f:(fun (name, _) ->
               not (String.Caseless.equal name "Sec-WebSocket-Key"))
             : (string * string) list)
        }]
    ;;
  end

  let read_request (channel : Channel.t) =
    let line () =
      match%map Reader.read_line channel.reader with
      | `Ok line -> Some (String.rstrip line ~drop:(Char.equal '\r'))
      | `Eof -> None
    in
    match%bind line () with
    | None -> return None
    | Some first ->
      let rec headers acc =
        match%bind line () with
        | None | Some "" -> return (List.rev acc)
        | Some h ->
          (match String.lsplit2 h ~on:':' with
           | Some (n, v) -> headers ((String.strip n, String.strip v) :: acc)
           | None -> headers acc)
      in
      let%map headers = headers [] in
      Some { Request.line = first; headers }
  ;;

  let accept (channel : Channel.t) (request : Request.t) =
    let key =
      List.Assoc.find_exn
        request.headers
        ~equal:String.Caseless.equal
        "Sec-WebSocket-Key"
    in
    let accept =
      Base64.encode_exn
        (Digestif.SHA1.to_raw_string
           (Digestif.SHA1.digest_string
              (key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))
    in
    channel.write
      (sprintf
         "HTTP/1.1 101 Switching Protocols\r\n\
          Upgrade: websocket\r\n\
          Connection: Upgrade\r\n\
          Sec-WebSocket-Accept: %s\r\n\
          \r\n"
         accept)
  ;;

  let refuse (channel : Channel.t) ~status ~headers ~body =
    channel.write
      (String.concat
         ((sprintf "HTTP/1.1 %s\r\n" status
           :: List.map headers ~f:(fun (n, v) -> sprintf "%s: %s\r\n" n v))
          @ [ sprintf "Content-Length: %d\r\n\r\n" (String.length body); body ]
         ));
    channel.close ()
  ;;

  module Frame = struct
    type t =
      { fin : bool
      ; opcode : int
      ; masked : bool
      ; payload : string
      }
    [@@deriving sexp_of]
  end

  let send ?(fin = true) (channel : Channel.t) ~opcode payload =
    let len = String.length payload in
    let b0 = (if fin then 0x80 else 0) lor opcode in
    let head =
      if len < 126
      then String.of_char_list [ Char.of_int_exn b0; Char.of_int_exn len ]
      else
        String.of_char_list
          [ Char.of_int_exn b0
          ; Char.of_int_exn 126
          ; Char.of_int_exn (len lsr 8)
          ; Char.of_int_exn (len land 0xff)
          ]
    in
    channel.write (head ^ payload)
  ;;

  let read (channel : Channel.t) =
    let exactly n =
      let buf = Bytes.create n in
      if n = 0
      then return (Some "")
      else (
        match%map Reader.really_read channel.reader buf with
        | `Ok -> Some (Bytes.to_string buf)
        | `Eof _ -> None)
    in
    match%bind exactly 2 with
    | None -> return None
    | Some head ->
      let b0 = Char.to_int head.[0] in
      let b1 = Char.to_int head.[1] in
      let%bind len =
        match b1 land 0x7f with
        | 126 ->
          (match%map exactly 2 with
           | Some s -> Some ((Char.to_int s.[0] lsl 8) lor Char.to_int s.[1])
           | None -> None)
        | 127 ->
          (match%map exactly 8 with
           | Some s ->
             Some
               (String.fold s ~init:0 ~f:(fun acc c ->
                  (acc lsl 8) lor Char.to_int c))
           | None -> None)
        | n -> return (Some n)
      in
      let masked = b1 land 0x80 <> 0 in
      (match len with
       | None -> return None
       | Some len ->
         let%bind mask = if masked then exactly 4 else return (Some "") in
         let%map payload = exactly len in
         (match mask, payload with
          | Some mask, Some payload ->
            let payload =
              if masked
              then
                String.mapi payload ~f:(fun i c ->
                  Char.of_int_exn
                    (Char.to_int c lxor Char.to_int mask.[i land 3]))
              else payload
            in
            Some
              { Frame.fin = b0 land 0x80 <> 0
              ; opcode = b0 land 0x0f
              ; masked
              ; payload
              }
          | _ -> None))
  ;;
end
