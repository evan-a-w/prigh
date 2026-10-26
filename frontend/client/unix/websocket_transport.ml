open! Core
open! Async
open Prigh_client

module Refusal = struct
  type t =
    { status : string
    ; headers : (string * string) list
    }
  [@@deriving sexp_of]

  let header t name =
    List.Assoc.find t.headers ~equal:String.Caseless.equal name
  ;;

  let code t =
    match String.lsplit2 t.status ~on:' ' with
    | Some (code, _) -> Int.of_string_opt code
    | None -> Int.of_string_opt t.status
  ;;

  let is_html t =
    match header t "Content-Type" with
    | Some ct -> String.is_substring (String.lowercase ct) ~substring:"html"
    | None -> false
  ;;

  let exempt =
    "the proxy requires browser sign-in, which prigh-tui cannot do: exempt /ws \
     and /terminal from it for prigh (prigh checks its own token; see \
     DOCKER.md, \"Behind a reverse proxy\"), pass the proxy's credentials with \
     -header, or use an SSH tunnel to the backend's HOST:PORT"
  ;;

  let explain ~url ~host ~tls t =
    let status = "HTTP " ^ t.status in
    match code t, header t "Location" with
    | Some code, Some location
      when code >= 300
           && code < 400
           && (not tls)
           && String.is_prefix (String.lowercase location) ~prefix:"https://"
           && String.is_substring
                (String.lowercase location)
                ~substring:("//" ^ String.lowercase host) ->
      sprintf
        "%s redirects to HTTPS (%s to %s): connect with wss:// (or https://) \
         instead of %s"
        host
        status
        location
        url
    | Some code, Some location when code >= 300 && code < 400 ->
      sprintf
        "%s answered with a login page (%s to %s): %s%s"
        host
        status
        location
        exempt
        (if String.is_substring location ~substring:"goauthentik"
         then
           " (authentik: add ^/(ws|terminal)(\\?.*)?$ to the proxy provider's \
            Unauthenticated Paths)"
         else "")
    | Some (401 | 403 | 407), _ ->
      sprintf
        "%s refused the WebSocket (%s%s): %s"
        host
        status
        (match header t "WWW-Authenticate" with
         | Some scheme -> ", WWW-Authenticate: " ^ scheme
         | None -> "")
        exempt
    | Some 404, _ ->
      sprintf
        "%s has no WebSocket at %s (%s): check the URL (with the path \
         prigh-web is served under) and that the proxy forwards it to prigh's \
         -web port"
        host
        url
        status
    | Some (400 | 426), _ ->
      sprintf
        "%s did not take the WebSocket upgrade (%s): the proxy must pass the \
         Upgrade and Connection headers on to prigh's -web port (see \
         DOCKER.md, \"Behind a reverse proxy\")"
        host
        status
    | Some (502 | 503 | 504), _ ->
      sprintf
        "%s cannot reach prigh (%s): is prigh serve -web running, and the \
         proxy pointing at its port?"
        host
        status
    | _ when is_html t ->
      sprintf
        "%s answered with a web page (%s, %s) instead of opening the \
         WebSocket: if it is a login page, %s; otherwise check that %s goes to \
         prigh's -web port"
        host
        status
        (Option.value (header t "Content-Type") ~default:"")
        exempt
        url
    | _ ->
      sprintf
        "%s did not open the WebSocket at %s: it answered %s"
        host
        url
        status
  ;;
end

let parse_header text =
  match String.lsplit2 text ~on:':' with
  | Some (name, value)
    when (not (String.is_empty (String.strip name)))
         && not (String.exists name ~f:Char.is_whitespace) ->
    Ok (name, String.strip value)
  | _ ->
    Error
      (sprintf
         "%S is not a header: expected Name: value (e.g. 'X-Auth: ...')"
         text)
;;

let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let accept_key key =
  Base64.encode_exn
    (Digestif.SHA1.to_raw_string (Digestif.SHA1.digest_string (key ^ guid)))
;;

let random_bytes n =
  String.init n ~f:(fun _ -> Char.of_int_exn (Random.int 256))
;;

module Opcode = struct
  let text = 1
  let close = 8
  let ping = 9
  let pong = 10
end

(* Clients mask every frame. *)
let encode ~opcode payload =
  let mask = random_bytes 4 in
  let len = String.length payload in
  let buf = Buffer.create (len + 14) in
  Buffer.add_char buf (Char.of_int_exn (0x80 lor opcode));
  if len < 126
  then Buffer.add_char buf (Char.of_int_exn (0x80 lor len))
  else if len < 65536
  then (
    Buffer.add_char buf (Char.of_int_exn (0x80 lor 126));
    Buffer.add_char buf (Char.of_int_exn (len lsr 8));
    Buffer.add_char buf (Char.of_int_exn (len land 0xff)))
  else (
    Buffer.add_char buf (Char.of_int_exn (0x80 lor 127));
    for shift = 7 downto 0 do
      Buffer.add_char buf (Char.of_int_exn ((len lsr (shift * 8)) land 0xff))
    done);
  Buffer.add_string buf mask;
  String.iteri payload ~f:(fun i c ->
    Buffer.add_char
      buf
      (Char.of_int_exn (Char.to_int c lxor Char.to_int mask.[i land 3])));
  Buffer.contents buf
;;

let max_payload = 64 * 1024 * 1024

let read_exactly reader n =
  if n = 0
  then return (Some "")
  else (
    let buf = Bytes.create n in
    match%map Reader.really_read reader buf with
    | `Ok ->
      Some (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf)
    | `Eof _ -> None)
;;

module Frame = struct
  type t =
    { fin : bool
    ; opcode : int
    ; payload : string
    }
end

(* [None] at the end of the connection or on a malformed frame. *)
let read_frame reader : Frame.t option Deferred.t =
  let int_of bytes =
    String.fold bytes ~init:0 ~f:(fun acc c -> (acc lsl 8) lor Char.to_int c)
  in
  match%bind read_exactly reader 2 with
  | None -> return None
  | Some head ->
    let b0 = Char.to_int head.[0] in
    let b1 = Char.to_int head.[1] in
    let masked = b1 land 0x80 <> 0 in
    let%bind len =
      match b1 land 0x7f with
      | 126 -> read_exactly reader 2 >>| Option.map ~f:int_of
      | 127 -> read_exactly reader 8 >>| Option.map ~f:int_of
      | n -> return (Some n)
    in
    (match len with
     | None -> return None
     | Some len when len < 0 || len > max_payload -> return None
     | Some len ->
       let%bind mask =
         if masked then read_exactly reader 4 else return (Some "")
       in
       (match mask with
        | None -> return None
        | Some mask ->
          let%map payload = read_exactly reader len in
          Option.map payload ~f:(fun payload ->
            let payload =
              if String.is_empty mask
              then payload
              else
                String.mapi payload ~f:(fun i c ->
                  Char.of_int_exn
                    (Char.to_int c lxor Char.to_int mask.[i land 3]))
            in
            { Frame.fin = b0 land 0x80 <> 0; opcode = b0 land 0x0f; payload })))
;;

let read_head reader =
  let line () =
    match%map Reader.read_line reader with
    | `Ok line -> Some (String.rstrip line ~drop:(Char.equal '\r'))
    | `Eof -> None
  in
  match%bind line () with
  | None -> return None
  | Some status ->
    let rec headers acc =
      match%bind line () with
      | None | Some "" -> return (List.rev acc)
      | Some line ->
        (match String.lsplit2 line ~on:':' with
         | Some (name, value) ->
           headers ((String.strip name, String.strip value) :: acc)
         | None -> headers acc)
    in
    let%map headers = headers [] in
    Some (status, headers)
;;

let handshake (channel : Channel.t) ~url ~host ~tls ~authority ~path ~headers =
  let key = Base64.encode_exn (random_bytes 16) in
  channel.write
    (String.concat
       ([ sprintf "GET %s HTTP/1.1\r\n" path
        ; sprintf "Host: %s\r\n" authority
        ; "Upgrade: websocket\r\n"
        ; "Connection: Upgrade\r\n"
        ; sprintf "Sec-WebSocket-Key: %s\r\n" key
        ; "Sec-WebSocket-Version: 13\r\n"
        ]
        @ List.map headers ~f:(fun (name, value) ->
          sprintf "%s: %s\r\n" name value)
        @ [ "\r\n" ]));
  match%map
    Clock.with_timeout (Time_float.Span.of_sec 30.) (read_head channel.reader)
  with
  | `Timeout ->
    Or_error.errorf "%s did not answer the WebSocket upgrade within 30s" host
  | `Result None ->
    Or_error.errorf
      "%s closed the connection instead of answering the WebSocket upgrade%s"
      host
      (if tls then "" else " (is it HTTPS? then use wss:// or https://)")
  | `Result (Some (status_line, answer)) ->
    let status =
      match String.lsplit2 status_line ~on:' ' with
      | Some (version, rest) when String.is_prefix version ~prefix:"HTTP/" ->
        Some (String.strip rest)
      | _ -> None
    in
    (match status with
     | None
       when (not tls)
            && (String.is_prefix status_line ~prefix:"\021"
                || String.is_prefix status_line ~prefix:"\022") ->
       Or_error.errorf
         "%s answered in TLS: connect with wss:// (or https://) instead of %s"
         host
         url
     | None ->
       Or_error.errorf
         "%s answered %S, not HTTP: is it prigh-web's address?"
         host
         (String.prefix status_line 80)
     | Some status when String.is_prefix status ~prefix:"101" ->
       if
         Option.equal
           String.equal
           (List.Assoc.find
              answer
              ~equal:String.Caseless.equal
              "Sec-WebSocket-Accept")
           (Some (accept_key key))
       then Ok ()
       else
         Or_error.errorf
           "%s opened a WebSocket with the wrong Sec-WebSocket-Accept"
           host
     | Some status ->
       Error
         (Error.of_string
            (Refusal.explain ~url ~host ~tls { status; headers = answer })))
;;

let transport
      ?(ping_after = Time_float.Span.of_sec 20.)
      ?(timeout = Time_float.Span.of_sec 60.)
      (channel : Channel.t)
  =
  let closed = Ivar.create () in
  let lines_r, lines_w = Pipe.create () in
  let stderr_r, stderr_w = Pipe.create () in
  Pipe.close stderr_w;
  let is_open = ref true in
  let send ~opcode payload =
    if !is_open then channel.write (encode ~opcode payload)
  in
  let finish () =
    if !is_open
    then (
      is_open := false;
      channel.close ());
    Pipe.close lines_w;
    Ivar.fill_if_empty closed ()
  in
  (* The closing handshake: the server answers our close (or hangs up), so
     nothing we sent is lost to a reset; it gets a second. *)
  let closing = ref false in
  let close () =
    if !is_open && not !closing
    then (
      closing := true;
      send ~opcode:Opcode.close "\003\232";
      upon (Clock.after (Time_float.Span.of_sec 1.)) finish)
  in
  let heard = ref (Time_float.now ()) in
  let rec read_loop partial =
    match%bind read_frame channel.reader with
    | None -> return (finish ())
    | Some frame ->
      heard := Time_float.now ();
      let deliver message =
        String.split message ~on:'\n'
        |> List.iter ~f:(fun line ->
          if (not (String.is_empty line)) && not (Pipe.is_closed lines_w)
          then Pipe.write_without_pushback lines_w line)
      in
      (match frame.opcode, partial with
       | 8, _ ->
         if not !closing then send ~opcode:Opcode.close frame.payload;
         return (finish ())
       | 9, _ ->
         send ~opcode:Opcode.pong frame.payload;
         read_loop partial
       | 10, _ -> read_loop partial
       | (1 | 2), None when frame.fin ->
         if frame.opcode = Opcode.text then deliver frame.payload;
         read_loop None
       | (1 | 2), None ->
         let buf = Buffer.create (String.length frame.payload * 2) in
         Buffer.add_string buf frame.payload;
         read_loop (Some (frame.opcode, buf))
       | 0, Some (opcode, buf) ->
         Buffer.add_string buf frame.payload;
         if Buffer.length buf > max_payload
         then return (close ())
         else if frame.fin
         then (
           if opcode = Opcode.text then deliver (Buffer.contents buf);
           read_loop None)
         else read_loop partial
       | _ -> return (close ()))
  in
  don't_wait_for (read_loop None);
  let pinged = ref false in
  Clock.every'
    ~stop:(Ivar.read closed)
    (Time_float.Span.min
       (Time_float.Span.of_sec 1.)
       (Time_float.Span.scale ping_after 0.5))
    (fun () ->
       let quiet = Time_float.diff (Time_float.now ()) !heard in
       if Time_float.Span.( >= ) quiet timeout
       then finish ()
       else if Time_float.Span.( >= ) quiet ping_after
       then (
         if not !pinged then send ~opcode:Opcode.ping "";
         pinged := true)
       else pinged := false;
       return ());
  { Transport.send_line = (fun line -> send ~opcode:Opcode.text line)
  ; lines = lines_r
  ; stderr_lines = stderr_r
  ; close
  ; closed = Ivar.read closed
  }
;;

let connect
      ?authenticator
      ?(headers = [])
      ?ping_after
      ?timeout
      ~url
      ~tls
      ~host
      ~port
      ~authority
      ~path
      ()
  =
  let open Deferred.Or_error.Let_syntax in
  let%bind raw = Channel.tcp ~host ~port in
  let%bind channel =
    if tls then Tls_channel.client ?authenticator ~host raw else return raw
  in
  match%bind.Deferred
    handshake channel ~url ~host ~tls ~authority ~path ~headers
  with
  | Error _ as e ->
    channel.close ();
    Deferred.return e
  | Ok () -> return (transport ?ping_after ?timeout channel)
;;
