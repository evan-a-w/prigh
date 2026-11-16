open! Core
open! Import

let system_tls =
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

(* A non-blocking socket waited on with [Eio_unix.await_*], so that it can
   outlive the switch of the request that connected it. *)
module Socket = struct
  type t =
    { fd : Core_unix.File_descr.t
    ; mutable closed : bool
    }

  let unix_error e name = raise (Core_unix.Unix_error (e, name, ""))

  let rec single_read t (buf : Cstruct.t) =
    if t.closed then raise End_of_file;
    match
      Core_unix.Syscall_result.Int.to_result
        (Bigstring_unix.read_assume_fd_is_nonblocking
           t.fd
           ~pos:buf.off
           ~len:buf.len
           buf.buffer)
    with
    | Ok 0 -> raise End_of_file
    | Ok n -> n
    | Error (EAGAIN | EWOULDBLOCK | EINTR) ->
      Eio_unix.await_readable t.fd;
      single_read t buf
    | Error e -> unix_error e "read"
  ;;

  let rec single_write t (bufs : Cstruct.t list) =
    match List.find bufs ~f:(fun b -> b.len > 0) with
    | None -> 0
    | Some buf ->
      if t.closed then unix_error EBADF "write";
      (match
         Bigstring_unix.write_assume_fd_is_nonblocking
           t.fd
           ~pos:buf.off
           ~len:buf.len
           buf.buffer
       with
       | n -> n
       | exception Core_unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _)
         ->
         Eio_unix.await_writable t.fd;
         single_write t bufs)
  ;;

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let read_methods = []

  let shutdown t (command : Eio.Flow.shutdown_command) =
    if not t.closed
    then (
      let mode : Core_unix.shutdown_command =
        match command with
        | `Receive -> SHUTDOWN_RECEIVE
        | `Send -> SHUTDOWN_SEND
        | `All -> SHUTDOWN_ALL
      in
      try Core_unix.shutdown t.fd ~mode with
      | Core_unix.Unix_error _ -> ())
  ;;

  let close t =
    if not t.closed
    then (
      t.closed <- true;
      try Core_unix.close t.fd with
      | Core_unix.Unix_error _ -> ())
  ;;

  (* Whether the server closed it (or sent something unasked) while it was
     idle. *)
  let looks_open t =
    (not t.closed)
    &&
    let probe = Bigstring.create 1 in
    match
      Core_unix.Syscall_result.Int.to_result
        (Bigstring_unix.read_assume_fd_is_nonblocking t.fd probe)
    with
    | Error (EAGAIN | EWOULDBLOCK) -> true
    | Ok _ | Error _ -> false
  ;;

  let handler =
    Eio.Resource.handler
      (H (Eio.Resource.Close, close)
       :: Eio.Resource.bindings
            (Eio.Flow.Pi.two_way
               (module struct
                 type nonrec t = t

                 let single_read = single_read
                 let single_write = single_write
                 let copy = copy
                 let read_methods = read_methods
                 let shutdown = shutdown
               end)))
  ;;

  let flow t : [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t =
    Eio.Resource.T (t, handler)
  ;;

  let connect ~(env : Env.t) ~host ~port =
    let addr =
      match
        Eio.Net.getaddrinfo_stream
          ~service:(Int.to_string port)
          (Eio.Stdenv.net env)
          host
      with
      | addr :: _ -> addr
      | [] -> failwithf "could not resolve %s" host ()
    in
    Switch.run
    @@ fun sw ->
    let eio = Eio.Net.connect ~sw (Eio.Stdenv.net env) addr in
    let fd =
      match Eio_unix.Resource.fd_opt eio with
      | Some fd ->
        Eio_unix.Fd.use_exn "dup" fd (fun fd ->
          Core_unix.dup ~close_on_exec:true fd)
      | None -> failwith "the connection has no file descriptor"
    in
    Core_unix.set_nonblock fd;
    { fd; closed = false }
  ;;
end

module Connection = struct
  type t =
    { key : string
    ; socket : Socket.t
    ; flow : [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t
    ; mutable reused : bool
    ; mutable idle_since : Time_ns.t
    }

  let flow t = t.flow
  let reused t = t.reused
end

type t =
  { max_idle : int
  ; max_idle_per_host : int
  ; idle_timeout : Time_ns.Span.t
  ; tls : Tls.Config.client Lazy.t
  ; idle : Connection.t list String.Table.t (** most recently used first *)
  }

let create
      ?(max_idle = 16)
      ?(max_idle_per_host = 4)
      ?(idle_timeout = Time_ns.Span.of_int_sec 50)
      ?(tls = system_tls)
      ()
  =
  { max_idle
  ; max_idle_per_host
  ; idle_timeout
  ; tls
  ; idle = String.Table.create ()
  }
;;

let default = create ()
let close (c : Connection.t) = Socket.close c.socket

let idle t =
  Hashtbl.fold t.idle ~init:0 ~f:(fun ~key:_ ~data n -> n + List.length data)
;;

let expire t =
  let now = Time_ns.now () in
  Hashtbl.filter_map_inplace t.idle ~f:(fun conns ->
    let live, expired =
      List.partition_tf conns ~f:(fun (c : Connection.t) ->
        Time_ns.Span.( < ) (Time_ns.diff now c.idle_since) t.idle_timeout)
    in
    List.iter expired ~f:close;
    if List.is_empty live then None else Some live)
;;

let key uri =
  let scheme = Option.value (Uri.scheme uri) ~default:"http" in
  let host = Option.value (Uri.host uri) ~default:"localhost" in
  let port =
    match Uri.port uri, scheme with
    | Some p, _ -> p
    | None, "https" -> 443
    | None, _ -> 80
  in
  scheme, host, port, sprintf "%s://%s:%d" scheme host port
;;

let rec take_idle t key =
  match Hashtbl.find t.idle key with
  | None | Some [] -> None
  | Some (c :: rest) ->
    if List.is_empty rest
    then Hashtbl.remove t.idle key
    else Hashtbl.set t.idle ~key ~data:rest;
    if Socket.looks_open c.socket
    then Some c
    else (
      close c;
      take_idle t key)
;;

let tls_host host =
  Result.ok (Result.bind (Domain_name.of_string host) ~f:Domain_name.host)
;;

let acquire t ~env uri =
  expire t;
  let scheme, host, port, key = key uri in
  match take_idle t key with
  | Some c ->
    c.reused <- true;
    c
  | None ->
    let flow_of socket =
      match scheme with
      | "http" -> Socket.flow socket
      | "https" ->
        (Tls_eio.client_of_flow
           (force t.tls)
           ?host:(tls_host host)
           (Socket.flow socket)
          :> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t)
      | other -> failwithf "unsupported URL scheme %S" other ()
    in
    let socket = Socket.connect ~env ~host ~port in
    (match flow_of socket with
     | flow ->
       { Connection.key
       ; socket
       ; flow
       ; reused = false
       ; idle_since = Time_ns.now ()
       }
     | exception exn ->
       Socket.close socket;
       raise exn)
;;

let oldest t =
  Hashtbl.fold t.idle ~init:None ~f:(fun ~key:_ ~data acc ->
    List.fold data ~init:acc ~f:(fun acc (c : Connection.t) ->
      match acc with
      | Some (o : Connection.t) when Time_ns.( <= ) o.idle_since c.idle_since ->
        acc
      | _ -> Some c))
;;

let remove t (c : Connection.t) =
  Hashtbl.change t.idle c.key ~f:(function
    | None -> None
    | Some conns ->
      (match List.filter conns ~f:(fun o -> not (phys_equal o c)) with
       | [] -> None
       | conns -> Some conns))
;;

let release t (c : Connection.t) =
  expire t;
  c.idle_since <- Time_ns.now ();
  let here = Option.value (Hashtbl.find t.idle c.key) ~default:[] in
  if c.socket.closed || t.max_idle_per_host <= 0 || t.max_idle <= 0
  then close c
  else (
    let here =
      if List.length here >= t.max_idle_per_host
      then (
        let keep = t.max_idle_per_host - 1 in
        List.iter (List.drop here keep) ~f:close;
        List.take here keep)
      else here
    in
    Hashtbl.set t.idle ~key:c.key ~data:(c :: here);
    if idle t > t.max_idle
    then
      Option.iter (oldest t) ~f:(fun o ->
        remove t o;
        close o))
;;
