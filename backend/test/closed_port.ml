open! Core

let held = Int.Hash_set.create ()

let with_ f =
  let socket =
    Core_unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 ()
  in
  Exn.protect
    ~finally:(fun () -> Core_unix.close socket)
    ~f:(fun () ->
      Core_unix.bind socket ~addr:(ADDR_INET (Core_unix.Inet_addr.localhost, 0));
      let port =
        match Core_unix.getsockname socket with
        | ADDR_INET (_, port) -> port
        | ADDR_UNIX _ -> assert false
      in
      Hash_set.add held port;
      Exn.protect
        ~f:(fun () -> f port)
        ~finally:(fun () -> Hash_set.remove held port))
;;

let mask s =
  Hash_set.fold held ~init:s ~f:(fun s port ->
    Re.replace
      (Re.compile
         (Re.seq
            [ Re.str (sprintf ":%d" port)
            ; Re.group (Re.alt [ Re.eos; Re.compl [ Re.digit ] ])
            ]))
      ~f:(fun g -> ":CLOSED" ^ Re.Group.get g 1)
      s)
;;
