open! Core
open! Prigh
open Tool_test_helpers

let%expect_test "a fiber that raises fails neither its switch nor its siblings" =
  with_sandbox
  @@ fun _ ->
  Eio.Switch.run (fun sw ->
    Guarded.fork
      ~sw
      ~what:"a run"
      ~on_error:(fun exn -> print_s [%message "reported" (exn : exn)])
      (fun () -> failwith "bug");
    Eio.Fiber.fork ~sw (fun () -> print_endline "the sibling carries on"));
  print_endline "the switch finished";
  Guarded.run
    ~what:"x"
    ~on_error:(fun _ -> print_endline "run: reported")
    (fun () -> raise Exit);
  (* A cancellation is not an error: it goes on. *)
  (match
     Eio.Switch.run (fun sw ->
       Guarded.fork ~sw ~what:"a run" (fun () -> Eio.Fiber.await_cancel ());
       Eio.Switch.fail sw Exit)
   with
   | () -> print_endline "not cancelled"
   | exception Exit -> print_endline "cancelled with the switch");
  [%expect
    {|
    (reported (exn (Failure bug)))
    the sibling carries on
    the switch finished
    run: reported
    cancelled with the switch
    |}]
;;

(* A listening socket whose first [failures] accepts fail, like one out of
   file descriptors. *)
module Flaky = struct
  type t =
    { accept :
        sw:Eio.Switch.t
        -> [ `Generic | `Unix ] Eio.Net.stream_socket_ty Eio.Resource.t
           * Eio.Net.Sockaddr.stream
    ; addr : Eio.Net.Sockaddr.stream
    ; mutable failures : int
    }

  type tag =
    [ `Generic
    | `Unix
    ]

  let setsockopt _ _ _ = ()
  let getsockopt _ _ = failwith "unused"

  let accept t ~sw =
    if t.failures > 0
    then (
      t.failures <- t.failures - 1;
      raise (Core_unix.Unix_error (EMFILE, "accept", "")))
    else t.accept ~sw
  ;;

  let close _ = ()
  let listening_addr t = t.addr

  let create real ~failures =
    let t =
      { accept = (fun ~sw -> Eio.Net.accept ~sw real)
      ; addr = Eio.Net.listening_addr real
      ; failures
      }
    in
    Eio.Resource.T
      ( t
      , Eio.Net.Pi.listening_socket
          (module struct
            type nonrec t = t
            type nonrec tag = tag

            let setsockopt = setsockopt
            let getsockopt = getsockopt
            let accept = accept
            let close = close
            let listening_addr = listening_addr
          end) )
  ;;
end

let%expect_test "an accept loop survives failing accepts and handlers" =
  with_sandbox
  @@ fun t ->
  let net = Eio.Stdenv.net t.env in
  let clock = Eio.Stdenv.clock t.env in
  match
    Eio.Switch.run (fun sw ->
      let real =
        Eio.Net.listen
          ~sw
          ~backlog:4
          ~reuse_addr:true
          net
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
      in
      let socket = Flaky.create real ~failures:2 in
      Eio.Fiber.fork ~sw (fun () ->
        Guarded.accept_loop
          ~sw
          ~clock
          ~retry_after:0.01
          ~on_error:(fun exn -> print_s [%message "reported" (exn : exn)])
          socket
          (fun flow _ ->
             let reader = Eio.Buf_read.of_flow flow ~max_size:100 in
             match Eio.Buf_read.line reader with
             | "boom" -> failwith "boom"
             | line ->
               printf "served: %s\n" line;
               Eio.Flow.copy_string "ok\n" flow));
      let send line =
        Eio.Switch.run (fun sw ->
          let flow = Eio.Net.connect ~sw net (Eio.Net.listening_addr real) in
          Eio.Flow.copy_string (line ^ "\n") flow;
          ignore (Eio.Flow.read_all flow : string))
      in
      send "boom";
      send "hello";
      Eio.Switch.fail sw Exit)
  with
  | () | (exception Exit) ->
    [%expect
      {|
      (reported (exn (Unix.Unix_error "Too many open files" accept "")))
      (reported (exn (Unix.Unix_error "Too many open files" accept "")))
      (reported (exn (Failure boom)))
      served: hello
      |}]
;;
