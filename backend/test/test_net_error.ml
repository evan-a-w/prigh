open! Core
open! Prigh

let%expect_test "peer_gone: the other end closed the connection" =
  let unix error = Eio_unix.Unix_error (error, "write", "") in
  List.iter
    [ "reset", Eio.Net.err (Connection_reset (unix ECONNRESET))
    ; "write to a closed socket", Eio.Net.err (Connection_reset (unix EPIPE))
    ; "unix reset", Core_unix.Unix_error (ECONNRESET, "read", "")
    ; "refused", Eio.Net.err (Connection_failure (Refused (unix ECONNREFUSED)))
    ; "too many files", Core_unix.Unix_error (EMFILE, "accept", "")
    ; "other", Failure "boom"
    ]
    ~f:(fun (name, exn) ->
      printf
        "%s: %b (%s)\n"
        name
        (Net_error.peer_gone exn)
        (Net_error.to_string_hum exn));
  [%expect
    {|
    reset: true (connection reset by peer)
    write to a closed socket: true (broken pipe)
    unix reset: true (connection reset by peer)
    refused: false (connection refused)
    too many files: false (too many open files)
    other: false ((Failure boom))
    |}]
;;
