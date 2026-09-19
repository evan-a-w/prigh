open! Core
open! Prigh
open Eio.Std

let%expect_test "cancel is idempotent and observable" =
  Eio_main.run
  @@ fun _env ->
  let t = Cancellation.create () in
  print_s [%sexp (Cancellation.is_cancelled t : bool)];
  Cancellation.cancel t;
  Cancellation.cancel t;
  print_s [%sexp (Cancellation.is_cancelled t : bool)];
  [%expect
    {|
    false
    true
    |}]
;;

let%expect_test "protect: work finishes first, or cancellation wins" =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let t = Cancellation.create () in
  print_s [%sexp (Cancellation.protect t ~f:(fun () -> "done") : string option)];
  [%expect {| (done) |}];
  let cleanup_ran = ref false in
  let result =
    Fiber.first
      (fun () ->
         Cancellation.protect t ~f:(fun () ->
           Fun.protect
             ~finally:(fun () -> cleanup_ran := true)
             (fun () ->
                Eio.Time.sleep clock 5.;
                "never")))
      (fun () ->
         Eio.Time.sleep clock 0.01;
         Cancellation.cancel t;
         Fiber.await_cancel ())
  in
  print_s [%sexp (result : string option), (!cleanup_ran : bool)];
  [%expect {| (() true) |}];
  print_s
    [%sexp (Cancellation.protect t ~f:(fun () -> "skipped") : string option)];
  [%expect {| () |}]
;;

let%expect_test "await wakes a waiting fiber" =
  Eio_main.run
  @@ fun _env ->
  let t = Cancellation.create () in
  Fiber.both
    (fun () ->
       Cancellation.await t;
       print_endline "woken")
    (fun () ->
       print_endline "cancelling";
       Cancellation.cancel t);
  [%expect
    {|
    cancelling
    woken
    |}]
;;
