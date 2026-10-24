open! Core
open! Expect_test_helpers_core
module S = Prigh_client_unix.Saved_connection

let none : S.Settings.t = { connect = None; token = None; user = None }

let show ?saved ?(flags = none) ?(env = none) ?(local = false) () =
  print_s [%sexp (S.resolve ~saved ~flags ~env ~local : S.Resolved.t)]
;;

let saved : S.t =
  { connect = "srv:7777"; token = Some "sekrit"; user = Some "ann" }
;;

let%expect_test "nothing saved, no flags: spawn" =
  show ();
  [%expect
    {|
    ((settings (
       (connect ())
       (token   ())
       (user    ())))
     (save ()))
    |}]
;;

let%expect_test "flags are saved" =
  show
    ~flags:
      { connect = Some "srv:7777"; token = Some "sekrit"; user = Some "ann" }
    ();
  [%expect
    {|
    ((settings (
       (connect (srv:7777))
       (token   (sekrit))
       (user    (ann))))
     (save ((
       (connect srv:7777)
       (token   sekrit)
       (user    ann)))))
    |}]
;;

let%expect_test "the saved connection is used without flags, and not resaved" =
  show ~saved ();
  [%expect
    {|
    ((settings (
       (connect (srv:7777))
       (token   (sekrit))
       (user    (ann))))
     (save ()))
    |}];
  (* Flags repeating the saved values do not rewrite the file. *)
  show ~saved ~flags:{ none with connect = Some "srv:7777" } ();
  [%expect
    {|
    ((settings (
       (connect (srv:7777))
       (token   (sekrit))
       (user    (ann))))
     (save ()))
    |}]
;;

let%expect_test "-token or -user updates the saved connection" =
  show ~saved ~flags:{ none with user = Some "bob"; token = Some "pw" } ();
  [%expect
    {|
    ((settings (
       (connect (srv:7777))
       (token   (pw))
       (user    (bob))))
     (save ((
       (connect srv:7777)
       (token   pw)
       (user    bob)))))
    |}]
;;

let%expect_test "-connect elsewhere replaces it, without the old credentials" =
  show ~saved ~flags:{ none with connect = Some "other:1" } ();
  [%expect
    {|
    ((settings (
       (connect (other:1))
       (token ())
       (user  ())))
     (save (((connect other:1)))))
    |}]
;;

let%expect_test "the environment beats the saved values but is not saved" =
  show ~saved ~env:{ none with token = Some "envtok" } ();
  [%expect
    {|
    ((settings (
       (connect (srv:7777))
       (token   (envtok))
       (user    (ann))))
     (save ()))
    |}];
  (* A different address from the environment does not get the saved
     credentials; flags given with it are saved for it. *)
  show
    ~saved
    ~env:{ none with connect = Some "env:2" }
    ~flags:{ none with user = Some "bob" }
    ();
  [%expect
    {|
    ((settings ((connect (env:2)) (token ()) (user (bob))))
     (save ((
       (connect env:2)
       (user    bob)))))
    |}]
;;

let%expect_test "flags without an address to connect to save nothing" =
  show ~flags:{ none with token = Some "t" } ();
  [%expect {| ((settings ((connect ()) (token (t)) (user ()))) (save ())) |}]
;;

let%expect_test "-local ignores and keeps the saved connection" =
  show ~saved ~local:true ~flags:{ none with user = Some "bob" } ();
  [%expect
    {|
    ((settings (
       (connect ())
       (token   ())
       (user (bob))))
     (save ()))
    |}]
;;

let%expect_test "save, load, forget" =
  let home = Filename_unix.temp_dir "prigh-tui-saved" "" in
  let load () = print_s [%sexp (S.load ~home : S.t option Or_error.t)] in
  load ();
  [%expect {| (Ok ()) |}];
  ok_exn (S.save ~home saved);
  load ();
  [%expect
    {|
    (Ok ((
      (connect srv:7777)
      (token   sekrit)
      (user    ann))))
    |}];
  print_endline (In_channel.read_all (S.file ~home));
  printf "mode %o\n" (Core_unix.stat (S.file ~home)).st_perm;
  [%expect
    {|
    ((connect srv:7777) (token sekrit) (user ann))

    mode 600
    |}];
  ok_exn (S.save ~home { connect = "x:1"; token = None; user = None });
  load ();
  [%expect {| (Ok (((connect x:1)))) |}];
  ok_exn (S.forget ~home);
  ok_exn (S.forget ~home);
  load ();
  [%expect {| (Ok ()) |}];
  Out_channel.write_all (S.file ~home) ~data:"(garbage";
  (match S.load ~home with
   | Ok _ -> print_endline "loaded?"
   | Error _ -> print_endline "error");
  [%expect {| error |}]
;;
