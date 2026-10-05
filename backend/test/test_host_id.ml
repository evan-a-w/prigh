open! Core
open! Prigh

let with_home f = f (Filename_unix.temp_dir "prigh-host-id" "")

let hex_masked id =
  String.map id ~f:(fun c -> if Char.is_hex_digit c then 'x' else c)
;;

let show_file ~home =
  let path = Host_id.file ~home in
  if Sys_unix.file_exists_exn path
  then printf "file: %S\n" (hex_masked (In_channel.read_all path))
  else print_endline "no file"
;;

let%expect_test "created once, then the same id on every start" =
  with_home
  @@ fun home ->
  show_file ~home;
  let first = Or_error.ok_exn (Host_id.load_or_create ~home) in
  print_endline (hex_masked first);
  show_file ~home;
  let again = Or_error.ok_exn (Host_id.load_or_create ~home) in
  let chosen = Host_id.choose ~home ~warn:print_endline () in
  print_s
    [%message
      ""
        ~same_again:(String.equal first again : bool)
        ~same_chosen:(String.equal first chosen : bool)
        ~files:(Sys_unix.ls_dir (home ^/ ".prigh") : string list)];
  [%expect
    {|
    no file
    host-xxxxxxxxxxxxxxxx
    file: "host-xxxxxxxxxxxxxxxx\n"
    ((same_again true) (same_chosen true) (files (host-id)))
    |}]
;;

let%expect_test "an edited file wins; an empty one gets a new id" =
  with_home
  @@ fun home ->
  Core_unix.mkdir_p (home ^/ ".prigh");
  Out_channel.write_all (Host_id.file ~home) ~data:"  my-desk\n";
  print_s [%sexp (Host_id.load_or_create ~home : string Or_error.t)];
  Out_channel.write_all (Host_id.file ~home) ~data:"\n";
  print_endline (hex_masked (Or_error.ok_exn (Host_id.load_or_create ~home)));
  [%expect
    {|
    (Ok my-desk)
    host-xxxxxxxxxxxxxxxx
    |}]
;;

let%expect_test "an explicit id wins and leaves the file alone" =
  with_home
  @@ fun home ->
  print_endline
    (Host_id.choose ~given:"container-me" ~home ~warn:print_endline ());
  show_file ~home;
  [%expect
    {|
    container-me
    no file
    |}]
;;

let%expect_test "a home where the file cannot be kept: a fresh id, and why" =
  with_home
  @@ fun dir ->
  let home = dir ^/ "not-a-dir" in
  Out_channel.write_all home ~data:"";
  print_endline
    (hex_masked
       (Host_id.choose
          ~home
          ~warn:(fun msg ->
            print_endline
              (String.substr_replace_all msg ~pattern:dir ~with_:"$DIR"))
          ()));
  [%expect
    {|
    cannot keep this host's id in $DIR/not-a-dir/.prigh/host-id ((Unix.Unix_error "Not a directory" mkdir
     "((dirname $DIR/not-a-dir/.prigh) (perm 0o777))")): sessions will not follow it across restarts; pass -host-id ID instead
    host-xxxxxxxxxxxxxxxx
    |}]
;;
