(* Usage: fake_line_peer [FLAGS]
   --version              prints "fake 1.2.3" and exits
   login status           exits 0 when $FAKE_LOGGED_IN is set, else 1
   --flood N              prints "flood I" for I = 1..N first
   --stderr N             writes N bytes of stderr first
   --long N               prints a line of N "x"s first
   --split-utf8           prints "é" in two writes first
   --exit-after K         exits (code 3) after K input lines
   --ignore-term          ignores SIGTERM
   --no-newline           ends with an unterminated "last" line at EOF
   --hang-at-eof          never exits after the end of input
   Then each input line L is answered with "echo L"; at the end of input it
   says "bye" on stderr and exits 0. *)

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  (match args with
   | [ "--version" ] ->
     print_endline "fake 1.2.3";
     exit 0
   | [ "login"; "status" ] ->
     (match Sys.getenv_opt "FAKE_LOGGED_IN" with
      | Some _ ->
        print_endline "Logged in";
        exit 0
      | None ->
        prerr_endline "Not logged in";
        exit 1)
   | _ -> ());
  let exit_after = ref None in
  let no_newline = ref false in
  let hang = ref false in
  let rec go = function
    | "--flood" :: n :: rest ->
      for i = 1 to int_of_string n do
        Printf.printf "flood %d\n" i
      done;
      flush stdout;
      go rest
    | "--stderr" :: n :: rest ->
      prerr_string (String.make (int_of_string n) 'e');
      flush stderr;
      go rest
    | "--long" :: n :: rest ->
      print_endline (String.make (int_of_string n) 'x');
      go rest
    | "--split-utf8" :: rest ->
      print_string "\xc3";
      flush stdout;
      Unix.sleepf 0.05;
      print_string "\xa9\n";
      flush stdout;
      go rest
    | "--exit-after" :: k :: rest ->
      exit_after := Some (int_of_string k);
      go rest
    | "--ignore-term" :: rest ->
      Sys.set_signal Sys.sigterm Sys.Signal_ignore;
      go rest
    | "--no-newline" :: rest ->
      no_newline := true;
      go rest
    | "--hang-at-eof" :: rest ->
      hang := true;
      go rest
    | [] -> ()
    | arg :: _ ->
      prerr_endline ("unknown argument " ^ arg);
      exit 2
  in
  go args;
  let count = ref 0 in
  (try
     while true do
       let line = input_line stdin in
       print_endline ("echo " ^ line);
       incr count;
       match !exit_after with
       | Some k when !count >= k -> exit 3
       | _ -> ()
     done
   with
   | End_of_file -> ());
  if !no_newline then print_string "last";
  if !hang
  then (
    flush stdout;
    Unix.sleep 3600);
  prerr_endline "bye";
  exit 0
;;
