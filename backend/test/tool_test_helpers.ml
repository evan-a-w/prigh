open! Core
open! Prigh

type t =
  { env : Eio_unix.Stdenv.base
  ; dir : string
  }

let with_sandbox f =
  Eio_main.run
  @@ fun env ->
  let dir = Filename_unix.temp_dir "prigh-tool" "" in
  let dir = Filename_unix.realpath dir in
  f { env; dir }
;;

let write t path content =
  let path = Filename.concat t.dir path in
  Core_unix.mkdir_p (Filename.dirname path);
  Out_channel.write_all path ~data:content
;;

let read t path = In_channel.read_all (Filename.concat t.dir path)

let id_re =
  Re.compile (Re.repn (Re.alt [ Re.digit; Re.rg 'a' 'f' ]) 16 (Some 16))
;;

let stamp_re =
  Re.compile
    (Re.seq
       [ Re.repn Re.digit 8 (Some 8); Re.char '-'; Re.repn Re.digit 9 (Some 9) ])
;;

let time_re =
  Re.compile (Re.Perl.re {|\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d+Z|})
;;

let duration_re = Re.compile (Re.Perl.re {|"duration_seconds":-?[0-9.eE+-]+|})

(* Replaces the sandbox dir, session ids, file stamps, timestamps and
   durations. *)
let mask t s =
  String.substr_replace_all s ~pattern:t.dir ~with_:"$DIR"
  |> Re.replace_string time_re ~by:"<time>"
  |> Re.replace_string duration_re ~by:{|"duration_seconds":<t>|}
  |> Re.replace_string id_re ~by:"<id>"
  |> Re.replace_string stamp_re ~by:"<stamp>"
  |> String.substr_replace_all
       ~pattern:(sprintf {|"name":"%s"|} (Core_unix.gethostname ()))
       ~with_:{|"name":"<host>"|}
  |> String.substr_replace_all
       ~pattern:(sprintf "tools now run on %s" (Core_unix.gethostname ()))
       ~with_:"tools now run on <host>"
  |> String.substr_replace_all
       ~pattern:(sprintf "\"%s: not a directory" (Core_unix.gethostname ()))
       ~with_:"\"<host>: not a directory"
  |> String.substr_replace_all
       ~pattern:(sprintf "(name %s)" (Core_unix.gethostname ()))
       ~with_:"(name <host>)"
;;

let run ?cancel ?on_output t tool args =
  let context =
    Tool.Context.create ?cancel ?on_output ~env:t.env ~cwd:t.dir ()
  in
  let result = Tool.execute tool context (Jsonaf.of_string args) in
  let text = mask t result.text in
  let text = if String.is_suffix text ~suffix:"\n" then text else text ^ "\n" in
  print_string (if result.is_error then "ERROR: " ^ text else text)
;;
