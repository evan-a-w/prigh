open! Core
open! Import

let limit = 500
let path ~home = Filename.concat home ".prigh/history"

let load ~home =
  match In_channel.read_lines (path ~home) with
  | exception _ -> []
  | lines ->
    let entries =
      List.filter_map lines ~f:(fun line ->
        match Json.parse line with
        | Ok (`String s) -> Some s
        | Ok _ | Error _ -> None)
    in
    List.drop entries (Int.max 0 (List.length entries - limit))
;;

let append ~home text =
  let path = path ~home in
  (try Core_unix.mkdir_p (Filename.dirname path) with
   | _ -> ());
  Out_channel.with_file path ~append:true ~f:(fun oc ->
    Out_channel.output_string oc (Json.to_string (`String text));
    Out_channel.newline oc)
;;
