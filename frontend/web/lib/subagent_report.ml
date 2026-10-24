open! Core

type t =
  { text : string
  ; stats : string option
  }
[@@deriving sexp_of]

let of_string text =
  let lines = String.split_lines (String.rstrip text) in
  match List.last lines with
  | Some last ->
    (match
       Option.bind
         (String.chop_prefix last ~prefix:"[subagent: ")
         ~f:(fun rest -> String.chop_suffix rest ~suffix:"]")
     with
     | Some stats ->
       { text =
           String.rstrip (String.concat ~sep:"\n" (List.drop_last_exn lines))
       ; stats = Some stats
       }
     | None -> { text; stats = None })
  | None -> { text; stats = None }
;;

let started text =
  let open Option.Let_syntax in
  let%bind rest = String.chop_prefix text ~prefix:"started agent " in
  let%map id, _ = String.lsplit2 rest ~on:' ' in
  id
;;
