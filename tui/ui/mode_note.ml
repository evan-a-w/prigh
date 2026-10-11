open! Core
open! Import

let split text =
  let note ~prefix ~on =
    Option.bind (String.chop_prefix text ~prefix) ~f:(fun rest ->
      Option.map (String.substr_index rest ~pattern:"]") ~f:(fun i ->
        on, String.lstrip (String.drop_prefix rest (i + 1))))
  in
  match note ~prefix:"[Plan mode is on:" ~on:true with
  | Some _ as found -> found
  | None -> note ~prefix:"[Plan mode is off:" ~on:false
;;

let summary ~on =
  if on
  then "plan mode on: the model was told to plan, not change files"
  else "plan mode off: the model was told it can change files"
;;
