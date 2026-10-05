open! Core
open! Import

type t =
  | Complete of Json.t
  | Partial of string

let of_call (call : Tool_call.t) =
  match Json.parse call.arguments with
  | Ok json -> Complete json
  | Error _ -> Partial call.arguments
;;

(* The JSON string starting after the quote at [pos], unterminated or not. *)
let partial_string s pos =
  let buf = Buffer.create 64 in
  let len = String.length s in
  let rec go i =
    if i < len
    then (
      match s.[i] with
      | '"' -> ()
      | '\\' when i + 1 < len ->
        let escaped, next =
          match s.[i + 1] with
          | 'n' -> Some '\n', i + 2
          | 't' -> Some '\t', i + 2
          | 'r' -> Some '\r', i + 2
          | 'b' | 'f' -> None, i + 2
          | 'u' when i + 5 < len ->
            Int.of_string_opt ("0x" ^ String.sub s ~pos:(i + 2) ~len:4)
            |> Option.bind ~f:Uchar.of_scalar
            |> Option.iter ~f:(Stdlib.Buffer.add_utf_8_uchar buf);
            None, i + 6
          | 'u' -> None, len
          | c -> Some c, i + 2
        in
        Option.iter escaped ~f:(Buffer.add_char buf);
        go next
      | c ->
        Buffer.add_char buf c;
        go (i + 1))
  in
  go pos;
  Buffer.contents buf
;;

let partial_field s key =
  let pattern = sprintf "%S" key in
  let rec find from =
    match String.substr_index s ~pos:from ~pattern with
    | None -> None
    | Some i ->
      let rest = String.drop_prefix s (i + String.length pattern) in
      let after_colon = String.lstrip rest in
      (match String.chop_prefix after_colon ~prefix:":" with
       | None -> find (i + 1)
       | Some value ->
         let value = String.lstrip value in
         if String.is_prefix value ~prefix:"\""
         then Some (partial_string value 1)
         else find (i + 1))
  in
  find 0
;;

let field t key =
  match t with
  | Complete json -> Json.field json key
  | Partial _ -> None
;;

let string t key =
  match t with
  | Complete _ ->
    (match field t key with
     | Some (`String s) -> Some s
     | _ -> None)
  | Partial s -> partial_field s key
;;

let bool t key =
  match field t key with
  | Some `True -> Some true
  | Some `False -> Some false
  | _ -> None
;;

let int t key =
  match field t key with
  | Some (`Number n) ->
    Option.try_with (fun () -> Float.to_int (Float.of_string n))
  | _ -> None
;;

let strings t key =
  match field t key with
  | Some (`Array items) ->
    List.filter_map items ~f:(function
      | `String s -> Some s
      | _ -> None)
  | Some (`String s) -> [ s ]
  | _ -> []
;;

let edits t =
  match field t "edits" with
  | Some (`Array items) ->
    List.filter_map items ~f:(fun item ->
      match Json.field item "old_text", Json.field item "new_text" with
      | Some (`String old_text), Some (`String new_text) ->
        Some (old_text, new_text)
      | _ -> None)
  | _ -> []
;;

let to_string_hum t =
  match t with
  | Complete json -> Jsonaf.to_string_hum json
  | Partial s -> s
;;
