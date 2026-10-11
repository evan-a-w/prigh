open! Core
open! Import

let ascii_equivalent = function
  | 0x2010 | 0x2011 | 0x2012 | 0x2013 | 0x2014 | 0x2015 | 0x2212 -> Some '-'
  | 0x2018 | 0x2019 | 0x201A | 0x201B -> Some '\''
  | 0x201C | 0x201D | 0x201E | 0x201F -> Some '"'
  | 0x00A0
  | 0x2002
  | 0x2003
  | 0x2004
  | 0x2005
  | 0x2006
  | 0x2007
  | 0x2008
  | 0x2009
  | 0x200A
  | 0x202F
  | 0x205F
  | 0x3000 -> Some ' '
  | _ -> None
;;

(* Calls [f ~pos ~len replacement] for every character of [s], where
   [replacement] is its ASCII equivalent if it has one. *)
let iter_chars s ~f =
  let rec loop pos =
    if pos < String.length s
    then (
      let decode = Stdlib.String.get_utf_8_uchar s pos in
      let len = Stdlib.Uchar.utf_decode_length decode in
      let replacement =
        if Stdlib.Uchar.utf_decode_is_valid decode
        then
          ascii_equivalent
            (Uchar.to_scalar (Stdlib.Uchar.utf_decode_uchar decode))
        else None
      in
      f ~pos ~len replacement;
      loop (pos + len))
  in
  loop 0
;;

let ascii_punctuation s =
  let buffer = Buffer.create (String.length s) in
  iter_chars s ~f:(fun ~pos ~len -> function
    | Some c -> Buffer.add_char buffer c
    | None -> Buffer.add_substring buffer s ~pos ~len);
  Buffer.contents buffer
;;

(* The normalised text, and for each of its bytes the range of the original
   character it comes from. *)
let normalise s =
  let buffer = Buffer.create (String.length s) in
  let origins = Queue.create () in
  (* Spaces, tabs and a CR held back until we know whether the line ends. *)
  let pending = Queue.create () in
  let add c ~pos ~len =
    Buffer.add_char buffer c;
    Queue.enqueue origins (pos, pos + len)
  in
  let flush () =
    Queue.iter pending ~f:(fun (c, pos, len) -> add c ~pos ~len);
    Queue.clear pending
  in
  iter_chars s ~f:(fun ~pos ~len replacement ->
    match replacement, s.[pos] with
    | Some c, _ when Char.equal c ' ' -> Queue.enqueue pending (c, pos, len)
    | None, ((' ' | '\t' | '\r') as c) when len = 1 ->
      Queue.enqueue pending (c, pos, len)
    | None, '\n' when len = 1 ->
      Queue.clear pending;
      add '\n' ~pos ~len
    | Some c, _ ->
      flush ();
      add c ~pos ~len
    | None, _ ->
      flush ();
      for i = 0 to len - 1 do
        add s.[pos + i] ~pos ~len
      done);
  Queue.clear pending;
  Buffer.contents buffer, Queue.to_array origins
;;

let find content ~pattern =
  let text, origins = normalise content in
  let pattern, _ = normalise pattern in
  if String.is_empty pattern
  then []
  else (
    let rec go from acc =
      match String.substr_index text ~pos:from ~pattern with
      | None -> List.rev acc
      | Some i ->
        let start, _ = origins.(i) in
        let _, stop = origins.(i + String.length pattern - 1) in
        go (i + 1) ((start, stop) :: acc)
    in
    go 0 [])
;;
