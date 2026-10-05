open! Core

type t =
  { name : string
  ; location : string
  ; body : string
  ; args : string
  }
[@@deriving sexp_of, equal]

let attribute text ~pos ~name =
  let open Option.Let_syntax in
  let prefix = name ^ "=\"" in
  let%bind () =
    Option.some_if (String.is_substring_at text ~pos ~substring:prefix) ()
  in
  let start = pos + String.length prefix in
  let%map stop = String.index_from text start '"' in
  String.sub text ~pos:start ~len:(stop - start), stop + 1
;;

(* The body ends at the first [</skill>] closing a line that is followed by
   the end or a blank line: the body or the arguments may mention the tag. *)
let parse text =
  let open Option.Let_syntax in
  let%bind () =
    Option.some_if (String.is_prefix text ~prefix:"<skill name=\"") ()
  in
  let%bind name, pos = attribute text ~pos:7 ~name:"name" in
  let%bind () =
    Option.some_if (String.is_substring_at text ~pos ~substring:" ") ()
  in
  let%bind location, pos = attribute text ~pos:(pos + 1) ~name:"location" in
  let%bind () =
    Option.some_if (String.is_substring_at text ~pos ~substring:">\n") ()
  in
  let start = pos + 2 in
  let closing = "\n</skill>" in
  let%map stop =
    String.substr_index_all text ~may_overlap:false ~pattern:closing
    |> List.find ~f:(fun i ->
      let after = i + String.length closing in
      i >= start - 1
      && (after = String.length text
          || String.is_substring_at text ~pos:after ~substring:"\n\n"))
  in
  let after = stop + String.length closing in
  { name
  ; location
  ; body =
      String.strip (String.sub text ~pos:start ~len:(Int.max 0 (stop - start)))
  ; args = String.strip (String.drop_prefix text after)
  }
;;

let invocation t = String.strip (sprintf "/skill:%s %s" t.name t.args)
