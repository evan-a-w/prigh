open! Core

type t =
  { name : string
  ; location : string
  ; body : string
  ; args : string
  }
[@@deriving sexp_of, equal]

let close = "\n</skill>"

(* The end of the body is the first [</skill>] line that ends the text or is
   followed by the arguments' blank line: the body or the arguments may
   mention the tag. *)
let split_body rest =
  let rest = "\n" ^ rest in
  let rec find pos =
    match String.substr_index rest ~pos ~pattern:close with
    | None -> None
    | Some i ->
      let after = String.drop_prefix rest (i + String.length close) in
      let body = String.drop_prefix (String.prefix rest i) 1 in
      if String.is_empty after
      then Some (body, "")
      else (
        match String.chop_prefix after ~prefix:"\n\n" with
        | Some args -> Some (body, args)
        | None -> find (i + 1))
  in
  find 0
;;

let parse text =
  let open Option.Let_syntax in
  let%bind header, rest = String.lsplit2 text ~on:'\n' in
  let%bind attributes =
    String.chop_prefix header ~prefix:"<skill name=\""
    >>= String.chop_suffix ~suffix:"\">"
  in
  let%bind i = String.substr_index attributes ~pattern:"\" location=\"" in
  let name = String.prefix attributes i in
  let location =
    String.drop_prefix attributes (i + String.length "\" location=\"")
  in
  let%map body, args = split_body rest in
  { name; location; body = String.strip body; args = String.strip args }
;;

let invocation t =
  if String.is_empty t.args
  then "/skill:" ^ t.name
  else sprintf "/skill:%s %s" t.name t.args
;;
