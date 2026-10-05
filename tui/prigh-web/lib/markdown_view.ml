open! Core
open! Import

let inline text =
  (* `code` spans; everything else is text. *)
  String.split text ~on:'`'
  |> List.mapi ~f:(fun i part ->
    if i % 2 = 1 then Node.code [ Node.text part ] else Node.text part)
;;

let render text =
  let lines = String.split_lines text in
  let rec blocks acc paragraph = function
    | [] -> List.rev (flush acc paragraph)
    | line :: rest when String.is_prefix (String.lstrip line) ~prefix:"```" ->
      let acc = flush acc paragraph in
      let code, rest =
        List.split_while rest ~f:(fun l ->
          not (String.is_prefix (String.lstrip l) ~prefix:"```"))
      in
      let lang =
        String.strip (String.chop_prefix_exn (String.lstrip line) ~prefix:"```")
      in
      let block =
        Node.pre
          ~attrs:[ Attr.class_ "code-block" ]
          [ Node.code
              ~attrs:
                (if String.is_empty lang
                 then []
                 else [ Attr.class_ ("lang-" ^ lang) ])
              [ Node.text (String.concat ~sep:"\n" code) ]
          ]
      in
      blocks (block :: acc) [] (List.drop rest 1)
    | line :: rest when String.is_empty (String.strip line) ->
      blocks (flush acc paragraph) [] rest
    | line :: rest -> blocks acc (line :: paragraph) rest
  and flush acc = function
    | [] -> acc
    | paragraph ->
      Node.p (inline (String.concat ~sep:"\n" (List.rev paragraph))) :: acc
  in
  Node.div ~attrs:[ Attr.class_ "markdown" ] (blocks [] [] lines)
;;
