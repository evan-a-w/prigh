open! Core
open! Import
module P = Patience_diff_lib.Patience_diff

type t =
  { diff : string
  ; added : int
  ; removed : int
  }
[@@deriving sexp_of, equal]

let context = 3

(* Lines with their newline, so that a last line losing or gaining it
   differs. *)
let lines text =
  let rec go acc pos =
    if pos >= String.length text
    then List.rev acc
    else (
      match String.index_from text pos '\n' with
      | Some i -> go (String.sub text ~pos ~len:(i - pos + 1) :: acc) (i + 1)
      | None -> List.rev (String.drop_prefix text pos :: acc))
  in
  Array.of_list (go [] 0)
;;

let unified ~path ~old ~new_ =
  if String.equal old new_
  then { diff = ""; added = 0; removed = 0 }
  else (
    let hunks =
      P.String.get_hunks
        ~transform:Fn.id
        ~context
        ~big_enough:3
        ~prev:(lines old)
        ~next:(lines new_)
        ()
      |> List.filter ~f:(fun h -> not (P.Hunk.all_same h))
    in
    let buf = Buffer.create 1024 in
    let added = ref 0 in
    let removed = ref 0 in
    bprintf buf "diff --git a/%s b/%s\n--- a/%s\n+++ b/%s\n" path path path path;
    let line prefix l =
      match String.chop_suffix l ~suffix:"\n" with
      | Some l -> bprintf buf "%c%s\n" prefix l
      | None -> bprintf buf "%c%s\n\\ No newline at end of file\n" prefix l
    in
    let starts start size = if size = 0 then start - 1 else start in
    List.iter hunks ~f:(fun (h : string P.Hunk.t) ->
      bprintf
        buf
        "@@ -%d,%d +%d,%d @@\n"
        (starts h.prev_start h.prev_size)
        h.prev_size
        (starts h.next_start h.next_size)
        h.next_size;
      let minus ls =
        Array.iter ls ~f:(fun l ->
          incr removed;
          line '-' l)
      in
      let plus ls =
        Array.iter ls ~f:(fun l ->
          incr added;
          line '+' l)
      in
      List.iter h.ranges ~f:(fun (r : string P.Range.t) ->
        match r with
        | Same pairs -> Array.iter pairs ~f:(fun (_, l) -> line ' ' l)
        | Prev (ls, _) -> minus ls
        | Next (ls, _) -> plus ls
        | Replace (prev, next, _) ->
          minus prev;
          plus next
        | Unified (ls, _) -> Array.iter ls ~f:(fun l -> line ' ' l)));
    { diff = Buffer.contents buf; added = !added; removed = !removed })
;;
