open! Core
open! Import

let placeholder = "[screenshot omitted]"

let omitted ~min ~max n =
  let min = Int.max 0 min in
  let max = Int.max min max in
  if n <= max then 0 else (n - min) / (max - min + 1) * (max - min + 1)
;;

let screenshots : Message.t -> int = function
  | Tool_result r when String.equal r.tool_name Tool_computer.name ->
    List.length r.images
  | Tool_result _ | User _ | Assistant _ -> 0
;;

let prune ~min ~max messages =
  let total = List.sum (module Int) messages ~f:screenshots in
  let left = ref (omitted ~min ~max total) in
  List.map messages ~f:(fun (message : Message.t) ->
    match message with
    | Tool_result r when !left > 0 && screenshots message > 0 ->
      let n = Int.min !left (List.length r.images) in
      left := !left - n;
      Message.Tool_result
        { r with
          images = List.drop r.images n
        ; text =
            String.concat
              ~sep:"\n"
              (r.text :: List.init n ~f:(fun _ -> placeholder))
        }
    | message -> message)
;;
