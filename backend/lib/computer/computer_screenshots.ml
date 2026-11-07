open! Core
open! Import

let placeholder = "[screenshot omitted]"

let keep_last keep messages =
  let left = ref keep in
  List.rev messages
  |> List.map ~f:(fun (message : Message.t) ->
    match message with
    | Tool_result r
      when String.equal r.tool_name Tool_computer.name
           && not (List.is_empty r.images) ->
      let n = List.length r.images in
      if !left >= n
      then (
        left := !left - n;
        message)
      else (
        left := 0;
        Tool_result
          { r with
            images = []
          ; text =
              String.concat
                ~sep:"\n"
                (r.text :: List.init n ~f:(fun _ -> placeholder))
          })
    | message -> message)
  |> List.rev
;;
