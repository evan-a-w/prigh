open! Import
open Js_of_ocaml

let letter : Dom_html.Keyboard_code.t -> char option = function
  | KeyA -> Some 'a'
  | KeyB -> Some 'b'
  | KeyC -> Some 'c'
  | KeyD -> Some 'd'
  | KeyE -> Some 'e'
  | KeyF -> Some 'f'
  | KeyG -> Some 'g'
  | KeyH -> Some 'h'
  | KeyI -> Some 'i'
  | KeyJ -> Some 'j'
  | KeyK -> Some 'k'
  | KeyL -> Some 'l'
  | KeyM -> Some 'm'
  | KeyN -> Some 'n'
  | KeyO -> Some 'o'
  | KeyP -> Some 'p'
  | KeyQ -> Some 'q'
  | KeyR -> Some 'r'
  | KeyS -> Some 's'
  | KeyT -> Some 't'
  | KeyU -> Some 'u'
  | KeyV -> Some 'v'
  | KeyW -> Some 'w'
  | KeyX -> Some 'x'
  | KeyY -> Some 'y'
  | KeyZ -> Some 'z'
  | Digit0 -> Some '0'
  | Digit1 -> Some '1'
  | Digit2 -> Some '2'
  | Digit3 -> Some '3'
  | Digit4 -> Some '4'
  | Digit5 -> Some '5'
  | Digit6 -> Some '6'
  | Digit7 -> Some '7'
  | Digit8 -> Some '8'
  | Digit9 -> Some '9'
  | _ -> None
;;

let move ?(loop = true) ~equal ~items ~current (key : Dom_html.Keyboard_code.t) =
  let items = Array.of_list items in
  let n = Array.length items in
  if n = 0
  then None
  else (
    let index =
      Option.bind current ~f:(fun c ->
        Array.findi items ~f:(fun _ (x, _) -> equal x c) |> Option.map ~f:fst)
    in
    let at i = Some (fst items.(i)) in
    match key, index with
    | ArrowDown, None -> at 0
    | ArrowDown, Some i -> if loop then at ((i + 1) % n) else at (Int.min (i + 1) (n - 1))
    | ArrowUp, None -> at (n - 1)
    | ArrowUp, Some i -> if loop then at ((i - 1 + n) % n) else at (Int.max (i - 1) 0)
    | Home, _ | PageUp, _ -> at 0
    | End, _ | PageDown, _ -> at (n - 1)
    | key, index ->
      (match letter key with
       | None -> None
       | Some c ->
         let start = Option.value_map index ~default:0 ~f:(fun i -> i + 1) in
         List.init n ~f:(fun k -> (start + k) % n)
         |> List.find ~f:(fun i ->
           let label = String.lowercase (String.strip (snd items.(i))) in
           (not (String.is_empty label)) && Char.equal label.[0] c)
         |> Option.bind ~f:at))
;;

let rec text_of_nodes nodes =
  List.map nodes ~f:(fun (node : Node.t) ->
    match node with
    | Text s -> s
    | Fragment nodes -> text_of_nodes nodes
    | Lazy { t; _ } -> text_of_nodes [ Lazy.force t ]
    | None | Element _ | Widget _ -> "")
  |> String.concat
  |> String.strip
;;
