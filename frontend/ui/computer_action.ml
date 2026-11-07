open! Core
open! Import

module Point = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp_of, equal]

  let to_string { x; y } = sprintf "(%d, %d)" x y
  let compact { x; y } = sprintf "%d,%d" x y
end

type t =
  | Screenshot
  | Click of
      { at : Point.t option
      ; button : string
      ; count : int
      ; keys : string option
      }
  | Move of Point.t option
  | Drag of Point.t list
  | Mouse_down of
      { at : Point.t option
      ; button : string
      }
  | Mouse_up of
      { at : Point.t option
      ; button : string
      }
  | Scroll of
      { at : Point.t option
      ; dx : int
      ; dy : int
      }
  | Type of string
  | Key of string
  | Hold_key of
      { combo : string
      ; duration : float option
      }
  | Wait of float option
  | Zoom of int list
  | Cursor_position
  | Other of string
[@@deriving sexp_of]

let number = function
  | `Number n -> Float.of_string_opt n
  | _ -> None
;;

let int json = Option.map (number json) ~f:Float.iround_nearest_exn

let point_of_pair = function
  | `Array [ x; y ] ->
    (match int x, int y with
     | Some x, Some y -> Some { Point.x; y }
     | _ -> None)
  | _ -> None
;;

let of_json json action =
  let field name = P.Json.field json name in
  let int_field name = Option.bind (field name) ~f:int in
  let str name =
    match field name with
    | Some (`String s) -> Some s
    | _ -> None
  in
  let at =
    match int_field "x", int_field "y" with
    | Some x, Some y -> Some { Point.x; y }
    | _ -> None
  in
  let button = Option.value (str "button") ~default:"left" in
  match action with
  | "screenshot" -> Screenshot
  | "click" ->
    Click
      { at
      ; button
      ; count = Option.value (int_field "count") ~default:1
      ; keys = str "keys"
      }
  | "move" -> Move at
  | "drag" ->
    Drag
      (match field "path" with
       | Some (`Array points) -> List.filter_map points ~f:point_of_pair
       | _ -> [])
  | "mouse_down" -> Mouse_down { at; button }
  | "mouse_up" -> Mouse_up { at; button }
  | "scroll" ->
    Scroll
      { at
      ; dx = Option.value (int_field "dx") ~default:0
      ; dy = Option.value (int_field "dy") ~default:0
      }
  | "type" -> Type (Option.value (str "text") ~default:"")
  | "key" -> Key (Option.value (str "combo") ~default:"")
  | "hold_key" ->
    Hold_key
      { combo = Option.value (str "combo") ~default:""
      ; duration = Option.bind (field "duration") ~f:number
      }
  | "wait" -> Wait (Option.bind (field "seconds") ~f:number)
  | "zoom" ->
    Zoom
      (match field "region" with
       | Some (`Array items) -> List.filter_map items ~f:int
       | _ -> [])
  | "cursor_position" -> Cursor_position
  | other -> Other other
;;

let of_call (call : P.Tool_call.t) =
  match P.Json.parse call.arguments with
  | Ok json ->
    (match P.Json.field json "action" with
     | Some (`String action) -> Some (of_json json action)
     | _ -> None)
  | Error _ ->
    (* Streaming: what is there so far. *)
    let args = Tool_args.of_call call in
    Option.map (Tool_args.string args "action") ~f:(fun action ->
      let json =
        `Object
          (List.filter_map [ "text"; "combo" ] ~f:(fun key ->
             Option.map (Tool_args.string args key) ~f:(fun v -> key, `String v)))
      in
      of_json json action)
;;

let seconds s =
  let s = Float.to_string_hum ~decimals:1 ~strip_zero:true s in
  s ^ "s"
;;

let clicks ~button ~count =
  let what =
    match count with
    | 1 -> "click"
    | 2 -> "double-click"
    | 3 -> "triple-click"
    | n -> sprintf "%d×click" n
  in
  match button with
  | "left" -> what
  | button -> button ^ " " ^ what
;;

let scroll_amount ~dx ~dy =
  let part n ~neg ~pos =
    if n = 0 then None else Some (sprintf "%s%d" (if n < 0 then neg else pos) (abs n))
  in
  match List.filter_opt [ part dy ~neg:"↑" ~pos:"↓"; part dx ~neg:"←" ~pos:"→" ] with
  | [] -> "0"
  | parts -> String.concat ~sep:" " parts
;;

let quoted text =
  let q = Tool_summary.quoted ~width:40 text in
  let q = Option.value (String.chop_prefix q ~prefix:"\"") ~default:q in
  let q = Option.value (String.chop_suffix q ~suffix:"\"") ~default:q in
  "“" ^ q ^ "”"
;;

let render ~point t =
  let at = function
    | None -> ""
    | Some p -> " " ^ point p
  in
  match t with
  | Screenshot -> "screenshot"
  | Click { at = p; button; count; keys } ->
    Option.value_map keys ~default:"" ~f:(fun k -> k ^ "+")
    ^ clicks ~button ~count
    ^ at p
  | Move p -> "move" ^ at p
  | Drag points ->
    (match points with
     | [] -> "drag"
     | first :: _ ->
       sprintf "drag %s → %s" (point first) (point (List.last_exn points)))
  | Mouse_down { at = p; button } -> sprintf "%s down%s" button (at p)
  | Mouse_up { at = p; button } -> sprintf "%s up%s" button (at p)
  | Scroll { at = p; dx; dy } ->
    sprintf
      "scroll %s%s"
      (scroll_amount ~dx ~dy)
      (match p with
       | None -> ""
       | Some p -> " at " ^ point p)
  | Type text -> "type " ^ quoted text
  | Key combo -> "key " ^ combo
  | Hold_key { combo; duration } ->
    "hold " ^ combo ^ Option.value_map duration ~default:"" ~f:(fun d -> " " ^ seconds d)
  | Wait s -> "wait" ^ Option.value_map s ~default:"" ~f:(fun s -> " " ^ seconds s)
  | Zoom region ->
    sprintf
      "zoom [%s]"
      (String.concat ~sep:", " (List.map region ~f:Int.to_string))
  | Cursor_position -> "cursor position"
  | Other action -> action
;;

let describe = render ~point:Point.to_string
let compact = render ~point:Point.compact

let point = function
  | Click { at; _ } | Move at | Scroll { at; _ } | Mouse_down { at; _ } | Mouse_up { at; _ }
    -> at
  | Drag points -> List.last points
  | Screenshot
  | Type _
  | Key _
  | Hold_key _
  | Wait _
  | Zoom _
  | Cursor_position
  | Other _ -> None
;;

let screenshot_size text =
  match List.last (String.split_lines (String.rstrip text)) with
  | None -> None
  | Some line ->
    (match String.chop_prefix (String.strip line) ~prefix:"screenshot " with
     | None -> None
     | Some size ->
       (match String.substr_index size ~pattern:"×" with
        | None -> None
        | Some i ->
          let w = String.prefix size i in
          let h = String.drop_prefix size (i + String.length "×") in
          (match Int.of_string_opt w, Int.of_string_opt h with
           | Some w, Some h when w > 0 && h > 0 -> Some (w, h)
           | _ -> None)))
;;
