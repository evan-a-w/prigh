open! Core
open! Import

module Button = struct
  type t =
    | Left
    | Right
    | Middle
    | Back
    | Forward
  [@@deriving sexp_of, equal, enumerate]

  let to_string = function
    | Left -> "left"
    | Right -> "right"
    | Middle -> "middle"
    | Back -> "back"
    | Forward -> "forward"
  ;;

  let of_string s =
    List.find all ~f:(fun b -> String.equal (to_string b) (String.lowercase s))
  ;;
end

module Point = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp_of, equal]

  let to_string { x; y } = sprintf "(%d, %d)" x y
  let to_json { x; y } = `Array [ `Number (Int.to_string x); `Number (Int.to_string y) ]
end

module Region = struct
  type t =
    { x0 : int
    ; y0 : int
    ; x1 : int
    ; y1 : int
    }
  [@@deriving sexp_of, equal]
end

type t =
  | Screenshot
  | Click of
      { at : Point.t option
      ; button : Button.t
      ; count : int
      ; keys : string option
      }
  | Move of Point.t
  | Drag of
      { path : Point.t list
      ; button : Button.t
      ; keys : string option
      }
  | Mouse_down of
      { at : Point.t option
      ; button : Button.t
      }
  | Mouse_up of
      { at : Point.t option
      ; button : Button.t
      }
  | Scroll of
      { at : Point.t option
      ; dx : int
      ; dy : int
      ; keys : string option
      }
  | Type of string
  | Key of string
  | Hold_key of
      { combo : string
      ; duration : float
      }
  | Wait of float
  | Zoom of Region.t
  | Cursor_position
[@@deriving sexp_of, equal]

let names =
  [ "screenshot"
  ; "click"
  ; "move"
  ; "drag"
  ; "mouse_down"
  ; "mouse_up"
  ; "scroll"
  ; "type"
  ; "key"
  ; "hold_key"
  ; "wait"
  ; "zoom"
  ; "cursor_position"
  ]
;;

let number_of = function
  | `Number n -> Float.of_string_opt n
  | _ -> None
;;

let of_json json =
  let open Or_error.Let_syntax in
  let%bind fields =
    match json with
    | `Object fields -> Ok fields
    | _ -> Or_error.error_string "the arguments must be an object with \"action\""
  in
  let find name = List.Assoc.find fields ~equal:String.equal name in
  let%bind action =
    match find "action" with
    | Some (`String a) -> Ok a
    | _ ->
      Or_error.errorf
        "missing \"action\": one of %s"
        (String.concat ~sep:", " names)
  in
  let fail fmt = ksprintf (fun s -> Or_error.errorf "%s: %s" action s) fmt in
  let coordinate name = function
    | `Number _ as n ->
      (match number_of n with
       | Some f when Float.is_finite f && Float.(f >= 0.) ->
         Ok (Float.iround_nearest_exn f)
       | _ -> fail "%S must be a coordinate, 0 or more" name)
    | _ -> fail "%S must be a number (a coordinate in the screenshot)" name
  in
  let point_opt () =
    match find "x", find "y" with
    | None, None -> Ok None
    | Some x, Some y ->
      let%bind x = coordinate "x" x in
      let%bind y = coordinate "y" y in
      Ok (Some { Point.x; y })
    | _ -> fail "give both \"x\" and \"y\" (or neither, for where the pointer is)"
  in
  let point () =
    match%bind point_opt () with
    | Some p -> Ok p
    | None -> fail "needs \"x\" and \"y\" (in the screenshot's coordinates)"
  in
  let button () =
    match find "button" with
    | None | Some `Null -> Ok Button.Left
    | Some (`String s) ->
      (match Button.of_string s with
       | Some b -> Ok b
       | None ->
         fail
           "\"button\" must be one of %s, not %S"
           (String.concat ~sep:", " (List.map Button.all ~f:Button.to_string))
           s)
    | Some _ -> fail "\"button\" must be a string"
  in
  let string_opt name =
    match find name with
    | None | Some `Null -> Ok None
    | Some (`String "") -> Ok None
    | Some (`String s) -> Ok (Some s)
    | Some _ -> fail "%S must be a string" name
  in
  let string name ~what =
    match%bind string_opt name with
    | Some s -> Ok s
    | None -> fail "needs %S: %s" name what
  in
  let int_field name ~default =
    match find name with
    | None | Some `Null -> Ok default
    | Some n ->
      (match number_of n with
       | Some f when Float.is_integer f -> Ok (Float.to_int f)
       | _ -> fail "%S must be a whole number" name)
  in
  let seconds name ~limit =
    match Option.bind (find name) ~f:number_of with
    | Some s when Float.(s > 0. && s <= Float.of_int limit) -> Ok s
    | _ -> fail "%S must be a number of seconds, more than 0 and at most %d" name limit
  in
  match action with
  | "screenshot" -> Ok Screenshot
  | "cursor_position" -> Ok Cursor_position
  | "click" ->
    let%bind at = point_opt () in
    let%bind button = button () in
    let%bind count = int_field "count" ~default:1 in
    let%bind () =
      if count >= 1 && count <= 3
      then Ok ()
      else fail "\"count\" must be 1, 2 (double-click) or 3 (triple-click)"
    in
    let%bind keys = string_opt "keys" in
    Ok (Click { at; button; count; keys })
  | "move" ->
    let%bind p = point () in
    Ok (Move p)
  | "drag" ->
    let%bind path =
      match find "path" with
      | Some (`Array points) when List.length points >= 2 ->
        Or_error.all
          (List.map points ~f:(function
             | `Array [ x; y ] ->
               let%bind x = coordinate "x" x in
               let%bind y = coordinate "y" y in
               Ok { Point.x; y }
             | _ -> fail "each point of \"path\" must be [x, y]"))
      | _ -> fail "needs \"path\": at least two points [[x, y], [x, y], ...]"
    in
    let%bind button = button () in
    let%bind keys = string_opt "keys" in
    Ok (Drag { path; button; keys })
  | "mouse_down" ->
    let%bind at = point_opt () in
    let%bind button = button () in
    Ok (Mouse_down { at; button })
  | "mouse_up" ->
    let%bind at = point_opt () in
    let%bind button = button () in
    Ok (Mouse_up { at; button })
  | "scroll" ->
    let%bind at = point_opt () in
    let%bind dx = int_field "dx" ~default:0 in
    let%bind dy = int_field "dy" ~default:0 in
    let%bind () =
      if dx = 0 && dy = 0
      then fail "give \"dy\" (notches, positive down) or \"dx\" (positive right)"
      else Ok ()
    in
    let%bind keys = string_opt "keys" in
    Ok (Scroll { at; dx; dy; keys })
  | "type" ->
    let%bind text = string "text" ~what:"the text to type" in
    Ok (Type text)
  | "key" ->
    let%bind combo = string "combo" ~what:"the keys, e.g. \"ctrl+shift+t\" or \"Return\"" in
    Ok (Key combo)
  | "hold_key" ->
    let%bind combo = string "combo" ~what:"the keys to hold, e.g. \"shift\"" in
    let%bind duration = seconds "duration" ~limit:10 in
    Ok (Hold_key { combo; duration })
  | "wait" ->
    let%bind s = seconds "seconds" ~limit:30 in
    Ok (Wait s)
  | "zoom" ->
    (match find "region" with
     | Some (`Array [ a; b; c; d ]) ->
       let%bind x0 = coordinate "region" a in
       let%bind y0 = coordinate "region" b in
       let%bind x1 = coordinate "region" c in
       let%bind y1 = coordinate "region" d in
       if x1 > x0 && y1 > y0
       then Ok (Zoom { x0; y0; x1; y1 })
       else fail "\"region\" is [x0, y0, x1, y1] with x1 > x0 and y1 > y0"
     | _ -> fail "needs \"region\": [x0, y0, x1, y1] in the screenshot's coordinates")
  | other ->
    Or_error.errorf
      "unknown action %S: use one of %s"
      other
      (String.concat ~sep:", " names)
;;

let to_json t =
  let int n = `Number (Int.to_string n) in
  let float f = `Number (Float.to_string_hum ~strip_zero:true ~decimals:3 f) in
  let at = function
    | None -> []
    | Some { Point.x; y } -> [ "x", int x; "y", int y ]
  in
  let keys = function
    | None -> []
    | Some k -> [ "keys", `String k ]
  in
  let button b = [ "button", `String (Button.to_string b) ] in
  let fields =
    match t with
    | Screenshot -> [ "action", `String "screenshot" ]
    | Cursor_position -> [ "action", `String "cursor_position" ]
    | Click { at = p; button = b; count; keys = k } ->
      [ "action", `String "click" ] @ at p @ button b @ [ "count", int count ] @ keys k
    | Move p -> [ "action", `String "move" ] @ at (Some p)
    | Drag { path; button = b; keys = k } ->
      [ "action", `String "drag"; "path", `Array (List.map path ~f:Point.to_json) ]
      @ button b
      @ keys k
    | Mouse_down { at = p; button = b } ->
      [ "action", `String "mouse_down" ] @ at p @ button b
    | Mouse_up { at = p; button = b } -> [ "action", `String "mouse_up" ] @ at p @ button b
    | Scroll { at = p; dx; dy; keys = k } ->
      [ "action", `String "scroll" ] @ at p @ [ "dx", int dx; "dy", int dy ] @ keys k
    | Type text -> [ "action", `String "type"; "text", `String text ]
    | Key combo -> [ "action", `String "key"; "combo", `String combo ]
    | Hold_key { combo; duration } ->
      [ "action", `String "hold_key"; "combo", `String combo; "duration", float duration ]
    | Wait s -> [ "action", `String "wait"; "seconds", float s ]
    | Zoom { x0; y0; x1; y1 } ->
      [ "action", `String "zoom"; "region", `Array [ int x0; int y0; int x1; int y1 ] ]
  in
  `Object fields
;;

let quoted text =
  let text =
    if String.length text > 40 then String.prefix text 37 ^ "…" else text
  in
  "“" ^ text ^ "”"
;;

let with_keys keys s =
  match keys with
  | None -> s
  | Some k -> sprintf "%s+%s" k s
;;

let describe t =
  let at = Option.value_map ~default:"" ~f:(fun p -> " " ^ Point.to_string p) in
  let button_prefix (b : Button.t) =
    match b with
    | Left -> ""
    | b -> Button.to_string b ^ " "
  in
  match t with
  | Screenshot -> "screenshot"
  | Cursor_position -> "cursor position"
  | Click { at = p; button; count; keys } ->
    let click =
      match count with
      | 1 -> "click"
      | 2 -> "double-click"
      | _ -> "triple-click"
    in
    with_keys keys (button_prefix button ^ click) ^ at p
  | Move p -> "move " ^ Point.to_string p
  | Drag { path; button; keys } ->
    with_keys keys (button_prefix button ^ "drag ")
    ^ String.concat ~sep:" → " (List.map path ~f:Point.to_string)
  | Mouse_down { at = p; button } -> button_prefix button ^ "mouse down" ^ at p
  | Mouse_up { at = p; button } -> button_prefix button ^ "mouse up" ^ at p
  | Scroll { at = p; dx; dy; keys } ->
    let part n ~pos ~neg =
      if n = 0 then [] else [ sprintf "%s%d" (if n > 0 then pos else neg) (abs n) ]
    in
    let amount = String.concat ~sep:" " (part dy ~pos:"↓" ~neg:"↑" @ part dx ~pos:"→" ~neg:"←") in
    with_keys keys "scroll " ^ amount ^ Option.value_map p ~default:"" ~f:(fun p -> " at " ^ Point.to_string p)
  | Type text -> "type " ^ quoted text
  | Key combo -> "key " ^ combo
  | Hold_key { combo; duration } ->
    sprintf "hold %s for %ss" combo (Float.to_string_hum ~strip_zero:true ~decimals:2 duration)
  | Wait s -> sprintf "wait %ss" (Float.to_string_hum ~strip_zero:true ~decimals:2 s)
  | Zoom { x0; y0; x1; y1 } -> sprintf "zoom [%d, %d, %d, %d]" x0 y0 x1 y1
;;

let point = function
  | Click { at; _ } | Mouse_down { at; _ } | Mouse_up { at; _ } | Scroll { at; _ } -> at
  | Move p -> Some p
  | Drag { path; _ } -> List.last path
  | Screenshot | Cursor_position | Type _ | Key _ | Hold_key _ | Wait _ | Zoom _ -> None
;;

let takes_screenshot = function
  | Wait _ -> false
  | _ -> true
;;

let parameters =
  let prop ?enum type_ description =
    `Object
      ([ "type", `String type_; "description", `String description ]
       @ Option.value_map enum ~default:[] ~f:(fun values ->
         [ "enum", `Array (List.map values ~f:(fun v -> `String v)) ]))
  in
  let point = `Object [ "type", `String "array"; "items", `Object [ "type", `String "integer" ] ] in
  `Object
    [ "type", `String "object"
    ; ( "properties"
      , `Object
          [ ( "action"
            , prop
                "string"
                ~enum:names
                "What to do. Every action but wait returns a screenshot afterwards." )
          ; "x", prop "integer" "Horizontal coordinate in the screenshot (click, move, mouse_down/up, scroll; default: where the pointer is)"
          ; "y", prop "integer" "Vertical coordinate in the screenshot"
          ; ( "button"
            , prop
                "string"
                ~enum:(List.map Button.all ~f:Button.to_string)
                "Mouse button (default left)" )
          ; "count", prop "integer" "click: 1, 2 (double-click) or 3 (triple-click)"
          ; "keys", prop "string" "Modifier keys held during a click, drag or scroll, e.g. \"ctrl+shift\""
          ; ( "path"
            , `Object
                [ "type", `String "array"
                ; "items", point
                ; "description", `String "drag: the points [[x, y], ...] the pointer goes through, at least two"
                ] )
          ; "dx", prop "integer" "scroll: wheel notches to the right (negative: left)"
          ; "dy", prop "integer" "scroll: wheel notches down (negative: up)"
          ; "text", prop "string" "type: the text to type (any Unicode)"
          ; ( "combo"
            , prop
                "string"
                "key, hold_key: keys like \"ctrl+shift+t\", \"Return\", \"alt+Tab\"; several separated by spaces are pressed in turn" )
          ; "duration", prop "number" "hold_key: seconds to hold (at most 10)"
          ; "seconds", prop "number" "wait: seconds (at most 30)"
          ; ( "region"
            , `Object
                [ "type", `String "array"
                ; "items", `Object [ "type", `String "integer" ]
                ; ( "description"
                  , `String
                      "zoom: [x0, y0, x1, y1] in the screenshot; returns that area at the screen's full resolution" )
                ] )
          ] )
    ; "required", `Array [ `String "action" ]
    ]
;;
