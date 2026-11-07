open! Core
open! Import

module Safety_check = struct
  type t =
    { id : string
    ; code : string option
    ; message : string option
    }
  [@@deriving sexp_of]

  let of_json json =
    let str name =
      match Json.member name json with
      | Some (`String s) -> Some s
      | _ -> None
    in
    Option.map (str "id") ~f:(fun id ->
      { id; code = str "code"; message = str "message" })
  ;;

  let to_json { id; code; message } =
    `Object
      ([ "id", `String id ]
       @ Option.value_map code ~default:[] ~f:(fun c -> [ "code", `String c ])
       @ Option.value_map message ~default:[] ~f:(fun m ->
         [ "message", `String m ]))
  ;;
end

let native_key = "native"
let safety_checks_key = "safety_checks"

let fields = function
  | `Object fields -> fields
  | _ -> []
;;

let parse arguments =
  match Json.parse arguments with
  | Ok (`Object fields) -> fields
  | Ok _ | Error _ -> []
;;

let find fields name = List.Assoc.find fields ~equal:String.equal name

let safety_checks arguments =
  match find (fields arguments) safety_checks_key with
  | Some (`Array checks) -> List.filter_map checks ~f:Safety_check.of_json
  | _ -> []
;;

let safety_note checks =
  "OpenAI safety checks acknowledged: "
  ^ String.concat
      ~sep:"; "
      (List.map checks ~f:(fun (c : Safety_check.t) ->
         match c.code, c.message with
         | Some code, Some message -> code ^ ": " ^ message
         | Some s, None | None, Some s -> s
         | None, None -> c.id))
;;

let without_extras fields =
  List.filter fields ~f:(fun (key, _) ->
    not (String.equal key native_key || String.equal key safety_checks_key))
;;

let generic arguments =
  match Json.parse arguments with
  | Ok (`Object fields) -> Json.to_string (`Object (without_extras fields))
  | Ok _ | Error _ -> arguments
;;

let int n = `Number (Int.to_string n)

let number = function
  | `Number n -> Float.of_string_opt n
  | _ -> None
;;

let int_of json = Option.map (number json) ~f:Float.iround_nearest_exn

(* The arguments [action], or [fields] themselves when they are not a valid
   action, so that the host's error names what is wrong. *)
let stored ~fallback ~native ~extra =
  let base =
    match Computer_action.of_json (`Object fallback) with
    | Ok a -> fields (Computer_action.to_json a)
    | Error _ -> fallback
  in
  Json.to_string (`Object (base @ [ native_key, native ] @ extra))
;;

(* Anthropic *)

let coordinate = function
  | Some (`Array [ x; y ]) -> [ "x", x; "y", y ]
  | _ -> []
;;

let of_anthropic ~toolset ~name input =
  let input_fields = fields input in
  let action =
    if toolset
    then name
    else (
      match find input_fields "action" with
      | Some (`String a) -> a
      | _ -> "")
  in
  let get = find input_fields in
  let keys =
    match get "text" with
    | Some (`String k) when not (String.is_empty k) -> [ "keys", `String k ]
    | _ -> []
  in
  let click button count =
    [ "action", `String "click" ]
    @ coordinate (get "coordinate")
    @ [ "button", `String button; "count", int count ]
    @ keys
  in
  let text_as name =
    match get "text" with
    | Some text -> [ name, text ]
    | None -> []
  in
  let generic =
    match action with
    | "left_click" -> click "left" 1
    | "right_click" -> click "right" 1
    | "middle_click" -> click "middle" 1
    | "double_click" -> click "left" 2
    | "triple_click" -> click "left" 3
    | "mouse_move" ->
      [ "action", `String "move" ] @ coordinate (get "coordinate")
    | "left_click_drag" ->
      let point = function
        | Some (`Array [ x; y ]) -> Some (`Array [ x; y ])
        | _ -> None
      in
      [ "action", `String "drag"
      ; ( "path"
        , `Array
            (List.filter_opt
               [ point (get "start_coordinate"); point (get "coordinate") ]) )
      ]
      @ keys
    | "left_mouse_down" | "left_mouse_up" ->
      [ ( "action"
        , `String
            (if String.equal action "left_mouse_down"
             then "mouse_down"
             else "mouse_up") )
      ]
      @ coordinate (get "coordinate")
    | "scroll" ->
      let amount =
        Option.value (Option.bind (get "scroll_amount") ~f:int_of) ~default:3
      in
      let delta =
        match get "scroll_direction" with
        | Some (`String "up") -> [ "dy", int (-amount) ]
        | Some (`String "down") -> [ "dy", int amount ]
        | Some (`String "left") -> [ "dx", int (-amount) ]
        | Some (`String "right") -> [ "dx", int amount ]
        | _ -> []
      in
      [ "action", `String "scroll" ]
      @ coordinate (get "coordinate")
      @ delta
      @ keys
    | "type" -> [ "action", `String "type" ] @ text_as "text"
    | "key" ->
      let repeat =
        Int.max
          1
          (Option.value (Option.bind (get "repeat") ~f:int_of) ~default:1)
      in
      [ "action", `String "key" ]
      @
        (match get "text" with
        | Some (`String combo) ->
          [ ( "combo"
            , `String
                (String.concat ~sep:" " (List.init repeat ~f:(fun _ -> combo)))
            )
          ]
        | _ -> [])
    | "hold_key" ->
      [ "action", `String "hold_key" ]
      @ text_as "combo"
      @ Option.value_map (get "duration") ~default:[] ~f:(fun d ->
        [ "duration", d ])
    | "wait" ->
      [ "action", `String "wait" ]
      @ Option.value_map (get "duration") ~default:[] ~f:(fun d ->
        [ "seconds", d ])
    | "zoom" ->
      [ "action", `String "zoom" ]
      @ Option.value_map (get "region") ~default:[] ~f:(fun r ->
        [ "region", r ])
    | "screenshot" | "cursor_position" -> [ "action", `String action ]
    | other ->
      ("action", `String other)
      :: List.filter input_fields ~f:(fun (k, _) ->
        not (String.equal k "action"))
  in
  let native =
    `Object
      (if toolset
       then
         [ "form", `String "anthropic_toolset"
         ; "name", `String name
         ; "input", input
         ]
       else [ "form", `String "anthropic"; "input", input ])
  in
  stored ~fallback:generic ~native ~extra:[]
;;

let point_json { Computer_action.Point.x; y } = `Array [ int x; int y ]

let anthropic_of_action (action : Computer_action.t) =
  let coordinate = function
    | None -> []
    | Some p -> [ "coordinate", point_json p ]
  in
  let modifiers = function
    | None -> []
    | Some k -> [ "text", `String k ]
  in
  let seconds f =
    `Number (Float.to_string_hum ~strip_zero:true ~decimals:3 f)
  in
  match action with
  | Screenshot -> "screenshot", []
  | Cursor_position -> "cursor_position", []
  | Click { at; button; count; keys } ->
    let name =
      match button, count with
      | Right, _ -> "right_click"
      | Middle, _ -> "middle_click"
      | (Left | Back | Forward), 2 -> "double_click"
      | (Left | Back | Forward), 3 -> "triple_click"
      | (Left | Back | Forward), _ -> "left_click"
    in
    name, coordinate at @ modifiers keys
  | Move p -> "mouse_move", coordinate (Some p)
  | Drag { path; button = _; keys } ->
    ( "left_click_drag"
    , (match path, List.last path with
       | first :: _, Some last ->
         [ "start_coordinate", point_json first; "coordinate", point_json last ]
       | _ -> [])
      @ modifiers keys )
  | Mouse_down _ -> "left_mouse_down", []
  | Mouse_up _ -> "left_mouse_up", []
  | Scroll { at; dx; dy; keys } ->
    let direction, amount =
      if dy <> 0
      then (if dy > 0 then "down" else "up"), Int.abs dy
      else (if dx > 0 then "right" else "left"), Int.abs dx
    in
    ( "scroll"
    , coordinate at
      @ [ "scroll_direction", `String direction; "scroll_amount", int amount ]
      @ modifiers keys )
  | Type text -> "type", [ "text", `String text ]
  | Key combo -> "key", [ "text", `String combo ]
  | Hold_key { combo; duration } ->
    "hold_key", [ "text", `String combo; "duration", seconds duration ]
  | Wait s -> "wait", [ "duration", seconds s ]
  | Zoom { x0; y0; x1; y1 } ->
    "zoom", [ "region", `Array [ int x0; int y0; int x1; int y1 ] ]
;;

let native_of stored ~form =
  match find stored native_key with
  | Some native ->
    (match Json.member "form" native with
     | Some (`String f) when String.equal f form -> Some (fields native)
     | _ -> None)
  | None -> None
;;

let to_anthropic ~toolset arguments =
  let stored = parse arguments in
  let form = if toolset then "anthropic_toolset" else "anthropic" in
  match native_of stored ~form with
  | Some native ->
    let input = Option.value (find native "input") ~default:(`Object []) in
    (match toolset, find native "name" with
     | true, Some (`String name) -> name, input
     | _ -> "computer", input)
  | None ->
    let name, input =
      match Computer_action.of_json (`Object (without_extras stored)) with
      | Ok action -> anthropic_of_action action
      | Error _ ->
        ( (match find stored "action" with
           | Some (`String a) -> a
           | _ -> "screenshot")
        , List.filter (without_extras stored) ~f:(fun (k, _) ->
            not (String.equal k "action")) )
    in
    if toolset
    then name, `Object input
    else "computer", `Object (("action", `String name) :: input)
;;

(* OpenAI *)

let key_name key = if String.length key = 1 then String.lowercase key else key

let keys_of = function
  | Some (`Array keys) ->
    (match
       List.filter_map keys ~f:(function
         | `String k -> Some (key_name k)
         | _ -> None)
     with
     | [] -> None
     | keys -> Some (String.concat ~sep:"+" keys))
  | _ -> None
;;

(* The API's scroll distances are pixels; the tool's are wheel notches. *)
let pixels_per_notch = 100

let notches = function
  | None -> 0
  | Some px ->
    (match int_of px with
     | None | Some 0 -> 0
     | Some px ->
       let n = Int.max 1 (Int.abs px / pixels_per_notch) in
       if px < 0 then -n else n)
;;

let of_openai ?item_id ?(safety_checks = []) action =
  let get = find (fields action) in
  let at =
    [ "x"; "y" ]
    |> List.filter_map ~f:(fun k -> Option.map (get k) ~f:(fun v -> k, v))
  in
  let keys =
    Option.value_map
      (keys_of (get "keys"))
      ~default:[]
      ~f:(fun k -> [ "keys", `String k ])
  in
  let type_ =
    match get "type" with
    | Some (`String t) -> t
    | _ -> ""
  in
  let generic =
    match type_ with
    | "click" ->
      let button =
        match get "button" with
        | Some (`String "wheel") -> "middle"
        | Some (`String b) -> b
        | _ -> "left"
      in
      [ "action", `String "click" ] @ at @ [ "button", `String button ] @ keys
    | "double_click" ->
      [ "action", `String "click" ] @ at @ [ "count", int 2 ] @ keys
    | "drag" ->
      let path =
        match get "path" with
        | Some (`Array points) ->
          List.filter_map points ~f:(fun p ->
            match Json.member "x" p, Json.member "y" p with
            | Some x, Some y -> Some (`Array [ x; y ])
            | _ -> None)
        | _ -> []
      in
      [ "action", `String "drag"; "path", `Array path ] @ keys
    | "move" -> [ "action", `String "move" ] @ at
    | "scroll" ->
      [ "action", `String "scroll" ]
      @ at
      @ [ "dx", int (notches (get "scroll_x"))
        ; "dy", int (notches (get "scroll_y"))
        ]
      @ keys
    | "keypress" ->
      [ "action", `String "key" ]
      @ Option.value_map
          (keys_of (get "keys"))
          ~default:[]
          ~f:(fun k -> [ "combo", `String k ])
    | "type" ->
      [ "action", `String "type" ]
      @ Option.value_map (get "text") ~default:[] ~f:(fun t -> [ "text", t ])
    | "wait" -> [ "action", `String "wait"; "seconds", int 2 ]
    | "screenshot" -> [ "action", `String "screenshot" ]
    | other ->
      ("action", `String other)
      :: List.filter (fields action) ~f:(fun (k, _) ->
        not (String.equal k "type"))
  in
  stored
    ~fallback:generic
    ~native:
      (`Object
          ([ "form", `String "openai" ]
           @ Option.value_map item_id ~default:[] ~f:(fun id ->
             [ "id", `String id ])
           @ [ "action", action ]))
    ~extra:
      (if List.is_empty safety_checks
       then []
       else [ safety_checks_key, `Array safety_checks ])
;;

let openai_of_action (action : Computer_action.t) =
  let at = function
    | None -> []
    | Some { Computer_action.Point.x; y } -> [ "x", int x; "y", int y ]
  in
  let keys = function
    | None -> []
    | Some k ->
      [ ( "keys"
        , `Array (List.map (String.split k ~on:'+') ~f:(fun k -> `String k)) )
      ]
  in
  let fields =
    match action with
    | Screenshot | Cursor_position | Zoom _ -> [ "type", `String "screenshot" ]
    | Click { at = p; button; count; keys = k } ->
      if count >= 2
      then [ "type", `String "double_click" ] @ at p @ keys k
      else
        [ "type", `String "click"
        ; ( "button"
          , `String
              (match button with
               | Middle -> "wheel"
               | b -> Computer_action.Button.to_string b) )
        ]
        @ at p
        @ keys k
    | Move p | Mouse_down { at = Some p; _ } | Mouse_up { at = Some p; _ } ->
      [ "type", `String "move" ] @ at (Some p)
    | Mouse_down { at = None; _ } | Mouse_up { at = None; _ } ->
      [ "type", `String "wait" ]
    | Drag { path; button = _; keys = k } ->
      [ "type", `String "drag"
      ; ( "path"
        , `Array
            (List.map path ~f:(fun { x; y } ->
               `Object [ "x", int x; "y", int y ])) )
      ]
      @ keys k
    | Scroll { at = p; dx; dy; keys = k } ->
      [ "type", `String "scroll" ]
      @ at p
      @ [ "scroll_x", int (dx * pixels_per_notch)
        ; "scroll_y", int (dy * pixels_per_notch)
        ]
      @ keys k
    | Type text -> [ "type", `String "type"; "text", `String text ]
    | Key combo | Hold_key { combo; _ } ->
      [ "type", `String "keypress" ]
      @ keys (Some (List.hd_exn (String.split combo ~on:' ')))
    | Wait _ -> [ "type", `String "wait" ]
  in
  `Object fields
;;

let to_openai arguments =
  let stored = parse arguments in
  let checks =
    match find stored safety_checks_key with
    | Some (`Array checks) -> checks
    | _ -> []
  in
  let native = native_of stored ~form:"openai" in
  let item_id =
    match Option.bind native ~f:(fun n -> find n "id") with
    | Some (`String id) -> Some id
    | _ -> None
  in
  let action =
    match native with
    | Some native ->
      Option.value
        (find native "action")
        ~default:(`Object [ "type", `String "screenshot" ])
    | None ->
      (match Computer_action.of_json (`Object (without_extras stored)) with
       | Ok action -> openai_of_action action
       | Error _ -> `Object [ "type", `String "screenshot" ])
  in
  action, checks, item_id
;;
