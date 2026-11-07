open! Core
open! Import

type t =
  { backend : string
  ; detail : string option
  ; platform : string
  ; output : string option
  ; width : int
  ; height : int
  ; scale : float
  ; scaled_width : int
  ; scaled_height : int
  }
[@@deriving sexp_of, equal]

let of_json json =
  let field name =
    match json with
    | `Object fields -> List.Assoc.find fields ~equal:String.equal name
    | _ -> None
  in
  let string_opt name =
    match field name with
    | Some (`String s) -> Some s
    | _ -> None
  in
  let number name =
    match field name with
    | Some (`Number n) -> Float.of_string_opt n
    | _ -> None
  in
  let int name =
    match number name with
    | Some f when Float.(f >= 1.) -> Ok (Float.iround_nearest_exn f)
    | _ -> Or_error.errorf "computer info: %S must be a positive number" name
  in
  let open Or_error.Let_syntax in
  let%bind backend =
    match string_opt "backend" with
    | Some b -> Ok b
    | None -> Or_error.error_string "computer info: missing \"backend\""
  in
  let%bind width = int "width" in
  let%bind height = int "height" in
  let%bind scaled_width = int "scaled_width" in
  let%bind scaled_height = int "scaled_height" in
  Ok
    { backend
    ; detail = string_opt "detail"
    ; platform = Option.value (string_opt "platform") ~default:"linux"
    ; output = string_opt "output"
    ; width
    ; height
    ; scale = Option.value (number "scale") ~default:1.
    ; scaled_width
    ; scaled_height
    }
;;

let to_json t =
  let int n = `Number (Int.to_string n) in
  `Object
    (List.filter_opt
       [ Some ("backend", `String t.backend)
       ; Option.map t.detail ~f:(fun d -> "detail", `String d)
       ; Some ("platform", `String t.platform)
       ; Option.map t.output ~f:(fun o -> "output", `String o)
       ; Some ("width", int t.width)
       ; Some ("height", int t.height)
       ; Some ("scale", `Number (Float.to_string_hum ~strip_zero:true t.scale))
       ; Some ("scaled_width", int t.scaled_width)
       ; Some ("scaled_height", int t.scaled_height)
       ])
;;

let to_real t ~x ~y =
  let map v ~real ~scaled =
    let r =
      Float.iround_nearest_exn
        (Float.of_int v *. Float.of_int real /. Float.of_int scaled)
    in
    Int.clamp_exn r ~min:0 ~max:(real - 1)
  in
  map x ~real:t.width ~scaled:t.scaled_width, map y ~real:t.height ~scaled:t.scaled_height
;;

let scaled_size ~max_long_side ~max_pixels ~width ~height =
  let w = Float.of_int width
  and h = Float.of_int height in
  let factor =
    Float.min
      1.
      (Float.min
         (Float.of_int max_long_side /. Float.max w h)
         (Float.sqrt (Float.of_int max_pixels /. (w *. h))))
  in
  ( Int.max 1 (Float.iround_down_exn (w *. factor))
  , Int.max 1 (Float.iround_down_exn (h *. factor)) )
;;

let to_string_hum t =
  sprintf
    "%s%s, %d×%d shown as %d×%d"
    t.backend
    (Option.value_map t.detail ~default:"" ~f:(sprintf " (%s)"))
    t.width
    t.height
    t.scaled_width
    t.scaled_height
;;
let feature = "computer"
