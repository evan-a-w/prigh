open! Core
open! Import

let mb = 1_048_576

let known (model : Model.t) : Image.Limit.t =
  match model.provider with
  | Openai | Openai_codex ->
    { max_base64_bytes = 18 * mb
    ; max_dimension = Image.Limit.default.max_dimension
    }
  | Anthropic | Deepseek | Custom _ -> Image.Limit.default
;;

let learned : Image.Limit.t String.Table.t = String.Table.create ()
let reset () = Hashtbl.clear learned

let for_model model =
  match Hashtbl.find learned (Model.key model) with
  | Some limit -> Image.Limit.min limit (known model)
  | None -> known model
;;

let many_images = 20
let many_image_dimension = 2000

let for_request (model : Model.t) ~images =
  let limit = for_model model in
  match model.provider with
  | Anthropic when images > many_images ->
    { limit with
      max_dimension = Int.min limit.max_dimension many_image_dimension
    }
  | Anthropic | Openai | Openai_codex | Deepseek | Custom _ -> limit
;;

let re s = Re.compile (Re.no_case (Re.Perl.re s))

let patterns =
  List.map
    ~f:re
    [ {|image[^.]*\bexceeds?\b|}
    ; {|image[^.]*\btoo (large|big)\b|}
    ; {|image dimensions exceed|}
    ; {|image[^.]*\bmaximum (allowed )?size\b|}
    ; {|image[^.]*\bsize limit\b|}
    ]
;;

let matches error = List.exists patterns ~f:(fun p -> Re.execp p error)

let number_before unit error =
  match Re.exec_opt (re (sprintf {|(\d+(?:\.\d+)?)\s*%s\b|} unit)) error with
  | None -> None
  | Some g -> Float.of_string_opt (Re.Group.get g 1)
;;

(* "5308416 bytes > 5242880 bytes": the limit is the last number of bytes. *)
let last_bytes error =
  Re.all (re {|(\d+)\s*bytes\b|}) error
  |> List.filter_map ~f:(fun g -> Int.of_string_opt (Re.Group.get g 1))
  |> List.last
;;

let of_error error ~(current : Image.Limit.t) =
  if not (matches error)
  then None
  else (
    let margin bytes = Float.iround_down_exn (Float.of_int bytes *. 0.9) in
    let bytes =
      match last_bytes error with
      | Some b -> Some (margin b)
      | None ->
        Option.map (number_before "MB" error) ~f:(fun m ->
          margin (Float.iround_down_exn (m *. Float.of_int mb)))
    in
    let pixels =
      Option.map (number_before "(?:pixels|px)" error) ~f:Float.iround_down_exn
    in
    let stated : Image.Limit.t =
      { max_base64_bytes =
          Option.value_map bytes ~default:current.max_base64_bytes ~f:(fun b ->
            Int.min b current.max_base64_bytes)
      ; max_dimension =
          Option.value_map pixels ~default:current.max_dimension ~f:(fun p ->
            Int.min p current.max_dimension)
      }
    in
    if Image.Limit.equal stated current
    then Some (Image.Limit.tighten current)
    else Some stated)
;;

let learn model limit =
  Hashtbl.update learned (Model.key model) ~f:(function
    | None -> limit
    | Some old -> Image.Limit.min old limit)
;;

let tighten model =
  let limit = Image.Limit.tighten (for_model model) in
  learn model limit;
  for_model model
;;
