open! Core
open! Import

module User = struct
  type t =
    { text : string
    ; images : Image.t list
          [@sexp.list] [@jsonaf.default []] [@jsonaf_drop_default.equal]
    ; origin : Injected.t option [@sexp.option] [@jsonaf.option]
    }
  [@@deriving sexp, jsonaf, equal]

  let body t =
    match Injected.of_message ~origin:t.origin t.text with
    | None -> t.text
    | Some origin -> Injected.body origin t.text
  ;;
end

module Assistant = struct
  type t =
    { content : Content.t list
    ; stop_reason : Stop_reason.t
    ; usage : Usage.t
    ; model : string
    }
  [@@deriving sexp, jsonaf, equal]

  let text t =
    List.filter_map t.content ~f:(function
      | Content.Text s -> Some s
      | Thinking _ | Tool_call _ | Server_tool _ -> None)
    |> String.concat
  ;;

  let thinking t =
    List.filter_map t.content ~f:(function
      | Content.Thinking th -> Some th.text
      | Text _ | Tool_call _ | Server_tool _ -> None)
    |> String.concat
  ;;

  let tool_calls t =
    List.filter_map t.content ~f:(function
      | Content.Tool_call c -> Some c
      | Text _ | Thinking _ | Server_tool _ -> None)
  ;;
end

module Tool_result = struct
  type t =
    { tool_call_id : string
    ; tool_name : string
    ; text : string
    ; is_error : bool
    ; images : Image.t list
          [@sexp.list] [@jsonaf.default []] [@jsonaf_drop_default.equal]
    }
  [@@deriving sexp, jsonaf, equal]
end

type t =
  | User of User.t
  | Assistant of Assistant.t
  | Tool_result of Tool_result.t
[@@deriving sexp, jsonaf, equal]

let user ?(images = []) ?origin text = User { text; images; origin }

let with_image_notes text (images : Image.t list) =
  String.concat
    ~sep:"\n"
    ((if String.is_empty text then [] else [ text ])
     @ List.map images ~f:(fun image ->
       sprintf
         "[%s image omitted: this model cannot see images]"
         image.mime_type))
;;

let omit_images = function
  | User ({ text; images = _ :: _ as images; _ } as u) ->
    User { u with text = with_image_notes text images; images = [] }
  | Tool_result ({ text; images = _ :: _ as images; _ } as r) ->
    Tool_result { r with text = with_image_notes text images; images = [] }
  | (User _ | Tool_result _ | Assistant _) as m -> m
;;

(* Anthropic charges about (width * height) / 750 tokens for an image, at
   most about 1600 after its own downscaling. *)
let tokens_per_image = 1600

let estimate_tokens messages =
  let images =
    List.sum
      (module Int)
      messages
      ~f:(function
        | User u -> List.length u.images
        | Tool_result r -> List.length r.images
        | Assistant _ -> 0)
  in
  let chars =
    List.sum
      (module Int)
      messages
      ~f:(function
        | User u -> String.length u.text
        | Tool_result r -> String.length r.text
        | Assistant a ->
          List.sum
            (module Int)
            a.content
            ~f:(function
              | Text s -> String.length s
              | Thinking th -> String.length th.text
              | Tool_call c -> String.length c.arguments + String.length c.name
              | Server_tool s ->
                String.length s.input
                + Option.value_map s.output ~default:0 ~f:String.length))
  in
  (chars / 4) + (images * tokens_per_image)
;;
