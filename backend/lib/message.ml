open! Core
open! Import

module User = struct
  type t =
    { text : string
    ; images : Image.t list
          [@sexp.list] [@jsonaf.default []] [@jsonaf_drop_default.equal]
    }
  [@@deriving sexp, jsonaf, equal]
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
      | Thinking _ | Tool_call _ -> None)
    |> String.concat
  ;;

  let thinking t =
    List.filter_map t.content ~f:(function
      | Content.Thinking th -> Some th.text
      | Text _ | Tool_call _ -> None)
    |> String.concat
  ;;

  let tool_calls t =
    List.filter_map t.content ~f:(function
      | Content.Tool_call c -> Some c
      | Text _ | Thinking _ -> None)
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

let user ?(images = []) text = User { text; images }

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
  | User { text; images = _ :: _ as images } ->
    User { text = with_image_notes text images; images = [] }
  | Tool_result ({ text; images = _ :: _ as images; _ } as r) ->
    Tool_result { r with text = with_image_notes text images; images = [] }
  | (User _ | Tool_result _ | Assistant _) as m -> m
;;
