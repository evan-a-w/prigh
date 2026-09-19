open! Core
open! Import

module User = struct
  type t = { text : string } [@@deriving sexp, jsonaf, equal]
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
    }
  [@@deriving sexp, jsonaf, equal]
end

type t =
  | User of User.t
  | Assistant of Assistant.t
  | Tool_result of Tool_result.t
[@@deriving sexp, jsonaf, equal]

let user text = User { text }
