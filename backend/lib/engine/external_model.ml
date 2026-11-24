open! Core
open! Import

let fixed = [ "codex"; "claude" ]

let is_adapter adapter =
  List.mem fixed adapter ~equal:String.equal
  || String.is_prefix adapter ~prefix:"acp:"
;;

let default_id = "default"

let name ~adapter model =
  sprintf "%s/%s" adapter (Option.value model ~default:default_id)
;;

let is_external name =
  match String.lsplit2 name ~on:'/' with
  | Some (adapter, _) -> is_adapter adapter
  | None -> false
;;

(* Big enough for any harness's models; the harness's reports correct it. *)
let fallback_window = 200_000

let model
      ~adapter
      ?name
      ?(context_window = fallback_window)
      ?(thinking_levels = [])
      ?(can_disable_thinking = false)
      ?(images = true)
      id
  : Model.t
  =
  { id
  ; provider = Custom adapter
  ; name =
      Option.value
        name
        ~default:(if String.equal id default_id then "Default" else id)
  ; context_window
  ; max_output = 0
  ; thinking_levels
  ; can_disable_thinking
  ; thinking_default = None
  ; thinking_style = Effort { off_is_none = false }
  ; cost = { input = 0.; output = 0.; cache_read = 0.; long_prompt = None }
  ; supports_images = images
  ; native_computer = None
  ; legacy = false
  }
;;

(* Before the harness lists its models: the levels every model of it takes
   (Claude Code's are budgets prigh sets). *)
let default ~adapter =
  match adapter with
  | "claude" ->
    model
      ~adapter
      ~thinking_levels:[ Low; Medium; High; Max ]
      ~can_disable_thinking:true
      default_id
  | "codex" -> model ~adapter ~thinking_levels:[ Low; Medium; High ] default_id
  | _ -> model ~adapter default_id
;;

let of_adapter ~adapter (m : Model.t) =
  match m.provider with
  | Custom a -> String.equal a adapter
  | Anthropic | Openai | Openai_codex | Deepseek -> false
;;

let native_id (m : Model.t) =
  match m.provider with
  | Custom a when is_adapter a && not (String.equal m.id default_id) ->
    Some m.id
  | Custom _ | Anthropic | Openai | Openai_codex | Deepseek -> None
;;

let restore ~adapter ~models key =
  match String.chop_prefix key ~prefix:(adapter ^ "/") with
  | None -> None
  | Some id ->
    (match List.find models ~f:(fun (m : Model.t) -> String.equal m.id id) with
     | Some m -> Some m
     | None -> Some (model ~adapter id))
;;
