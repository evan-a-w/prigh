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

(* The windows of the model families harnesses run, by a word of their id
   or name (the first that matches): Gemini 3.x has 1M tokens, Claude 4.x 200k, GPT-OSS 128k. *)
let known_windows =
  [ "1m context", 1_000_000
  ; "gemini", 1_048_576
  ; "claude", 200_000
  ; "sonnet", 200_000
  ; "opus", 200_000
  ; "haiku", 200_000
  ; "gpt-oss", 131_072
  ]
;;

let known_window ~id ~name =
  let text = String.lowercase (id ^ " " ^ Option.value name ~default:"") in
  List.find_map known_windows ~f:(fun (word, window) ->
    Option.some_if (String.is_substring text ~substring:word) window)
;;

(* Codex and Claude Code say their window after a turn: until then, one big
   enough for their models. An ACP agent's model of no known family has no
   window until its agent says (0). *)
let window ~adapter ~id ~name =
  match known_window ~id ~name with
  | Some window -> window
  | None -> if List.mem fixed adapter ~equal:String.equal then 200_000 else 0
;;

let model
      ~adapter
      ?name
      ?context_window
      ?(thinking_levels = [])
      ?(can_disable_thinking = false)
      ?(images = true)
      id
  : Model.t
  =
  { id
  ; provider = Custom adapter
  ; name = Option.value name ~default:id
  ; context_window =
      Option.value_or_thunk context_window ~default:(fun () ->
        window ~adapter ~id ~name)
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
let placeholder ~adapter ~name =
  match adapter with
  | "claude" ->
    model
      ~adapter
      ~name
      ~thinking_levels:[ Low; Medium; High; Max ]
      ~can_disable_thinking:true
      default_id
  | "codex" -> model ~adapter ~name ~thinking_levels:[ Low; Medium; High ] default_id
  | _ -> model ~adapter ~name default_id
;;

let pending ~adapter = placeholder ~adapter ~name:"loading models…"
let unlisted ~adapter = placeholder ~adapter ~name:"its own model"
let is_placeholder (m : Model.t) = String.equal m.id default_id
;;

let of_adapter ~adapter (m : Model.t) =
  match m.provider with
  | Custom a -> String.equal a adapter
  | Anthropic | Openai | Openai_codex | Deepseek | Google_antigravity -> false
;;

let native_id (m : Model.t) =
  match m.provider with
  | Custom a when is_adapter a && not (String.equal m.id default_id) ->
    Some m.id
  | Custom _ | Anthropic | Openai | Openai_codex | Deepseek | Google_antigravity
    -> None
;;

let restore ~adapter ~models key =
  match String.chop_prefix key ~prefix:(adapter ^ "/") with
  | None -> None
  | Some id ->
    (match List.find models ~f:(fun (m : Model.t) -> String.equal m.id id) with
     | Some m -> Some m
     | None when String.equal id default_id ->
       (* Recorded before prigh named the harness's default: the model it
          stands for, once listed. *)
       List.find models ~f:(Fn.non is_placeholder)
     | None -> Some (model ~adapter id))
;;

let same_model a b =
  let slug s =
    String.lowercase s
    |> String.map ~f:(fun c ->
      if Char.is_alphanum c || Char.equal c '.' then c else '-')
    |> fun s -> Option.value (String.chop_prefix s ~prefix:"claude-") ~default:s
  in
  let longer_by a b =
    match String.chop_prefix b ~prefix:(a ^ "-") with
    | None -> false
    | Some rest ->
      String.is_prefix rest ~prefix:"-"
      || (String.length rest = 8 && String.for_all rest ~f:Char.is_digit)
  in
  let a = slug a
  and b = slug b in
  String.equal a b || longer_by a b || longer_by b a
;;
