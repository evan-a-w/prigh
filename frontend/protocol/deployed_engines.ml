open! Core

type t =
  { engines : string list
  ; adapters : string list [@sexp.list]
  }
[@@deriving sexp_of, equal]

let harness_only = { engines = [ "harness" ]; adapters = [] }
let has_choice t = not (equal t harness_only)

let of_hello_json j =
  let open Or_error.Let_syntax in
  let%bind engines =
    Json.optional_list_field j "engines" ~f:Json.to_string_or_error
  in
  let%map adapters =
    Json.optional_list_field j "adapters" ~f:Json.to_string_or_error
  in
  match engines with
  | [] -> harness_only
  | engines -> { engines; adapters }
;;

module Choice = struct
  type t =
    { engine : string
    ; adapter : string option [@sexp.option]
    }
  [@@deriving sexp_of, equal]

  let key t = Option.value t.adapter ~default:t.engine

  let label t =
    match t.adapter, t.engine with
    | Some adapter, _ -> Session_engine.adapter_label adapter
    | None, "harness" -> "prigh harness"
    | None, engine -> Session_engine.engine_label engine
  ;;

  let describe t =
    match t.adapter, t.engine with
    | Some "codex", _ -> "OpenAI's Codex CLI on the session's tool host"
    | Some "claude", _ -> "Anthropic's Claude Code CLI on the session's tool host"
    | Some "prigh", _ -> "prigh's harness, run by the composer"
    | Some _, _ -> "an ACP agent on the session's tool host"
    | None, "harness" -> "prigh's own agent, calling the model directly"
    | None, _ -> "external agents on the session's tool host"
  ;;

  let of_session (e : Session_engine.t) = { engine = e.engine; adapter = e.adapter }

  let params t =
    ("engine", `String t.engine)
    :: Option.value_map t.adapter ~default:[] ~f:(fun a -> [ "adapter", `String a ])
  ;;
end

let choices t =
  List.concat_map t.engines ~f:(fun engine ->
    match engine, t.adapters with
    | "composer", (_ :: _ as adapters) ->
      List.map adapters ~f:(fun adapter ->
        { Choice.engine; adapter = Some adapter })
    | engine, _ -> [ { Choice.engine; adapter = None } ])
;;

let switch_targets t =
  let choices = choices t in
  let adapters =
    List.filter choices ~f:(fun (c : Choice.t) -> Option.is_some c.adapter)
  in
  match adapters with
  | [] -> []
  | _ ->
    List.filter choices ~f:(fun (c : Choice.t) ->
      Option.is_some c.adapter || String.equal c.engine "harness")
;;

let find_in choices arg =
  let arg = String.lowercase (String.strip arg) in
  let matches (c : Choice.t) =
    String.equal arg (Choice.key c)
    || String.equal arg (String.lowercase (Choice.label c))
    || String.equal arg c.engine
  in
  match List.filter choices ~f:matches with
  | c :: _ -> Ok c
  | [] ->
    (match
       List.filter choices ~f:(fun c ->
         String.is_prefix (Choice.key c) ~prefix:arg
         || String.is_prefix (String.lowercase (Choice.label c)) ~prefix:arg)
     with
     | [ c ] -> Ok c
     | _ ->
       Or_error.errorf
         "no engine %S here: this backend runs %s"
         arg
         (String.concat ~sep:", " (List.map choices ~f:Choice.key)))
;;

let find t arg = find_in (choices t) arg
