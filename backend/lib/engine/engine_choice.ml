open! Core
open! Import

type t =
  { engine : Engine_id.t
  ; adapter : Adapter_id.t option
  }
[@@deriving sexp, equal, compare]

let harness = { engine = Harness; adapter = None }
let is_harness t = equal t harness

let of_strings ~engine ~adapter =
  let open Or_error.Let_syntax in
  let%bind engine = Engine_id.of_string engine in
  let%bind adapter =
    match adapter with
    | None -> Ok None
    | Some a -> Adapter_id.of_string a >>| Option.some
  in
  match engine, adapter with
  | Harness, None | Composer, Some _ -> Ok { engine; adapter }
  | Harness, Some a ->
    Or_error.errorf
      "the harness engine takes no adapter (got %s); use the composer engine \
       to drive %s"
      (Adapter_id.to_string a)
      (Adapter_id.label a)
  | Composer, None ->
    Or_error.error_string
      "the composer engine needs an adapter: codex, claude, prigh or acp:<name>"
;;

let label t =
  match t.adapter with
  | None -> Engine_id.to_string t.engine
  | Some a ->
    sprintf "%s (%s)" (Engine_id.to_string t.engine) (Adapter_id.label a)
;;

let key t =
  match t.adapter with
  | Some a -> Adapter_id.to_string a
  | None -> Engine_id.to_string t.engine
;;

let display_name t =
  match t.adapter with
  | Some a -> Adapter_id.label a
  | None ->
    (match t.engine with
     | Harness -> "prigh harness"
     | Composer -> "composer")
;;

let names t =
  match t.engine, t.adapter with
  | Harness, _ | Composer, Some Prigh ->
    [ "harness"; "prigh harness"; "native" ]
  | Composer, Some Codex -> [ "codex" ]
  | Composer, Some Claude -> [ "claude"; "claude code" ]
  | Composer, Some (Acp name) ->
    let base = [ name; "acp:" ^ name; Adapter_id.label (Acp name) ] in
    if String.equal name "antigravity" then "agy" :: base else base
  | Composer, Some (Backend (Acp name) as a) ->
    let base =
      [ name ^ "-bridge"
      ; "bridge:" ^ name
      ; "bridge:acp:" ^ name
      ; name ^ "-backend"
      ; "backend:" ^ name
      ; "backend:acp:" ^ name
      ; Adapter_id.label a
      ]
    in
    if String.equal name "antigravity"
    then "agy-bridge" :: "agy-backend" :: base
    else base
  | Composer, Some (Backend inner as a) ->
    [ Adapter_id.to_string a
    ; Adapter_id.to_string inner ^ "-bridge"
    ; "bridge:" ^ Adapter_id.to_string inner
    ; Adapter_id.to_string inner ^ "-backend"
    ; Adapter_id.label a
    ]
  | Composer, None -> [ "composer" ]
;;
