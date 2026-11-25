open! Core

module Choice = struct
  type t =
    { ref_ : string
    ; label : string
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind ref_ = Json.string_field j "ref" in
    let%map label = Json.string_field j "label" in
    { ref_; label }
  ;;
end

type t =
  { adapter : string
  ; provider : string
  ; login : Choice.t option
  ; choices : Choice.t list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind adapter = Json.string_field j "adapter" in
  let%bind provider = Json.string_field j "provider" in
  let%bind login = Json.string_opt_field j "login" in
  let%bind label = Json.string_opt_field j "label" in
  let%map choices = Json.optional_list_field j "choices" ~f:Choice.of_json in
  { adapter
  ; provider
  ; login =
      Option.map login ~f:(fun ref_ ->
        { Choice.ref_; label = Option.value label ~default:ref_ })
  ; choices
  }
;;

let list_of_json j = Json.optional_list_field j "adapter_logins" ~f:of_json
let own t = Session_engine.adapter_label t.adapter ^ "'s own login"

let provider_name t =
  match t.provider with
  | "openai-codex" -> "ChatGPT"
  | "anthropic" -> "Claude Pro/Max"
  | p -> p
;;

let describe t =
  match t.login with
  | None -> own t
  | Some c -> sprintf "%s (prigh) · %s" (provider_name t) c.label
;;

let parse_choice t arg =
  let arg = String.strip arg in
  if String.Caseless.equal arg "own"
  then Ok None
  else (
    match
      List.find t.choices ~f:(fun c ->
        String.equal c.ref_ arg || String.Caseless.equal c.label arg)
    with
    | Some c -> Ok (Some c)
    | None ->
      Or_error.errorf
        "%s has no login %S: choose own%s"
        (provider_name t)
        arg
        (String.concat
           (List.map t.choices ~f:(fun c ->
              if String.equal c.ref_ c.label
              then ", " ^ c.ref_
              else sprintf ", %s (%s)" c.label c.ref_))))
;;
