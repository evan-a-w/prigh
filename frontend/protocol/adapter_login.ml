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

type adapter_login = t [@@deriving sexp_of, equal]

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
  | "google-antigravity" -> "Antigravity (Google)"
  | p -> p
;;

let warning t =
  match t.provider with
  | "anthropic" ->
    Some
      "Anthropic's terms keep Claude Pro/Max logins to Claude's own apps: \
       prigh holding one for Claude Code may breach them"
  | _ -> None
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

module Import = struct
  type t =
    { adapter : string
    ; host : string
    ; path : string
    ; label : string
    ; warning : string
    ; confirmed : bool
    ; note : string option
    ; logins : adapter_login list
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind adapter = Json.string_field j "adapter" in
    let%bind host = Json.string_field j "host" in
    let%bind path = Json.string_field j "path" in
    let%bind label = Json.string_field j "label" in
    let%bind warning = Json.string_field j "warning" in
    let%bind note = Json.string_opt_field j "note" in
    let%bind confirmed = Json.bool_default_field j "confirmed" ~default:false in
    let%map logins = list_of_json j in
    { adapter; host; path; label; warning; confirmed; note; logins }
  ;;

  let question t =
    let cli = Session_engine.adapter_label t.adapter in
    String.concat
      ~sep:" "
      ([ sprintf "Import %s's login from %s?" cli t.host; t.warning ]
       @
       match t.adapter with
       | "claude" ->
         [ "Anthropic's terms keep Claude Pro/Max logins to Claude's own apps."
         ]
       | _ -> [])
  ;;

  let done_ t =
    sprintf
      "%s runs on %S from its next turn: %s%s"
      (Session_engine.adapter_label t.adapter)
      t.label
      (if String.equal t.path "keyring"
       then sprintf "its login is cleared from %s's keyring" t.host
       else sprintf "%s's file is now %s.moved-to-prigh" t.host t.path)
      (Option.value_map t.note ~default:"" ~f:(fun n -> " (" ^ n ^ ")"))
  ;;
end
