open! Core
open! Import

type t =
  | Own
  | Prigh of Account_ref.t
[@@deriving sexp_of, equal]

(* Backend-hosted adapters share the login configuration of their underlying
   adapter (e.g. [backend:acp:antigravity] uses [acp:antigravity]'s login). *)
let canonical adapter =
  Option.value (String.chop_prefix adapter ~prefix:"backend:") ~default:adapter
;;

let provider ~adapter : Provider_id.t option =
  match canonical adapter with
  | "codex" -> Some Openai_codex
  | "claude" -> Some Anthropic
  | "acp:antigravity" -> Some Google_antigravity
  | _ -> None
;;

let adapters = [ "codex"; "claude"; "acp:antigravity" ]

let to_json = function
  | Own -> `Null
  | Prigh ref_ -> `String (Account_ref.to_string ref_)
;;

let of_json : Json.t -> t Or_error.t = function
  | `Null -> Ok Own
  | `String s -> Or_error.map (Account_ref.of_string s) ~f:(fun r -> Prigh r)
  | _ ->
    Or_error.error_string "an adapter's login must be a login's ref or null"
;;

let adapters_field fields =
  match List.Assoc.find fields ~equal:String.equal "adapters" with
  | Some (`Object adapters) -> adapters
  | _ -> []
;;

let load ~home =
  match Config.read_fields ~home with
  | Error _ -> []
  | Ok fields ->
    List.filter_map (adapters_field fields) ~f:(fun (adapter, settings) ->
      match settings with
      | `Object settings ->
        (match List.Assoc.find settings ~equal:String.equal "login" with
         | Some login ->
           Result.ok (of_json login)
           |> Option.map ~f:(fun login -> adapter, login)
         | None -> None)
      | _ -> None)
;;

let find ~home ~adapter =
  let adapter = canonical adapter in
  List.Assoc.find (load ~home) ~equal:String.equal adapter
  |> Option.value ~default:Own
;;

let save ~home ~adapter login =
  let adapter = canonical adapter in
  let open Or_error.Let_syntax in
  let%bind () =
    match login, provider ~adapter with
    | Own, _ | Prigh _, Some _ -> Ok ()
    | Prigh _, None ->
      Or_error.errorf
        "%s runs only on its own login: prigh cannot hand it one of its logins \
         (yet)"
        adapter
  in
  let%bind fields = Config.read_fields ~home in
  let adapters = adapters_field fields in
  let settings =
    match List.Assoc.find adapters ~equal:String.equal adapter with
    | Some (`Object settings) ->
      List.Assoc.remove settings ~equal:String.equal "login"
    | _ -> []
  in
  let settings =
    match login with
    | Own -> settings
    | Prigh _ -> settings @ [ "login", to_json login ]
  in
  let adapters =
    List.Assoc.remove adapters ~equal:String.equal adapter
    @ if List.is_empty settings then [] else [ adapter, `Object settings ]
  in
  let fields =
    List.Assoc.remove fields ~equal:String.equal "adapters"
    @ if List.is_empty adapters then [] else [ "adapters", `Object adapters ]
  in
  Config.write_fields ~home fields
;;
