open! Core

type t =
  | Native
  | Bridge
  | Host
[@@deriving sexp, equal, compare]

let all = [ Native; Bridge; Host ]

let to_string = function
  | Native -> "native"
  | Bridge -> "bridge"
  | Host -> "host"
;;

let of_string s =
  match String.lowercase (String.strip s) with
  | "native" -> Some Native
  | "bridge" -> Some Bridge
  | "host" -> Some Host
  | _ -> None
;;

let label = function
  | Native -> "Native"
  | Bridge -> "Bridge"
  | Host -> "Host"
;;

let describe = function
  | Native -> "prigh's built-in harness, calling the model directly"
  | Bridge ->
    "external agent running on the prigh server, bridging tools to the host \
     over MCP"
  | Host -> "external agent running directly on the active tool host"
;;

let of_adapter = function
  | None | Some "prigh" -> Native
  | Some s when String.is_prefix s ~prefix:"backend:" -> Bridge
  | Some _ -> Host
;;

let of_provider provider =
  if String.is_prefix provider ~prefix:"backend:"
  then Bridge
  else if
    String.equal provider "codex"
    || String.equal provider "claude"
    || String.is_prefix provider ~prefix:"acp:"
  then Host
  else Native
;;

let of_engine (engine : Session_engine.t) =
  match engine.adapter with
  | Some _ as a -> of_adapter a
  | None -> if String.equal engine.engine "composer" then Host else Native
;;

let deployed_modes (deployed : Deployed_engines.t) =
  let has_native = List.mem deployed.engines "harness" ~equal:String.equal in
  let has_bridge =
    List.exists deployed.adapters ~f:(String.is_prefix ~prefix:"backend:")
  in
  let has_host =
    List.exists deployed.adapters ~f:(fun a ->
      (not (String.is_prefix a ~prefix:"backend:"))
      && not (String.equal a "prigh"))
  in
  List.filter all ~f:(function
    | Native -> has_native
    | Bridge -> has_bridge
    | Host -> has_host)
;;

let choices_for_mode mode (deployed : Deployed_engines.t) =
  List.filter
    (Deployed_engines.choices deployed)
    ~f:(fun (c : Deployed_engines.Choice.t) ->
      equal (of_adapter c.adapter) mode)
;;

let format_model ~name ~provider =
  sprintf "%s @ %s" name (to_string (of_provider provider))
;;
