open! Core

module Transport = struct
  type t =
    | Stdio of
        { command : string
        ; args : string list
        ; env : (string * string) list
        }
    | Http of
        { url : string
        ; headers : (string * string) list
        }
  [@@deriving sexp_of, equal]

  let sexp_of_t t =
    let hide = List.map ~f:(fun (k, _) -> k, "<hidden>") in
    sexp_of_t
      (match t with
       | Stdio s -> Stdio { s with env = hide s.env }
       | Http h -> Http { h with headers = hide h.headers })
  ;;

  let pairs_json pairs =
    Json.obj (List.map pairs ~f:(fun (k, v) -> k, Json.str v))
  ;;

  let to_json = function
    | Stdio { command; args; env } ->
      Json.obj
        ([ "command", Json.str command ]
         @ (if List.is_empty args
            then []
            else [ "args", `Array (List.map args ~f:Json.str) ])
         @ if List.is_empty env then [] else [ "env", pairs_json env ])
    | Http { url; headers } ->
      Json.obj
        ([ "type", Json.str "http"; "url", Json.str url ]
         @
         if List.is_empty headers then [] else [ "headers", pairs_json headers ]
        )
  ;;

  let pairs j name =
    match Json.field j name with
    | None -> Ok []
    | Some (`Object fields) ->
      List.map fields ~f:(fun (k, v) ->
        match v with
        | `String s -> Ok (k, s)
        | _ -> Or_error.errorf "%s.%s must be a string" name k)
      |> Or_error.all
    | Some _ -> Or_error.errorf "%s must be an object" name
  ;;

  let of_json j =
    let open Or_error.Let_syntax in
    match Json.field j "url" with
    | Some (`String url) ->
      let%map headers = pairs j "headers" in
      Http { url; headers }
    | _ ->
      let%bind command = Json.string_field j "command" in
      let%bind args =
        Json.optional_list_field j "args" ~f:(function
          | `String s -> Ok s
          | _ -> Or_error.error_string "args must be strings")
      in
      let%map env = pairs j "env" in
      Stdio { command; args; env }
  ;;

  let quote arg =
    if
      (not (String.is_empty arg))
      && String.for_all arg ~f:(fun c ->
        Char.is_alphanum c || String.mem "-_./:=@%+,${}" c)
    then arg
    else "'" ^ String.substr_replace_all arg ~pattern:"'" ~with_:"'\\''" ^ "'"
  ;;

  let summary = function
    | Stdio { command; args; _ } ->
      String.concat ~sep:" " (List.map (command :: args) ~f:quote)
    | Http { url; _ } -> url
  ;;
end

module Server = struct
  type t =
    { name : string
    ; enabled : bool
    ; transport : Transport.t
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind name = Json.string_field j "name" in
    let%bind enabled = Json.bool_field j "enabled" in
    let%bind config = Json.object_field j "config" in
    let%map transport =
      Transport.of_json config |> Or_error.tag ~tag:(sprintf "server %S" name)
    in
    { name; enabled; transport }
  ;;
end

type t = Server.t list [@@deriving sexp_of, equal]

let of_json j = Json.list_field j "servers" ~f:Server.of_json

let find t name =
  List.find t ~f:(fun (s : Server.t) -> String.equal s.name name)
;;

let source = "settings"
