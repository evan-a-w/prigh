open! Core
open! Import

module Key = struct
  type t =
    | Keep
    | Set of string
    | Clear
  [@@deriving sexp_of]
end

let ( let* ) = Or_error.( >>= )

(* An empty answer takes the default: the CLI and pi-web cannot prefill. *)
let ask_text
      (i : Auth_interaction.t)
      ~message
      ~placeholder
      ~default
      ~validate
      ()
  =
  let rec loop ~error ~default =
    let message =
      match error with
      | None -> message
      | Some e -> sprintf "%s\n%s" e message
    in
    let* answer = i.prompt (Text { message; placeholder; default }) in
    let answer = String.strip answer in
    let answer = if String.is_empty answer then default else answer in
    match validate answer with
    | Ok v -> Ok v
    | Error e -> loop ~error:(Some (Error.to_string_hum e)) ~default:answer
  in
  loop ~error:None ~default
;;

let select (i : Auth_interaction.t) ~message options =
  let* answer = i.prompt (Select { message; options }) in
  Ok
    (match List.find options ~f:(fun (id, _) -> String.equal id answer) with
     | Some (id, _) -> id
     | None -> fst (List.hd_exn options))
;;

let ask_name i ~models ~store =
  ask_text
    i
    ~message:
      "Name of the provider (models are then <name>/<model id>; an existing \
       custom provider's name edits it)"
    ~placeholder:"aiproxy"
    ~default:""
    ~validate:(fun answer ->
      let* name = Custom_provider.validate_name answer in
      match Model_registry.find_provider models name with
      | Some _ -> Ok name
      | None ->
        let* taken = Auth_store.mem store name in
        if taken
        then
          Or_error.errorf
            "auth.json already has an entry named %S (another tool, such as \
             pi, may use it): choose another name"
            name
        else Ok name)
    ()
;;

let ask_base_url (i : Auth_interaction.t) ~name ~default =
  let* url =
    ask_text
      i
      ~message:
        (sprintf
           "Base URL of %s's API (the part before /chat/completions, usually \
            ending in /v1)"
           name)
      ~placeholder:"http://localhost:3000/v1"
      ~default
      ~validate:(fun answer ->
        Or_error.map (Custom_provider.validate_base_url answer) ~f:(fun url ->
          url, answer))
      ()
  in
  let url, typed = url in
  let typed = String.rstrip typed ~drop:(Char.equal '/') in
  if not (String.equal url typed)
  then
    i.notify
      (Progress
         (sprintf "Using %s (the endpoint paths are added per request)" url));
  Ok url
;;

let ask_api i ~name ~(current : Custom_provider.Api.t) =
  let ordered =
    current
    :: List.filter
         Custom_provider.Api.all
         ~f:(Fn.non (Custom_provider.Api.equal current))
  in
  let* id =
    select
      i
      ~message:(sprintf "API style of %s" name)
      (List.map ordered ~f:(fun api ->
         ( Custom_provider.Api.to_string api
         , Custom_provider.Api.label api
           ^
           match api with
           | Chat -> " - most servers"
           | Responses | Anthropic -> "" )))
  in
  Ok (Option.value_exn (Custom_provider.Api.of_string id))
;;

let ask_key (i : Auth_interaction.t) ~name ~stored ~getenv =
  match stored with
  | Some _ ->
    let* choice =
      select
        i
        ~message:(sprintf "%s has a stored API key" name)
        [ "keep", "Keep the stored key"
        ; "new", "Enter a new key"
        ; "none", "Remove it (the server needs no key)"
        ]
    in
    (match choice with
     | "new" ->
       let* key =
         i.prompt
           (Secret
              { message = sprintf "API key for %s" name; allow_empty = false })
       in
       Ok
         (if String.is_empty (String.strip key)
          then Key.Keep
          else Set (String.strip key))
     | "none" -> Ok Key.Clear
     | _ -> Ok Key.Keep)
  | None ->
    let var = Custom_provider.env_var name in
    let empty =
      match getenv var with
      | Some v when not (String.is_empty v) ->
        sprintf "leave empty to use $%s" var
      | _ -> "leave empty if the server needs none"
    in
    let* key =
      i.prompt
        (Secret
           { message = sprintf "API key for %s (%s)" name empty
           ; allow_empty = true
           })
    in
    let key = String.strip key in
    Ok (if String.is_empty key then Key.Keep else Set key)
;;

let effective_key ~(key : Key.t) ~stored ~getenv ~name =
  match key with
  | Set k -> Some k
  | Clear -> None
  | Keep ->
    (match stored with
     | Some k -> Some k
     | None ->
       Option.filter
         (getenv (Custom_provider.env_var name))
         ~f:(Fn.non String.is_empty))
;;

let summary (listed : Custom_provider.Listed_model.t list) =
  let ids = List.map listed ~f:(fun l -> l.id) in
  let shown = List.take ids 8 in
  sprintf
    "%s%s"
    (String.concat ~sep:", " shown)
    (if List.length ids > List.length shown
     then sprintf ", ... (%d more)" (List.length ids - List.length shown)
     else "")
;;

let login ~env ~models ~store ~getenv ?name (i : Auth_interaction.t) =
  Auth_interaction.run i ~f:(fun () ->
    let* home =
      match Model_registry.home models with
      | Some home -> Ok home
      | None -> Or_error.error_string "custom providers are not available here"
    in
    let* name =
      match name with
      | Some name -> Ok name
      | None -> ask_name i ~models ~store
    in
    let existing = Model_registry.find_provider models name in
    let* stored =
      match Auth_store.read store (Provider_id.Custom name) with
      | Ok (Some (Api_key k)) -> Ok (Some k)
      | Ok None -> Ok None
      | Ok (Some (Oauth _)) ->
        Or_error.errorf
          "auth.json's %S entry is an OAuth login; choose another name or \
           /logout %s"
          name
          name
      | Error e -> Error e
    in
    let rec settings ~base_url ~api =
      let* base_url = ask_base_url i ~name ~default:base_url in
      let* api = ask_api i ~name ~current:api in
      let* key = ask_key i ~name ~stored ~getenv in
      let candidate : Custom_provider.t =
        { name
        ; base_url
        ; api
        ; headers =
            Option.value_map existing ~default:[] ~f:(fun p -> p.headers)
        ; models = Option.value_map existing ~default:[] ~f:(fun p -> p.models)
        }
      in
      i.notify (Progress (sprintf "Checking %s/models ..." base_url));
      match
        Model_registry.fetch_models
          ~env
          ~cancel:i.cancel
          candidate
          ~key:(effective_key ~key ~stored ~getenv ~name)
      with
      | Ok listed ->
        i.notify
          (Progress
             (match listed with
              | [] ->
                sprintf
                  "The server listed no models; name them under \
                   providers.%s.models in config.json"
                  name
              | listed ->
                sprintf
                  "Found %d models: %s"
                  (List.length listed)
                  (summary listed)));
        Ok (candidate, key, Some listed)
      | Error e ->
        let* choice =
          select
            i
            ~message:
              (sprintf
                 "Could not list %s's models: %s\n\
                  Check the base URL (it usually ends in /v1), the API key, \
                  and that the server is running."
                 name
                 (Error.to_string_hum e))
            [ "save", "Save anyway"
            ; "edit", "Change the settings"
            ; "cancel", "Cancel (nothing is saved)"
            ]
        in
        (match choice with
         | "save" -> Ok (candidate, key, None)
         | "edit" -> settings ~base_url ~api
         | _ -> Auth_interaction.cancelled ())
    in
    let* provider, key, listed =
      settings
        ~base_url:
          (Option.value_map existing ~default:"" ~f:(fun p -> p.base_url))
        ~api:
          (Option.value_map
             existing
             ~default:Custom_provider.Api.Chat
             ~f:(fun p -> p.api))
    in
    let* () = Custom_provider.save ~home provider in
    let id = Custom_provider.provider_id provider in
    let* () =
      match key with
      | Keep -> Ok ()
      | Set k -> Auth_store.set store id (Api_key k)
      | Clear -> Auth_store.remove store id
    in
    Model_registry.reload
      ?listed:(Option.map listed ~f:(fun l -> name, l))
      models;
    Ok provider)
;;

module Logout = struct
  type t =
    | Key_removed
    | Provider_removed
    | Kept
  [@@deriving sexp_of]
end

let logout ~models ~store name (i : Auth_interaction.t) =
  let id = Provider_id.Custom name in
  match
    Model_registry.find_provider models name, Model_registry.home models
  with
  | None, _ | _, None ->
    Or_error.map (Auth_store.remove store id) ~f:(fun () -> Logout.Key_removed)
  | Some p, Some home ->
    Auth_interaction.run i ~f:(fun () ->
      let* stored = Auth_store.read store id in
      let* choice =
        select
          i
          ~message:(sprintf "Log out of %s (%s)" name p.base_url)
          (match stored with
           | Some _ ->
             [ "key", "Remove the API key only (keep the provider)"
             ; "all", "Remove the API key and the provider"
             ]
           | None ->
             [ "all", "Remove the provider from config.json"
             ; "keep", "Keep it"
             ])
      in
      let* () = Auth_store.remove store id in
      match choice with
      | "all" ->
        let* () = Custom_provider.remove ~home name in
        Model_registry.reload models;
        Ok Logout.Provider_removed
      | "key" -> Ok Logout.Key_removed
      | _ -> Ok Logout.Kept)
;;
