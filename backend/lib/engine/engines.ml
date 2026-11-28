open! Core
open! Import

type t =
  { engines : Engine_id.t list
  ; adapters : Adapter_id.t list
  ; acp_agents : Acp_agent.t list
  }
[@@deriving sexp_of, equal]

let default = { engines = [ Harness ]; adapters = []; acp_agents = [] }
let default_adapters : Adapter_id.t list = [ Codex; Claude ]

let create ~engines ?(adapters = default_adapters) ?(acp_agents = []) () =
  let engines = List.stable_dedup engines ~compare:Engine_id.compare in
  let adapters = List.stable_dedup adapters ~compare:Adapter_id.compare in
  let composer = List.mem engines Composer ~equal:Engine_id.equal in
  if List.is_empty engines
  then Or_error.error_string "no engines: give harness, composer or both"
  else if composer && List.is_empty adapters
  then
    Or_error.error_string
      "the composer engine needs at least one adapter: codex, claude, prigh or \
       acp:<name>"
  else
    Ok { engines; adapters = (if composer then adapters else []); acp_agents }
;;

let split s =
  String.split s ~on:','
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> not (String.is_empty s))
;;

(* [acp:name=command args] (or [backend:acp:name=command args]) names an ACP
   agent and how to start it. *)
let adapter_of_flag s =
  let open Or_error.Let_syntax in
  let on_backend, rest =
    match String.chop_prefix s ~prefix:"backend:" with
    | Some rest -> true, rest
    | None -> false, s
  in
  let wrap id = if on_backend then Adapter_id.Backend id else id in
  match String.chop_prefix rest ~prefix:"acp:" with
  | Some spec when String.mem spec '=' ->
    let%map agent = Acp_agent.of_spec spec in
    wrap (Adapter_id.Acp agent.name), Some agent
  | _ ->
    let%map id = Adapter_id.of_string s in
    id, None
;;

let of_flags ~engines ~adapters ~getenv =
  let open Or_error.Let_syntax in
  let from flag var =
    match flag with
    | Some _ -> flag
    | None ->
      Option.filter (getenv var) ~f:(fun s ->
        not (String.is_empty (String.strip s)))
  in
  match from engines "PRIGH_ENGINES", from adapters "PRIGH_ADAPTERS" with
  | None, None -> Ok default
  | None, Some a ->
    Or_error.errorf
      "adapters (%s) are for the composer engine: add -engines composer (or \
       harness,composer)"
      a
  | Some e, a ->
    let%bind engines =
      Or_error.combine_errors (List.map (split e) ~f:Engine_id.of_string)
    in
    let%bind adapters =
      match a with
      | None -> Ok None
      | Some a ->
        Or_error.combine_errors (List.map (split a) ~f:adapter_of_flag)
        >>| Option.some
    in
    let acp_agents =
      Option.value_map adapters ~default:[] ~f:(List.filter_map ~f:snd)
    in
    let adapters = Option.map adapters ~f:(List.map ~f:fst) in
    let composer = List.mem engines Composer ~equal:Engine_id.equal in
    (match adapters with
     | Some (_ :: _ as adapters) when not composer ->
       Or_error.errorf
         "adapters (%s) are for the composer engine: add composer to the \
          engines"
         (String.concat ~sep:"," (List.map adapters ~f:Adapter_id.to_string))
     | _ -> create ~engines ?adapters ~acp_agents ())
;;

let acp_agent t name =
  match
    List.find t.acp_agents ~f:(fun (a : Acp_agent.t) ->
      String.equal a.name name)
  with
  | Some _ as a -> a
  | None -> Acp_agent.find_known name
;;

let drivable t : Adapter_id.t -> bool = function
  | Codex | Claude | Prigh -> true
  | Acp name -> Option.is_some (acp_agent t name)
  | Backend (Acp "antigravity") -> Option.is_some (acp_agent t "antigravity")
  | Backend _ -> false
;;

let unknown_acp name =
  sprintf
    "prigh does not know how to start the ACP agent %s: give its command with \
     -adapters acp:%s=<command and args> (it knows %s)"
    name
    name
    (String.concat ~sep:", " (List.map Acp_agent.known ~f:(fun a -> a.name)))
;;

let check_build t =
  match List.filter t.adapters ~f:(Fn.non (drivable t)) with
  | [] -> Ok ()
  | missing ->
    Or_error.error_string
      (String.concat
         ~sep:"; "
         (List.map missing ~f:(function
            | Acp name -> unknown_acp name
            | Backend inner ->
              sprintf
                "the composer can't run %s on the backend yet (only \
                 backend:acp:antigravity is supported)"
                (Adapter_id.label inner)
            | a -> sprintf "the composer can't drive %s" (Adapter_id.label a))))
;;

let engine_strings t = List.map t.engines ~f:Engine_id.to_string
let adapter_strings t = List.map t.adapters ~f:Adapter_id.to_string

let default_choice t : Engine_choice.t =
  match List.hd_exn t.engines with
  | Harness -> Engine_choice.harness
  | Composer -> { engine = Composer; adapter = List.hd t.adapters }
;;

let choices t =
  List.concat_map t.engines ~f:(function
    | Harness -> [ Engine_choice.harness ]
    | Composer ->
      List.map t.adapters ~f:(fun adapter ->
        { Engine_choice.engine = Composer; adapter = Some adapter }))
;;

let resolve t raw =
  let arg = String.lowercase (String.strip raw) in
  let all = choices t in
  let keys = String.concat ~sep:", " (List.map all ~f:Engine_choice.key) in
  if String.is_empty arg
  then Or_error.errorf "empty engine name: this backend runs %s" keys
  else if String.equal arg "composer"
  then (
    match
      List.filter all ~f:(fun (c : Engine_choice.t) ->
        Engine_id.equal c.engine Composer)
    with
    | [ one ] -> Ok one
    | [] ->
      Or_error.errorf
        "this backend does not run the composer engine (it runs %s): start one \
         with prigh serve -engines composer"
        keys
    | many ->
      Or_error.errorf
        "composer needs an adapter: this backend drives %s"
        (String.concat ~sep:", " (List.map many ~f:Engine_choice.key)))
  else (
    let exact (c : Engine_choice.t) =
      String.equal arg (String.lowercase (Engine_choice.key c))
      || String.equal arg (String.lowercase (Engine_choice.display_name c))
      || List.exists (Engine_choice.names c) ~f:(fun n ->
        String.equal arg (String.lowercase n))
    in
    match List.filter all ~f:exact with
    | c :: _ -> Ok c
    | [] ->
      let prefix (c : Engine_choice.t) =
        String.is_prefix (String.lowercase (Engine_choice.key c)) ~prefix:arg
        || String.is_prefix
             (String.lowercase (Engine_choice.display_name c))
             ~prefix:arg
        || List.exists (Engine_choice.names c) ~f:(fun n ->
          String.is_prefix (String.lowercase n) ~prefix:arg)
      in
      (match List.filter all ~f:prefix with
       | [ c ] -> Ok c
       | _ :: _ as several ->
         Or_error.errorf
           "%S matches several engines (%s): name one of them"
           raw
           (String.concat ~sep:", " (List.map several ~f:Engine_choice.key))
       | [] ->
         Or_error.errorf "no engine %S here: this backend runs %s" raw keys))
;;

let deployed t = String.concat ~sep:"," (engine_strings t)

let choose t ~engine ~adapter =
  let open Or_error.Let_syntax in
  let%bind engine =
    match engine with
    | Some e -> Engine_id.of_string e
    | None ->
      (match adapter with
       | Some _ -> Ok Engine_id.Composer
       | None -> Ok (default_choice t).engine)
  in
  if not (List.mem t.engines engine ~equal:Engine_id.equal)
  then
    Or_error.errorf
      "this backend does not run the %s engine (it runs %s): start one with \
       prigh serve -engines %s"
      (Engine_id.to_string engine)
      (deployed t)
      (Engine_id.to_string engine)
  else (
    match engine with
    | Harness ->
      (match adapter with
       | None -> Ok Engine_choice.harness
       | Some a ->
         Or_error.errorf
           "the harness engine takes no adapter (got %s): use engine composer"
           a)
    | Composer ->
      let%bind adapter =
        match adapter with
        | None -> Ok (List.hd_exn t.adapters)
        | Some a ->
          let%bind a = Adapter_id.of_string a in
          if List.mem t.adapters a ~equal:Adapter_id.equal
          then Ok a
          else
            Or_error.errorf
              "this backend's composer does not drive %s (it drives %s)"
              (Adapter_id.label a)
              (String.concat
                 ~sep:", "
                 (List.map t.adapters ~f:Adapter_id.label))
      in
      Ok { Engine_choice.engine = Composer; adapter = Some adapter })
;;

let unavailable ?buildable t (choice : Engine_choice.t) =
  let buildable = Option.value buildable ~default:(drivable t) in
  match choice.adapter with
  | Some a when not (buildable a) ->
    Some
      (sprintf
         "%s; this session can still be read here"
         (match a with
          | Acp name -> unknown_acp name
          | _ -> sprintf "the composer can't drive %s" (Adapter_id.label a)))
  | _ ->
    if not (List.mem t.engines choice.engine ~equal:Engine_id.equal)
    then
      Some
        (sprintf
           "this session runs on the %s engine, which this backend does not \
            run (it runs %s): open it on a backend started with prigh serve \
            -engines %s; it can still be read here"
           (Engine_choice.label choice)
           (deployed t)
           (Engine_id.to_string choice.engine))
    else (
      match choice.adapter with
      | Some a when not (List.mem t.adapters a ~equal:Adapter_id.equal) ->
        Some
          (sprintf
             "this session drives %s, which this backend's composer does not \
              (it drives %s): add it with -adapters %s"
             (Adapter_id.label a)
             (String.concat ~sep:", " (List.map t.adapters ~f:Adapter_id.label))
             (String.concat
                ~sep:","
                (List.map (t.adapters @ [ a ]) ~f:Adapter_id.to_string)))
      | Some _ | None -> None)
;;
